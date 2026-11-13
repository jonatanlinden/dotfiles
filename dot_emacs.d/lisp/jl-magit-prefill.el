;;; jl-magit-prefill.el --- Fewer git processes per magit refresh -*- lexical-binding: t; -*-

;;; Commentary:

;; A magit status refresh starts ~26 git processes; ~15 of them are tiny
;; lookups (rev-parse --verify, symbolic-ref, upstream name, subjects, short
;; hashes).  Answer those from three combined git calls at the start of
;; the refresh and put the answers into `magit--refresh-cache' under the
;; exact keys magit uses; `magit-rev-verify' on HEAD and full ref names is
;; answered from one `for-each-ref' table.  If magit changes its arguments,
;; the prefilled entries simply go unused: slower again, but never wrong.
;;
;; Log buffers get the ref table too: each branch name in the decorations
;; otherwise costs a `rev-parse --verify refs/tags/NAME' (~40 per log).
;; Commit (revision) buffers build the table lazily, on the first such
;; check, since most commits have no decorations.
;;
;; Also here: `jl/magit-insert-worktrees', a cheaper replacement for the
;; status buffer's worktrees section (1 git call instead of ~40).
;;
;; Tested against magit 4.x on 2026-11-11: identical status, log and
;; revision buffers; D:/ew/ew status 26 -> 13 and log 50 -> 10 git
;; processes.  Loaded from init.el with `use-package' :after magit.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'magit)

(defconst jl/magit--ref-lookup-order
  '("%s" "refs/%s" "refs/tags/%s" "refs/heads/%s"
    "refs/remotes/%s" "refs/remotes/%s/HEAD")
  "Where git looks for a short ref name, in order (gitrevisions(7)).")

(defvar-local jl/magit--untracked-set nil
  "Non-nil if the prefill made `magit-status-show-untracked-files' local.")

(defun jl/magit--git-lines (&rest args)
  "Run git ARGS; return stdout lines, or nil if git failed."
  (with-temp-buffer
    (and (zerop (apply #'magit-git-insert args))
         (split-string (buffer-string) "\n" t))))

(defun jl/magit--cache-put (args value)
  "Store VALUE for git ARGS (run in `default-directory') unless cached."
  (let ((key (cons default-directory args)))
    (unless (assoc key (cdr magit--refresh-cache))
      (cl-incf (cdar magit--refresh-cache))
      (push (cons key value) (cdr magit--refresh-cache)))))

(defun jl/magit--cached (args)
  "Value cached for ARGS in `default-directory', or nil."
  (and magit--refresh-cache
       (cdr (assoc (cons default-directory args) (cdr magit--refresh-cache)))))

(defun jl/magit--ref-table ()
  "Ref name -> object id for this refresh, or nil when not prefilled."
  (jl/magit--cached 'jl/ref-table))

(defun jl/magit--resolve (table name)
  "Object id that short ref NAME resolves to in TABLE, by git's lookup order."
  (seq-some (lambda (fmt) (gethash (format fmt name) table))
            jl/magit--ref-lookup-order))

(defun jl/magit--rev-ancestor-from-track (orig a b)
  "Answer `magit-rev-ancestor-p' (ORIG) for HEAD vs. upstream from the prefill.
A and B are the commits; anything else falls through to ORIG."
  (let ((known (and (equal a "HEAD") (jl/magit--cached 'jl/head-vs-upstream))))
    (if (and known (equal b (car known)))
        (cdr known)
      (funcall orig a b))))

(defun jl/magit--rev-verify-from-table (orig rev)
  "Answer `magit-rev-verify' (ORIG) for \"HEAD\" and full ref names REV.
Uses the ref table; anything else falls through to ORIG."
  (let ((table (and (stringp rev)
                    (or (equal rev "HEAD") (string-prefix-p "refs/" rev))
                    (not (string-match-p "[~^@:{*?\\[ ]" rev))
                    (or (jl/magit--ref-table)
                        ;; Commit buffers: build the table only once a
                        ;; decoration needs it; most commits have none.
                        (and magit--refresh-cache
                             (derived-mode-p 'magit-revision-mode)
                             (progn (jl/magit-prefill-status-cache)
                                    (jl/magit--ref-table)))))))
    (if table
        (gethash rev table)
      (funcall orig rev))))

(defun jl/magit--sync-untracked-setting ()
  "Make `magit-status-show-untracked-files' buffer-local `no' per git config.
Magit otherwise asks `git config --local' (never cached) on every
refresh.  Decided from the config list magit loads anyway.  Unlike
magit, this also honours a global status.showUntrackedFiles=no."
  (let ((off (member (magit-get "status.showUntrackedFiles")
                     '("no" "off" "false" "0"))))
    (cond ((local-variable-p 'jl/magit--untracked-set)
           (unless off
             (kill-local-variable 'magit-status-show-untracked-files)
             (kill-local-variable 'jl/magit--untracked-set)))
          ((and off (not (local-variable-p 'magit-status-show-untracked-files)))
           (setq-local magit-status-show-untracked-files 'no
                       jl/magit--untracked-set t)))))

(defun jl/magit--prefill-status (table branch upstream track)
  "Prefill what only status buffers ask for.
TABLE is the ref table, BRANCH the current branch (nil if detached or
ambiguous), UPSTREAM its upstream and TRACK its %(upstream:track)."
  ;; HEAD is an ancestor of its upstream iff it isn't ahead of it.
  (when upstream
    (jl/magit--cache-put 'jl/head-vs-upstream
                         (cons upstream (not (string-match-p "ahead" track)))))
  ;; Push target ("Push:" header).  `magit-get' reads the config list
  ;; magit would load anyway, so this costs no extra process.
  (let* ((push-target
          (and-let* ((branch)
                     (remote (or (magit-get "branch" branch "pushRemote")
                                 (magit-get "remote.pushDefault"))))
            (concat remote "/" branch)))
         (push-oid (and push-target (jl/magit--resolve table push-target))))
    (when push-target
      (jl/magit--cache-put (list "rev-parse" "--verify" push-target) push-oid))
    ;; 2. Repository paths and the commits the status buffer looks at.
    ;;    Fails (and is skipped) in repos with fewer than 11 commits.
    (when-let* ((lines (jl/magit--git-lines
                        "rev-parse" "--show-toplevel" "--git-dir"
                        "--is-bare-repository" "HEAD" "HEAD~" "HEAD~10")))
      (pcase-let ((`(,top ,gitdir ,bare ,head ,head1 ,head10) lines))
        (jl/magit--cache-put '("rev-parse" "--show-toplevel") top)
        (jl/magit--cache-put '("rev-parse" "--git-dir") gitdir)
        (jl/magit--cache-put '("rev-parse" "--is-bare-repository")
                             (concat bare "\n"))
        (jl/magit--cache-put '("rev-parse" "--verify" "HEAD~10") head10)
        ;; 3. Short hashes and subjects, in one log call.
        (let* ((up-oid (and upstream (jl/magit--resolve table upstream)))
               (info (make-hash-table :test #'equal)))
          (dolist (line (apply #'jl/magit--git-lines
                               "log" "--no-walk=unsorted" "--format=%H%x00%h%x00%s"
                               (delete-dups (delq nil (list head head1 up-oid push-oid)))))
            (pcase-let ((`(,oid ,short ,subject) (split-string line "\0")))
              (puthash oid (cons short subject) info)))
          (pcase-dolist (`(,rev . ,oid) `(("HEAD" . ,head) ("HEAD~" . ,head1)))
            (when-let* ((h (gethash oid info)))
              (jl/magit--cache-put (list "rev-parse" "--short" rev) (car h))))
          (when-let* ((h (gethash head info)))
            (jl/magit--cache-put
             '("log" "--no-walk" "--format=%h %s" "HEAD^{commit}" "--")
             (concat (car h) " " (cdr h))))
          (pcase-dolist (`(,name . ,oid) `((,upstream . ,up-oid)
                                           (,push-target . ,push-oid)))
            (when-let* ((h (and oid (gethash oid info))))
              (jl/magit--cache-put
               (list "log" "--no-walk" "--format=%s" (concat name "^{commit}") "--")
               (cdr h)))))))))

(defun jl/magit-prefill-status-cache ()
  "Prefill `magit--refresh-cache' with cheap lookups for a status refresh.
In log and revision buffers only the ref table (plus current branch and
its upstream) is built: it answers the per-decoration \"is there a tag
with this branch's name\" checks (~40 per log)."
  (when-let* ((magit--refresh-cache)
              ((zerop (recursion-depth)))
              (kind (cond ((derived-mode-p 'magit-status-mode) 'status)
                          ((derived-mode-p 'magit-revision-mode) 'revision)
                          ((derived-mode-p 'magit-log-mode) 'log)))
              ;; Once per refresh (the table may be stored as nil).
              ((not (assoc (cons default-directory 'jl/ref-table)
                           (cdr magit--refresh-cache)))))
    (when (eq kind 'status)
      (jl/magit--sync-untracked-setting))
    (let ((table (make-hash-table :test #'equal))
          head-branch upstream head-track)
      ;; 1. All refs, with symref targets and upstreams.
      (dolist (line (jl/magit--git-lines
                     "for-each-ref" "--include-root-refs"
                     "--format=%(refname)%00%(objectname)%00%(symref)%00%(upstream:short)%00%(upstream:track)"))
        (pcase-let ((`(,ref ,oid ,symref ,up ,track) (split-string line "\0")))
          (puthash ref oid table)
          (when (and (equal ref "HEAD") (string-prefix-p "refs/heads/" symref))
            (setq head-branch (string-remove-prefix "refs/heads/" symref)))
          (when (and (string-prefix-p "refs/" ref) (not (equal symref "")))
            (jl/magit--cache-put (list "symbolic-ref" ref) symref))
          (when (and head-branch (equal ref (concat "refs/heads/" head-branch))
                     (not (equal up "")) (not (string-match-p "gone" track)))
            (setq upstream up head-track track))))
      ;; The HEAD row sorts first, so the branch row above saw head-branch.
      ;; If a tag (or other ref) shares the branch's name, git shortens it
      ;; to "heads/NAME" and magit uses that; leave all of that to magit.
      (let* ((ambiguous
              (and head-branch
                   (seq-some (lambda (fmt) (gethash (format fmt head-branch) table))
                             (seq-take jl/magit--ref-lookup-order 3))))
             (branch (and (not ambiguous) head-branch))
             (upstream (and branch upstream)))
        (unless (gethash "HEAD" table)
          (setq table nil))
        ;; Stored even when nil, so commit buffers don't retry per ref.
        (jl/magit--cache-put 'jl/ref-table table)
        (when table
          (unless ambiguous
            (jl/magit--cache-put '("symbolic-ref" "--short" "HEAD") branch))
          ;; The upstream name is looked up by status and revision buffers.
          (when (and branch (not (eq kind 'log)))
            (jl/magit--cache-put
             (list "rev-parse" "--verify" "--abbrev-ref" (concat branch "@{upstream}"))
             upstream))
          (when (eq kind 'status)
            (jl/magit--prefill-status table branch upstream head-track)))))))

;;; Worktrees section

(defun jl/magit-insert-worktrees ()
  "Like `magit-insert-worktrees', but with one git call, not one per worktree.
Magit runs `git rev-parse' per worktree (to resolve its toplevel) and
per detached worktree (to abbreviate the hash).  With ~25 worktrees and
~80 ms per git process on Windows that costs >1.5 s per refresh.  The
paths from `git worktree list' are already toplevels (no separate
git-dir setups here), hashes can be truncated locally, and the current
branch (asked once per worktree) only needs to be looked up once."
  (let ((abbrev-len (magit-abbrev-length))
        (current-branch (magit-get-current-branch)))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda (&optional dir)
                 (when dir
                   (let ((d (expand-file-name dir)))
                     (and (file-directory-p d)
                          (file-name-as-directory d))))))
              ((symbol-function 'magit-rev-abbrev)
               (lambda (rev)
                 (substring rev 0 (min (length rev) abbrev-len))))
              ((symbol-function 'magit-get-current-branch)
               (lambda () current-branch)))
      (magit-insert-worktrees))))

(add-hook 'magit-status-sections-hook #'jl/magit-insert-worktrees t)

;;; Advice

;; Not `magit-pre-refresh-hook': that doesn't run when a buffer is first
;; created.  These run on creation and on every refresh, inside the cache
;; magit binds for it.
(advice-add 'magit-status-refresh-buffer :before #'jl/magit-prefill-status-cache)
(advice-add 'magit-log-refresh-buffer :before #'jl/magit-prefill-status-cache)
(advice-add 'magit-rev-verify :around #'jl/magit--rev-verify-from-table)
(advice-add 'magit-rev-ancestor-p :around #'jl/magit--rev-ancestor-from-track)

(provide 'jl-magit-prefill)
;;; jl-magit-prefill.el ends here
