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

(defun jl/magit--ref-table ()
  "Ref name -> object id for this refresh, or nil when not prefilled."
  (and magit--refresh-cache
       (cdr (assoc (cons default-directory 'jl/ref-table)
                   (cdr magit--refresh-cache)))))

(defun jl/magit--resolve (table name)
  "Object id that short ref NAME resolves to in TABLE, by git's lookup order."
  (seq-some (lambda (fmt) (gethash (format fmt name) table))
            '("%s" "refs/%s" "refs/tags/%s" "refs/heads/%s"
              "refs/remotes/%s" "refs/remotes/%s/HEAD")))

(defun jl/magit--rev-ancestor-from-track (orig a b)
  "Answer `magit-rev-ancestor-p' (ORIG) for HEAD vs. upstream from the prefill.
A and B are the commits; anything else falls through to ORIG."
  (let ((known (and magit--refresh-cache (equal a "HEAD")
                    (cdr (assoc (cons default-directory 'jl/head-vs-upstream)
                                (cdr magit--refresh-cache))))))
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

(defun jl/magit-prefill-status-cache ()
  "Prefill `magit--refresh-cache' with cheap lookups for a status refresh.
In log and revision buffers only the ref table (plus current branch and
its upstream) is built: it answers the per-decoration \"is there a tag
with this branch's name\" checks (~40 per log)."
  (when-let* ((magit--refresh-cache)
              ((zerop (recursion-depth)))
              ((derived-mode-p 'magit-status-mode 'magit-log-mode
                               'magit-revision-mode))
              ;; Once per refresh.
              ((not (assoc (cons default-directory 'jl/ref-table)
                           (cdr magit--refresh-cache))))
              (kind (cond ((derived-mode-p 'magit-status-mode) 'status)
                          ((derived-mode-p 'magit-revision-mode) 'revision)
                          ('log))))
    ;; 0. Untracked files: magit asks `git config --local' (never cached)
    ;;    on every refresh unless `magit-status-show-untracked-files' is
    ;;    buffer-local.  Decide from the config list magit loads anyway.
    ;;    Unlike magit, this also honours a global showUntrackedFiles=no.
    (cond ((not (eq kind 'status)))
          ((local-variable-p 'jl/magit--untracked-set)
           (unless (member (magit-get "status.showUntrackedFiles")
                           '("no" "off" "false" "0"))
             (kill-local-variable 'magit-status-show-untracked-files)
             (kill-local-variable 'jl/magit--untracked-set)))
          ((and (not (local-variable-p 'magit-status-show-untracked-files))
                (member (magit-get "status.showUntrackedFiles")
                        '("no" "off" "false" "0")))
           (setq-local magit-status-show-untracked-files 'no)
           (setq-local jl/magit--untracked-set t)))
    (let ((table (make-hash-table :test #'equal))
          head-branch upstream head-track push-target push-oid)
      ;; 1. All refs, with symref targets and upstreams.
      (dolist (line (jl/magit--git-lines
                     "for-each-ref" "--include-root-refs"
                     "--format=%(refname)%00%(objectname)%00%(symref)%00%(upstream:short)%00%(upstream:track)"))
        (pcase-let ((`(,ref ,oid ,symref ,up ,track) (split-string line "\0")))
          (puthash ref oid table)
          (when (and (equal ref "HEAD") (string-prefix-p "refs/heads/" symref))
            (setq head-branch (substring symref 11)))
          (when (and (string-prefix-p "refs/" ref) (not (equal symref "")))
            (jl/magit--cache-put (list "symbolic-ref" ref) symref))
          (when (and head-branch (equal ref (concat "refs/heads/" head-branch))
                     (not (equal up "")) (not (string-match-p "gone" track)))
            (setq upstream up head-track track))))
      ;; The HEAD row sorts first, so the branch row above saw head-branch.
      ;; If a tag (or other ref) shares the branch's name, git shortens it
      ;; to "heads/NAME" and magit uses that; leave all of that to magit.
      (when (and head-branch
                 (seq-some (lambda (fmt) (gethash (format fmt head-branch) table))
                           '("%s" "refs/%s" "refs/tags/%s")))
        (setq head-branch 'ambiguous upstream nil))
      (when (gethash "HEAD" table)
        (push (cons (cons default-directory 'jl/ref-table) table)
              (cdr magit--refresh-cache))
        (if (eq head-branch 'ambiguous)
            (setq head-branch nil)
          (jl/magit--cache-put '("symbolic-ref" "--short" "HEAD") head-branch))
        ;; The upstream name is looked up by status and revision buffers,
        ;; everything after it by status buffers only.
        (when (eq kind 'log)
          (setq head-branch nil upstream nil))
        (when head-branch
          (jl/magit--cache-put
           (list "rev-parse" "--verify" "--abbrev-ref"
                 (concat head-branch "@{upstream}"))
           upstream))
        (unless (eq kind 'status)
          (setq head-branch nil upstream nil))
        ;; HEAD is an ancestor of its upstream iff it isn't ahead of it.
        (when upstream
          (push (cons (cons default-directory 'jl/head-vs-upstream)
                      (cons upstream (not (string-match-p "ahead" head-track))))
                (cdr magit--refresh-cache)))
        ;; Push target ("Push:" header).  `magit-get' reads the config
        ;; list magit would load anyway, so this costs no extra process.
        (setq push-target
              (and-let* ((head-branch)
                         (remote (or (magit-get "branch" head-branch "pushRemote")
                                     (magit-get "remote.pushDefault"))))
                (concat remote "/" head-branch)))
        (when push-target
          (setq push-oid (jl/magit--resolve table push-target))
          (jl/magit--cache-put (list "rev-parse" "--verify" push-target) push-oid))
        ;; 2. Repository paths and the commits the status buffer looks at.
        ;;    Fails (and is skipped) in repos with fewer than 11 commits.
        (when-let* (((eq kind 'status))
                    (lines (jl/magit--git-lines
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
              (when-let* ((h (gethash head info)))
                (jl/magit--cache-put '("rev-parse" "--short" "HEAD") (car h))
                (jl/magit--cache-put
                 '("log" "--no-walk" "--format=%h %s" "HEAD^{commit}" "--")
                 (concat (car h) " " (cdr h))))
              (when-let* ((h (gethash head1 info)))
                (jl/magit--cache-put '("rev-parse" "--short" "HEAD~") (car h)))
              (when-let* ((h (and up-oid (gethash up-oid info))))
                (jl/magit--cache-put
                 (list "log" "--no-walk" "--format=%s"
                       (concat upstream "^{commit}") "--")
                 (cdr h)))
              (when-let* ((h (and push-oid (gethash push-oid info))))
                (jl/magit--cache-put
                 (list "log" "--no-walk" "--format=%s"
                       (concat push-target "^{commit}") "--")
                 (cdr h))))))))))

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
