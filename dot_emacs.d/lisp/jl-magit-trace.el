;;; jl-magit-trace.el --- List git processes of one magit refresh -*- lexical-binding: t; -*-

;;; Commentary:

;; M-x jl/magit-trace-refresh in a magit buffer: refreshes it once and
;; writes every git process it started, with wall time, to the buffer
;; *jl-magit-trace*, slowest first.  Tells whether a refresh is dominated
;; by process startup (many short calls: an in-process libgit2 shim would
;; help) or by a few heavy worktree calls (status/diff: it wouldn't).

;;; Code:

(require 'magit)
(require 'profiler)

(defvar jl/magit-trace--log nil)
(defvar jl/magit-trace--inside nil
  "Non-nil while timing a call (`process-file' calls `call-process').")

(defun jl/magit-trace--around (orig program &rest rest)
  "Time ORIG (process-file/call-process) when PROGRAM is git."
  (if (and jl/magit-trace--log (not jl/magit-trace--inside)
           (string-match-p "git\\(\\.exe\\)?\\'" (format "%s" program)))
      (let ((t0 (float-time))
            (jl/magit-trace--inside t))
        (prog1 (apply orig program rest)
          (push (list (* 1000 (- (float-time) t0))
                      (string-join (nthcdr 3 rest) " "))
                jl/magit-trace--log)))
    (apply orig program rest)))

(defun jl/magit-trace-refresh (&optional profile)
  "Refresh the current magit buffer and list its git processes by time.
With prefix arg PROFILE, also run the CPU profiler during the refresh
and show its report (expand the top entries with TAB)."
  (interactive "P")
  (setq jl/magit-trace--log (list 'start))
  (advice-add 'process-file :around #'jl/magit-trace--around)
  (advice-add 'call-process :around #'jl/magit-trace--around)
  (when profile
    (when (profiler-running-p) (profiler-stop))
    (profiler-start 'cpu))
  (let ((t0 (float-time)) (gc0 gcs-done) (gce0 gc-elapsed)
        (bufsize (buffer-size)) total)
    (unwind-protect (magit-refresh-buffer)
      (advice-remove 'process-file #'jl/magit-trace--around)
      (advice-remove 'call-process #'jl/magit-trace--around)
      (setq total (* 1000 (- (float-time) t0)))
      (when profile (profiler-stop)))
    (let* ((calls (sort (butlast jl/magit-trace--log)
                        (lambda (a b) (> (car a) (car b)))))
           (git-ms (apply #'+ (mapcar #'car calls)))
           (gc-ms (* 1000 (- gc-elapsed gce0)))
           (lines (count-lines (point-min) (point-max)))
           (newsize (buffer-size)))
      (setq jl/magit-trace--log nil)
      (with-current-buffer (get-buffer-create "*jl-magit-trace*")
        (erase-buffer)
        (insert (format "refresh %.0f ms, %d git processes %.0f ms, lisp %.0f ms\n"
                        total (length calls) git-ms (- total git-ms)))
        (insert (format "  of which GC %.0f ms (%d collections, gc-cons-threshold %s)\n"
                        gc-ms (- gcs-done gc0) gc-cons-threshold))
        (insert (format "  buffer: %d lines, %d chars (was %d)\n\n"
                        lines newsize bufsize))
        (dolist (c calls)
          (insert (format "%7.0f ms  git %s\n" (car c) (cadr c))))
        (goto-char (point-min)))
      (display-buffer "*jl-magit-trace*")
      (when profile (profiler-report)))))

(provide 'jl-magit-trace)
;;; jl-magit-trace.el ends here
