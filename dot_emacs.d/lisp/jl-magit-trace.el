;;; jl-magit-trace.el --- Where one magit refresh spends its time -*- lexical-binding: t; -*-

;;; Commentary:

;; M-x jl/magit-trace-refresh in a magit buffer: refreshes it once and
;; writes a wall-clock breakdown to the buffer *jl-magit-trace*:
;;
;;  - per function (each section inserter and header, the refresh hooks,
;;    the steps magit-refresh-buffer runs around them): total time, own
;;    time outside processes and nested traced functions ("self"), and
;;    time in processes started from it;
;;  - every synchronous process (git or not), slowest first, with the
;;    function it ran in;
;;  - blocking waits (sit-for, sleep-for, accept-process-output).
;;
;; Wall clock, not CPU: on Windows the CPU profiler only samples while
;; Emacs computes, so time spent waiting (on processes, slow file access,
;; timeouts) never shows up there.  With a prefix arg the CPU profiler
;; runs as well.

;;; Code:

(require 'cl-lib)
(require 'magit)
(require 'profiler)

(defvar jl/magit-trace--stack nil
  "Open frames, innermost first: vectors [NAME START CHILD-MS PROC-MS NPROC].")
(defvar jl/magit-trace--frames nil
  "Finished frames: (NAME TOTAL-MS SELF-MS PROC-MS NPROC).")
(defvar jl/magit-trace--procs nil
  "Timed processes: (MS PROGRAM ARGS IN-FUNCTION).")
(defvar jl/magit-trace--inside nil
  "Non-nil while timing a process (`process-file' calls `call-process').")

(defun jl/magit-trace--now () (* 1000 (float-time)))

(defun jl/magit-trace--enter (name)
  (push (vector name (jl/magit-trace--now) 0.0 0.0 0) jl/magit-trace--stack))

(defun jl/magit-trace--leave ()
  (let* ((f (pop jl/magit-trace--stack))
         (total (- (jl/magit-trace--now) (aref f 1))))
    (when jl/magit-trace--stack
      (cl-incf (aref (car jl/magit-trace--stack) 2) total))
    (push (list (aref f 0) total (- total (aref f 2) (aref f 3))
                (aref f 3) (aref f 4))
          jl/magit-trace--frames)))

(defun jl/magit-trace--wrap (name)
  "Around-advice timing a function as frame NAME."
  (lambda (orig &rest args)
    (if (not jl/magit-trace--stack)
        (apply orig args)
      (jl/magit-trace--enter name)
      (unwind-protect (apply orig args)
        (jl/magit-trace--leave)))))

(defun jl/magit-trace--process (orig program &rest rest)
  "Time ORIG (`process-file'/`call-process') running PROGRAM."
  (if (or (not jl/magit-trace--stack) jl/magit-trace--inside)
      (apply orig program rest)
    (let ((t0 (jl/magit-trace--now))
          (jl/magit-trace--inside t))
      (unwind-protect (apply orig program rest)
        (let ((ms (- (jl/magit-trace--now) t0))
              (top (car jl/magit-trace--stack)))
          (cl-incf (aref top 3) ms)
          (cl-incf (aref top 4))
          (push (list ms (file-name-nondirectory (format "%s" program))
                      (jl/magit-trace--args (nthcdr 3 rest))
                      (aref top 0))
                jl/magit-trace--procs))))))

(defun jl/magit-trace--args (args)
  "ARGS as a string, without magit's global git options."
  (let ((a (mapcar (lambda (x) (format "%s" x)) args)))
    (while (pcase (car a)
             ((or "--no-pager" "--literal-pathspecs") (setq a (cdr a)))
             ("-c" (setq a (cddr a)))))
    (string-join a " ")))

(defun jl/magit-trace--targets ()
  "Functions to time in the current buffer: (FUNCTION . LABEL)."
  (let* ((prefix (substring (symbol-name major-mode) 0 -5))
         (hooks (list (intern (format "%s-sections-hook" prefix))
                      (intern (format "%s-headers-hook" prefix))
                      'magit-refresh-buffer-hook))
         (fns (list (intern (format "%s-refresh-buffer" prefix))
                    'magit--refresh-buffer-get-positions
                    'magit--refresh-buffer-set-positions
                    'magit-section-show
                    'magit-section-update-highlight
                    'jl/magit-prefill-status-cache
                    'sit-for 'sleep-for 'accept-process-output
                    'make-process)))
    (dolist (hook hooks)
      (when (boundp hook)
        (dolist (fn (ensure-list (symbol-value hook)))
          (when (and (symbolp fn) (fboundp fn))
            (push fn fns)))))
    (mapcar (lambda (fn) (cons fn (symbol-name fn)))
            (seq-uniq (seq-filter #'fboundp fns)))))

(defun jl/magit-trace-refresh (&optional profile)
  "Refresh the current magit buffer and show where the time went.
With prefix arg PROFILE, also run the CPU profiler during the refresh
and show its report (expand the top entries with TAB)."
  (interactive "P")
  (let ((targets (jl/magit-trace--targets))
        (buffer (current-buffer))
        (gc0 gcs-done) (gce0 gc-elapsed)
        (lines0 (count-lines (point-min) (point-max))))
    (setq jl/magit-trace--frames nil
          jl/magit-trace--procs nil
          jl/magit-trace--stack nil)
    (dolist (target targets)
      (advice-add (car target) :around (jl/magit-trace--wrap (cdr target))
                  '((name . jl/magit-trace))))
    (advice-add 'process-file :around #'jl/magit-trace--process)
    (advice-add 'call-process :around #'jl/magit-trace--process)
    (when profile
      (when (profiler-running-p) (profiler-stop))
      (profiler-start 'cpu))
    (unwind-protect
        (progn
          (jl/magit-trace--enter "(refresh, outside the functions below)")
          (unwind-protect (magit-refresh-buffer)
            (jl/magit-trace--leave)))
      (setq jl/magit-trace--stack nil)
      (dolist (target targets)
        (advice-remove (car target) 'jl/magit-trace))
      (advice-remove 'process-file #'jl/magit-trace--process)
      (advice-remove 'call-process #'jl/magit-trace--process)
      (when profile (profiler-stop)))
    (let* ((root (car jl/magit-trace--frames)) ; finished last
           (total (nth 1 root))
           (proc-ms (apply #'+ (mapcar #'car jl/magit-trace--procs)))
           (gc-ms (* 1000 (- gc-elapsed gce0)))
           ;; Merge frames by name (a function can run more than once).
           (by-name (make-hash-table :test #'equal))
           rows)
      (dolist (f jl/magit-trace--frames)
        (let ((r (or (gethash (car f) by-name)
                     (puthash (car f) (list (car f) 0.0 0.0 0.0 0 0) by-name))))
          (cl-incf (nth 1 r) (nth 1 f))
          (cl-incf (nth 2 r) (nth 2 f))
          (cl-incf (nth 3 r) (nth 3 f))
          (cl-incf (nth 4 r) (nth 4 f))
          (cl-incf (nth 5 r))))
      (maphash (lambda (_ r) (push r rows)) by-name)
      (setq rows (sort rows (lambda (a b) (> (nth 2 a) (nth 2 b)))))
      (with-current-buffer (get-buffer-create "*jl-magit-trace*")
        (erase-buffer)
        (insert (format "refresh %.0f ms: %d processes %.0f ms, rest %.0f ms\n"
                        total (length jl/magit-trace--procs) proc-ms
                        (- total proc-ms)))
        (insert (format "  GC %.0f ms (%d collections, gc-cons-threshold %s)\n"
                        gc-ms (- gcs-done gc0) gc-cons-threshold))
        (insert (format "  buffer: %d lines (was %d)\n\n"
                        (with-current-buffer buffer
                          (count-lines (point-min) (point-max)))
                        lines0))
        (insert "   self    total    procs  n  calls  function (>= 1 ms; self = not in\n"
                "                                     processes or nested functions)\n")
        (dolist (r (seq-filter (lambda (r) (>= (nth 1 r) 1)) rows))
          (insert (format "%7.0f  %7.0f  %7.0f %2d  %5d  %s\n"
                          (nth 2 r) (nth 1 r) (nth 3 r) (nth 4 r) (nth 5 r)
                          (nth 0 r))))
        (insert "\nProcesses, slowest first:\n")
        (dolist (p (sort jl/magit-trace--procs (lambda (a b) (> (car a) (car b)))))
          (insert (format "%7.0f ms  %-8s %s\n          in %s\n"
                          (nth 0 p) (nth 1 p)
                          (truncate-string-to-width (nth 2 p) 110 nil nil "...")
                          (nth 3 p))))
        (goto-char (point-min)))
      (display-buffer "*jl-magit-trace*")
      (when profile (profiler-report)))))

(provide 'jl-magit-trace)
;;; jl-magit-trace.el ends here
