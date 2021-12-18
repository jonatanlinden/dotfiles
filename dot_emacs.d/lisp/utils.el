;;; utils.el --- Collection of functions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun jl/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

;; Best of both worlds
(defun kill-region-or-backward-word ()
  "Kill the region if active, otherwise kill the word before point."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (and (boundp 'subword-mode) subword-mode)
        (subword-backward-kill 1)
      (backward-kill-word 1))))

(provide 'utils)
;;; utils.el ends here
