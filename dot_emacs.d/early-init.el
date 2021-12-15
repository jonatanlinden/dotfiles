;;; early-init.el --- early bird  -*- no-byte-compile: t; lexical-binding: t -*-

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 128 1024 1024))

(defvar before-init-time (current-time) "Time when init.el was started.")

(message "Starting emacs %s" (current-time-string))

(defun jl/reset-gc-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold (* 16 1024 1024)))

;; reset frequency of garbage collection once emacs has booted
(add-hook 'emacs-startup-hook #'jl/reset-gc-threshold)

(add-hook 'after-init-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


(setq package-enable-at-startup nil)

(defvar *is-mac* (eq system-type 'darwin))
(defvar *is-win* (eq system-type 'windows-nt))




(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Always load newest byte code
(setq load-prefer-newer t)

;; try the following for unicode characters
;; (setq inhibit-compacting-font-caches t)


;; Avoid emacs frame resize after font change for speed
(setq frame-inhibit-implied-resize t)

(cond (*is-win* (add-to-list 'default-frame-alist '(font . "Cascadia Code 10")))
      (*is-mac* (add-to-list 'default-frame-alist '(font . "SF Mono-13"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
