;; -*- lexical-binding: t -*-

;; For inspiration: https://emacs.nasy.moe/
;; https://ladicle.com/post/config
(setq esup-child-profile-require-level 0)
;; List available fonts in *Messages* buffer
;;(message
;; (mapconcat (quote identity)
;;            (sort (font-family-list) #'string-lessp) "\n"))

;(setq debug-on-error t)
;; *is-mac*, *is-win* and *is-linux* are defined in early-init.el


;; disable temporarily
(defvar jl/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; restore it after initialization
(add-hook 'after-init-hook
  (lambda ()
    (setq file-name-handler-alist jl/file-name-handler-alist)))


(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; TRY: check if this prevents freezing during command evaluation
(defun jl/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun jl/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

;;(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;;(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(defconst jonatan-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(defconst jonatan-personal-dir (expand-file-name "personal" user-emacs-directory))

(defconst jonatan-personal-preload (expand-file-name "personal/preload.el" user-emacs-directory))

(pixel-scroll-precision-mode)

(setopt use-short-answers t)
(setopt use-dialog-box nil)

(when (file-exists-p jonatan-personal-preload)
  (load jonatan-personal-preload))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p jonatan-savefile-dir)
  (make-directory jonatan-savefile-dir))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p jonatan-personal-dir)
  (make-directory jonatan-personal-dir))

(setq auth-sources
    '((:source "~/.emacs.d/.authinfo.gpg")))

;; Home directory is default
(setq default-directory "~/")
(setq command-line-default-directory "~/")

(when *is-win*
  (setq w32-pass-lwindow-to-system nil
       w32-pass-rwindow-to-system nil
       w32-lwindow-modifier 'super ;; Left Windows key
       w32-rwindow-modifier 'super ;; Right Windows key
       w32-apps-modifier 'hyper) ;; Menu key
  )

;; use right option as macos option key
(when *is-mac*
  (setq ns-right-alternate-modifier 'none))

(setq delete-by-moving-to-trash t)
(when *is-mac* (setq trash-directory "~/.Trash")) ;; not necessary on windows

(defun jl/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'solarized-light t))
    ('dark (load-theme 'solarized-dark t))))

(when *is-win*
  (set-selection-coding-system 'utf-16-le-dos))

(when *is-mac*
  (add-hook 'ns-system-appearance-change-functions #'jl/apply-theme))


(when *is-win*
  (progn
    (w32-register-hot-key [s-])
    (w32-register-hot-key [s-p])
    (w32-register-hot-key [s-f])
    (w32-register-hot-key [s-F])
    ))



;; Windows explorer to go to the file in the current buffer
;; either use subst-char-in-string to get backslash, or define
;; (defun w32-shell-dos-semantics() t)
(defun jl/open-folder-in-explorer ()
  "Open windows explorer in the current directory and select the current file."
  (interactive)
  (w32-shell-execute
    "open" "explorer"
    (concat "/e,/select," (subst-char-in-string ?/ ?\\ (convert-standard-filename buffer-file-name)))
  )
)

;; warn when opening files bigger than 10MB
(setq large-file-warning-threshold 10000000)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)


;; Time-stamp: <> in the first 8 lines?
(add-hook 'before-save-hook 'time-stamp)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 2)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
;; (delete-selection-mode t)

;; backups and auto-saves go to var/backup/ and var/auto-save/, see
;; `no-littering-theme-backups' in the no-littering block

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode)

;; handle files with long lines
(global-so-long-mode 1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)


;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;;Emacs normally fontifies text even while you’re actively typing.
(setq redisplay-skip-fontification-on-input t)

(repeat-mode 1)
(setq repeat-exit-timeout 5)

;; live visual feedback as you compose a regexp in the minibuffer (Emacs 30+)
(when (fboundp 'minibuffer-regexp-mode)
  (minibuffer-regexp-mode 1))

(setq window-combination-resize t)

(setq read-extended-command-predicate
      #'command-completion-default-include-p)
;; (minibuffer-prompt-properties
;;    '(read-only t cursor-intangible t face minibuffer-prompt))

(setq kill-do-not-save-duplicates t)
;; Save the Clipboard Before Killing
(setq save-interprogram-paste-before-kill t)

;; Don’t Render Cursors in Non-Focused Windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Increase Process Output Buffer for LSP
(setq read-process-output-max (* 4 1024 1024)) ; 4MB

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca-no-symlink-mode)

(setq elpaca-queue-limit 20)

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(setq use-package-verbose t
      use-package-compute-statistics t
      use-package-minimum-reported-time 0.05
      )

(use-package emacs
  :ensure nil
  :custom
  (ibuffer-human-readable-size t)
  (kill-region-dwim 'emacs-word)
  (delete-pair-push-mark t)
  (completion-eager-update t)
  (completion-eager-display 'auto)
  (minibuffer-visible-completions 'up-down)
  :config
  (setq mode-line-collapse-minor-modes '(abbrev-mode
                                         flyspell-mode
                                         flyspell-prog-mode
                                         eldoc-mode
                                         smartparens-mode
                                         )))

(use-package speedbar
  :ensure nil
  :commands (speedbar)
  :hook (eglot-managed-mode . jl/speedbar-for-eglot)
  :init
  (defun jl/speedbar-for-eglot ()
    "Open the speedbar when Eglot starts in a git-controlled directory."
    (when (and (eglot-managed-p)
               (locate-dominating-file default-directory ".git"))
      (save-selected-window (speedbar 1))))
  :custom
  (speedbar-prefer-window t)
  (speedbar-use-images nil)
  (speedbar-window-default-width 25)
  (speedbar-window-max-width 25)
)


(use-package no-littering
  :ensure t
  :init
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  :config
  ;; backups in var/backup/, auto-saves in var/auto-save/
  (no-littering-theme-backups))

(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))


(use-package diminish
  :ensure (:wait t))

(setq set-mark-command-repeat-pop t)

(use-package general
  :ensure (:wait t)
  :config
  (general-define-key
   [remap delete-char] 'delete-forward-char
   ;; Better than default - from /u/zck
   "M-c" 'capitalize-dwim
   "M-l" 'downcase-dwim
   "M-u" 'upcase-dwim
   "C-x O"  (lambda () (interactive) (other-window -1))
   ;; use hippie-expand instead of dabbrev
   "M-/" 'hippie-expand
   "s-/" 'hippie-expand
   ;; align code
   "C-x \\" 'align-regexp
   ;; mark-end-of-sentence is normally unassigned
   "M-h" 'mark-end-of-sentence
   ;; switch between buffers fast
   "<f1>" 'previous-buffer
   "<f2>" 'next-buffer
   [remap comment-box] 'jl/comment-box
   )


  ;;; open current file in explorer/finder
  (when *is-win*
    (general-define-key "M-O" #'jl/open-folder-in-explorer))
  )

(use-package unfill
  :ensure t
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package org :ensure t
  :custom
  (org-table-duration-custom-format 'hours)
  (org-export-backends '(ascii html md))
  (org-directory (if *is-win* "c:/work/notes" "~/org"))
  (org-hide-emphasis-markers t)
  (org-clock-persist t)
  (org-duration-format '(("h" . t) (special . 2)))
  :bind (("C-c c" . org-capture))
  :config
  (org-clock-persistence-insinuate)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  :hook (org-mode . (lambda () (electric-indent-mode -1))))

(use-package denote
  :ensure t
  :custom (denote-file-type 'markdown-yaml)
  :bind
  (("C-c n n" . denote)
   ("C-c n l" . denote-link-or-create)
   ("C-c n r" . denote-rename-file))
  :config

  (denote-rename-buffer-mode 1)
  )




;; load the personal settings
(when (file-exists-p jonatan-personal-dir)
  (message "Loading personal configuration files in %s..." jonatan-personal-dir)
  (mapc 'load (directory-files jonatan-personal-dir 't "^[^#].*el$")))

(use-package utils
  :load-path "lisp"
  :bind (
         (:map emacs-lisp-mode-map
               ([remap eval-last-sexp] . jl/eval-last-sexp-or-region))
         )
  )

;; manage elpa keys
(use-package gnu-elpa-keyring-update
  :disabled t
  :ensure t)

(use-package page-break-lines
  :ensure t)



(use-package server
  :if *is-win*
  :hook (after-init . (lambda ()
                        (unless (server-running-p)
                            (server-mode 1)))))

(with-eval-after-load 'server
  (when (equal window-system 'w32)
    ;; Suppress error "directory  ~/.emacs.d/server is unsafe". It is needed
    ;; needed for the server to start on Windows.
    (defun server-ensure-safe-dir (dir) "Noop" t)))

(use-package project
  :custom (project-switch-commands '((project-find-file "Find file" "f")
                                     (project-find-dir "Find dir" "d")
                                     (project-dired "Dired" "D")
                                     (consult-ripgrep "ripgrep" "g")
                                     (magit-project-status "Magit" "m"))))

(use-package solarized-theme
  ;; Not `:if window-system': under a daemon that is nil at startup,
  ;; even though the frames created later are graphical.
  :ensure t
  :custom
  (solarized-scale-org-headlines nil)
  (solarized-use-variable-pitch nil)
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0)
  :init
  (load-theme 'solarized-light t)
  ;;(set-face-background 'default "#fdfdf0")
  )

(use-package color-theme-sanityinc-tomorrow
  :unless (or window-system (daemonp))
  :ensure t
  :init (load-theme 'sanityinc-tomorrow-night t)
  )

(use-package mood-line
  :ensure t
  :custom
  (mood-line-show-eol-style t)
  (mood-line-show-encoding-information t)
  :hook (after-init . mood-line-mode))

(use-package paren
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; note: in macos, disable shortcuts for mission control to make
;; <c-left> and <c-right> to work correctly
(use-package smartparens
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode) . smartparens-strict-mode)
  :custom
  (sp-base-key-bindings 'paredit)  ; installs the paredit-style bindings
  (sp-autoskip-closing-pair t)
  (sp-hybrid-kill-entire-symbol nil)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (unbind-key "M-?" smartparens-mode-map)
  ;; matching pairs are highlighted by show-paren-mode, not
  ;; show-smartparens-mode
  (defun jl/sp-close-or-skip (&optional arg)
    "Insert a closing delimiter, or jump out of the enclosing list.
Jump past the closer of the innermost enclosing list when the typed
char matches it and the surrounding top-level form is balanced;
otherwise insert the char literally (closing the unbalanced list)."
    (interactive "p")
    (let* ((char last-command-event)
           (ppss (syntax-ppss))
           (inner (nth 1 ppss))
           (outer (car (nth 9 ppss))))
      (if (and inner
               (not (nth 8 ppss))           ; not in string/comment
               (eq char (matching-paren (char-after inner)))
               (ignore-errors (scan-lists outer 1 0)))
          (goto-char (scan-lists inner 1 0))
        (let ((sp-autoskip-closing-pair nil))
          (self-insert-command arg)))))
  (dolist (key '(")" "]" "}"))
    (define-key smartparens-mode-map (kbd key) #'jl/sp-close-or-skip))
  ;; :diminish (smartparens-mode .  "()")
  :diminish smartparens-mode)

(use-package abbrev
  :custom (save-abbrevs 'silently)
  :init (setq-default abbrev-mode t)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


(use-package uniquify
  :custom
  (uniquify-separator "/")
  (uniquify-buffer-name-style 'forward)
  ;; Renaming after killing a uniquified buffer is the default; the
  ;; variable was renamed in Emacs 31, so it is not set here.
  ;; don't muck with special buffers
  (uniquify-ignore-buffers-re "^\\*"))

(use-package savehist
  :init (savehist-mode)
  :config
  ;; add-to-list, not setq: other packages (e.g. corfu-history) add
  ;; their own variables here
  (dolist (var '(search-ring regexp-search-ring))
    (add-to-list 'savehist-additional-variables var)))

(use-package recentf
  :custom (recentf-max-saved-items 300)
  (recentf-max-menu-items 15)
  ;; disable recentf-cleanup on Emacs start, because it can cause
  ;; problems with remote files
  (recentf-auto-cleanup 'never)
  (recentf-exclude '(".*-autoloads\\.el\\'" "COMMIT_EDITMSG\\'"))
  :config
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory))
  :hook (after-init . recentf-mode))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ;;("C-c n" . crux-cleanup-buffer-or-region)
         ;;("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c R" . crux-rename-buffer-and-file)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ([remap open-line] . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)))


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :init (setq hl-paren-highlight-adjacent t)
  :hook ((after-init . global-highlight-parentheses-mode)))

(use-package anzu
  :ensure t
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp)
  ("H-q" . anzu-query-replace-at-cursor-thing)
  :init
  (setq anzu-cons-mode-line-p nil)
  :hook
  (after-init . global-anzu-mode))

(use-package avy
  :ensure t
  :custom
  (avy-style 'de-bruijn)
  (avy-background t)
  :bind (("M--" . avy-goto-char-timer)
         ("M-g l" . avy-goto-line)
         )
  :config
  (avy-setup-default)
  ;;(global-set-key (kbd "C-c C-j") 'avy-resume)
  ;; :chords (("jj" . avy-goto-line)
  ;;          ("jk" . avy-goto-word)
  )

(use-package avy-zap
  :ensure t
  :bind
  ("M-z" . avy-zap-up-to-char-dwim)
  ("M-Z" . avy-zap-to-char-dwim))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package vertico
  :ensure t
  :hook
  (after-init . vertico-mode)
)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :config (marginalia-mode))

(use-package consult
  :ensure t
  :bind
  (("M-y" . consult-yank-from-kill-ring)
   ("C-x b" . consult-buffer)
   ("C-s" . consult-line)
   ("C-c r" . consult-ripgrep)
   ([remap goto-line] . consult-goto-line)
   )
  :config
  (consult-customize consult-buffer
                     :preview-key "M-.")
  (consult-customize consult-line
                   consult-ripgrep
                   :initial (when (use-region-p)
                              (buffer-substring-no-properties
                               (region-beginning) (region-end))))
  )

;;(setq completion-ignore-case t)
;;(setq read-file-name-completion-ignore-case t)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package corfu
  :ensure t
  :custom
  ;; Make the popup appear quicker
  (corfu-popupinfo-delay '(0.5 . 0.5))
  ;; Always have the same width
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  ;; Have Corfu wrap around when going up
  (corfu-cycle t)
  (corfu-preselect 'first)
  :bind (:map corfu-map
              ;; pick a candidate by its avy-style hint key
              ("s-j" . corfu-quick-complete))
  :init
  ;; Enable Corfu
  (global-corfu-mode t)
  ;; Enable Corfu history mode to act like `prescient'
  (corfu-history-mode t)
  ;; Allow Corfu to show help text next to suggested completion
  (corfu-popupinfo-mode t))

(use-package cape
  :ensure t
  :demand t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (setq-default completion-at-point-functions
                (append (default-value 'completion-at-point-functions)
                        (list #'cape-dabbrev #'cape-file #'cape-abbrev))))

(use-package completion-preview
  :if (>= emacs-major-version 30)
  :ensure nil
  :bind (:map completion-preview-active-mode-map
              ("M-f" . #'completion-preview-insert-word)
              ("C-M-f" . #'completion-preview-insert-sexp))
  :custom
  (completion-preview-minimum-symbol-length 2)
  :init
  (global-completion-preview-mode))

(use-package ace-window
  :ensure t
  :bind
  (("s-w" . ace-window)
   ;([remap other-window] . ace-window))
   ))


;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :diminish
  :ensure t
  :hook
  (after-init . volatile-highlights-mode)
  )

(use-package dired
  :commands dired-mode
  :custom
  ;; always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-create-destination-dirs t)
  (dired-dwim-target t))
;; https://github.com/emacsmirror/dired-plus
(use-package dired+
  :disabled t
  :ensure (dired-plus :type git :host github :repo "emacsmirror/dired-plus")
  :after dired
  :bind
  (:map dired-mode-map
        ([remap yank] . diredp-yank-files)))

(put 'dired-find-alternate-file 'disabled nil)

(defun jl/prog-mode-hook ()
  ;; causes projectile to choke?
  ;; (make-local-variable 'company-backends)
  ;; (push 'company-keywords company-backends)
  ;; show trailing whitespace in editor
  ;; (dumb-jump-mode)
  (setq show-trailing-whitespace t)
  ;;(setq show-tabs)
  )


(use-package expreg
  :ensure t
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(defvar expreg-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "=" #'expreg-expand)
    (define-key map "-" #'expreg-contract)
    map))

(put 'expreg-expand 'repeat-map 'expreg-repeat-map)
(put 'expreg-contract 'repeat-map 'expreg-repeat-map)

(use-package change-inner
  :ensure t
  :bind
  ("M-i" . change-inner)
  ;("M-o" . change-outer)
  :config
  (require 'cl-lib)
  (defun jl/delete-region-instead-of-kill-region (orig-fn &rest args)
    "Call ORIG-FN with `kill-region' temporarily acting as `delete-region'."
    (cl-letf (((symbol-function 'kill-region)
               (lambda (beg end &optional _region) (delete-region beg end))))
      (apply orig-fn args)))
  (advice-add 'change-inner* :around #'jl/delete-region-instead-of-kill-region))

(use-package symbol-overlay
  :ensure t
  :diminish symbol-overlay-mode
  :custom
  (symbol-overlay-idle-time 1.5)
  :bind
  ("M-n" . symbol-overlay-jump-next)
  ("M-p" . symbol-overlay-jump-prev)
  :hook
  (prog-mode . symbol-overlay-mode))


;; show available keybindings after you start typing
;; which-key is built in from Emacs 30; install it only on older versions.
(if (< emacs-major-version 30)
    (use-package which-key
      :ensure t
      :config (which-key-mode))
  (which-key-mode))

(use-package discover-my-major
  :ensure t
  :commands (discover-my-major discover-my-mode)
  )

(use-package vundo
  :ensure t
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-roll-back-on-quit nil)
  :bind (("C-z" . undo)
         ("C-x u" . vundo)))

(use-package find-file-in-project
  :ensure t
  :custom (ffip-use-rust-fd t)
  :bind (("s-f" . find-file-in-project)
         ("s-F". find-file-in-current-directory)
         ("M-s-f" . find-file-in-project-by-selected)))


(use-package flycheck
  :ensure t
  :custom
  (flycheck-checker-error-threshold 1605)
  (flycheck-check-syntax-automatically '(save))
  (flycheck-mode-line-prefix "FC")
  :preface
  (defun jl/flycheck-off-in-eglot ()
    "Leave diagnostics to eglot's flymake while eglot manages the buffer."
    (if (eglot-managed-p)
        (flycheck-mode -1)
      (when (derived-mode-p 'prog-mode)
        (flycheck-mode 1))))
  :config
  (add-to-list 'flycheck-disabled-checkers 'ruby-reek)
  :hook
  (prog-mode . flycheck-mode)
  (eglot-managed-mode . jl/flycheck-off-in-eglot))

;; (use-package casual-symbol-overlay
;;   :ensure t
;;   :bind (:map symbol-overlay-mode-map ("C-i" . 'casual-symbol-overlay-tmenu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LANGUAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package flycheck-clang-tidy
  :ensure t
  :if (executable-find "clang-tidy")
  :custom (flycheck-clang-tidy-build-path "../../build")
  :after (flycheck)
  :hook ((c++-mode c++-ts-mode) . flycheck-clang-tidy-setup)
  )

;; (use-package envrc
;;   :ensure t
;;   :hook (after-init . envrc-global-mode)
;; )


(use-package pyvenv
  :ensure t)

(use-package reformatter
  :ensure t
  :config
  (reformatter-define dd/ruff-format
    :program "uvx"
    :args `("ruff" "format" "--stdin-filename" ,buffer-file-name "-"))
  (reformatter-define dd/ruff-sort
    :program "uvx"
    :args `("ruff" "check" "--select" "I" "--fix" "--stdin-filename" ,buffer-file-name "-")))

(defun dd/python-init ()
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (venv-path (when project-root
                      (expand-file-name ".venv" project-root))))
    (when (and venv-path (file-directory-p venv-path))
      (make-local-variable 'pyvenv-virtual-env)
      (pyvenv-activate venv-path))
    (dd/ruff-format-on-save-mode +1)
    (dd/ruff-sort-on-save-mode +1)))

(add-hook 'python-base-mode-hook #'dd/python-init)

(use-package groovy-mode
  :ensure t
  :custom (groovy-indent-offset 2)
  :mode ("Jenkinsfile" "\\.groovy\\'" )
  )

(use-package inf-ruby
  :after ruby-mode
  :ensure t
  :config
  ; (add-to-list 'inf-ruby-implementations '("ruby" . "irb --prompt default --noreadline -r irb/completion"))
  (setq inf-ruby-default-implementation "ruby")
  :hook ((ruby-mode ruby-ts-mode) . inf-ruby-minor-mode))

;; Tree-sitter modes.  Grammars are found in ~/.emacs.d/tree-sitter/
;; (always searched) and var/treesit/ (added by no-littering).
;;
;; Emacs 31+: built in, new grammars go to var/treesit/.  Plain `ask' is
;; set on purpose: no-littering would otherwise switch it to `ask-dir',
;; which also prompts for a directory.  Every -ts- mode replaces its classic mode, and
;; the first time a file needs a missing grammar, Emacs asks whether to
;; download and compile it (requires git and a C compiler).  Declining
;; leaves that buffer without a working mode; to keep a language on its
;; classic mode, change `t' to a list of the -ts- modes you want.
;;
;; Emacs 29/30: treesit-auto, which only switches when the grammar is
;; already installed.  Note that it takes about 2.4 s at startup.
(if (>= emacs-major-version 31)
    (setopt treesit-auto-install-grammar 'ask
            treesit-enabled-modes t)
  (use-package treesit-auto
    :ensure t
    :config
    (treesit-auto-add-to-auto-mode-alist)
    (global-treesit-auto-mode)))

(use-package treesit-fold
  :ensure (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold"))

(use-package eglot
  :custom (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  :config (add-to-list 'eglot-server-programs
                      `(python-base-mode
                        . ,(eglot-alternatives '(("basedpyright-langserver" "--stdio")
                                                 ("ty" "server")
                                                 ("pyright-langserver" "--stdio")))))
 :hook
 ;; Hook both the classic and the -ts- mode: a -ts- mode never runs the
 ;; classic mode's hook.  Python has a shared parent in Emacs 29+.
 ((typescript-ts-mode tsx-ts-mode) . eglot-ensure)
 ((js-mode js-ts-mode) . eglot-ensure)
 (json-ts-mode . eglot-ensure)
 ((yaml-mode yaml-ts-mode) . eglot-ensure)
 (dockerfile-ts-mode . eglot-ensure)
 ((css-mode css-ts-mode) . eglot-ensure)
 (python-base-mode . eglot-ensure)
 (html-mode . eglot-ensure))

(use-package breadcrumb
  :ensure t
  :commands (breadcrumb-mode breadcrumb-local-mode)
  :hook (eglot-managed-mode . jl/breadcrumb-follow-eglot)
  :init
  (defun jl/breadcrumb-follow-eglot ()
    "Enable `breadcrumb-local-mode' while Eglot manages the buffer."
    (breadcrumb-local-mode (if (eglot-managed-p) 1 -1)))
  :config
  (setq which-func-functions #'(breadcrumb-imenu-crumbs))
)

(use-package ruby-mode
  :ensure nil
  :mode ("Rakefile" "\\.rb\\'")
  :custom (ruby-align-chained-calls t)
  :config
  (use-package smartparens-ruby)
  :hook
  ((ruby-mode ruby-ts-mode) . subword-mode)
  ((ruby-mode ruby-ts-mode) . eglot-ensure)
  :interpreter "ruby"
  :bind (:map ruby-mode-map ("C-c C-e" . ruby-send-region))
  )

(with-eval-after-load 'ruby-ts-mode
  (keymap-set ruby-ts-mode-map "C-c C-e" #'ruby-send-region))

(use-package yard-mode
  :ensure t
  :diminish yard-mode
  :after ruby-mode
  :hook (ruby-mode ruby-ts-mode))

(use-package ruby-tools
  :ensure t
  :commands ruby-tools-mode
  :hook ((ruby-mode ruby-ts-mode) . ruby-tools-mode)
  :diminish ruby-tools-mode)


;; Markdown: gfm-mode (from markdown-mode) on Emacs 29/30, the built-in
;; markdown-ts-mode on Emacs 31.  markdown-mode stays installed on 31
;; too, since eglot renders hover documentation with `gfm-view-mode'.
(use-package markdown-mode
  :ensure t
  :custom (markdown-fontify-code-block-natively t)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)))

;; Emacs 31: markdown-ts-mode is built in but not enabled by default (it
;; is still marked experimental), and `treesit-enabled-modes' doesn't
;; cover it.  Without the markdown grammars, Emacs asks to install them
;; and otherwise falls back to text-mode with a warning.
(when (>= emacs-major-version 31)
  (use-package markdown-ts-mode
    :ensure nil
    :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
    :init
    ;; markdown-mode's autoloads (loaded by elpaca after init) put their
    ;; own .md/.mdx entry ahead of the ones above, and the markdown-mode
    ;; block maps .md to gfm-mode.  Remap both so they still end up here.
    (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
    (add-to-list 'major-mode-remap-alist '(gfm-mode . markdown-ts-mode))
    :config
    (require 'markdown-ts-mode-x)))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package css-mode
  :mode ("\\.css\\'" "\\.scss\\'" "\\.sass\\'")
  :custom (css-indent-offset 2)
  )

(use-package com-css-sort
  :ensure t
  :after css-mode
  :custom (com-css-sort-sort-type 'alphabetic-sort)
  :bind (:map css-mode-map
              ("C-c s" . com-css-sort-attributes-block)))

(defun jl/web-mode-hook ()
  "Hooks for Web mode."
  (progn
    (setq web-mode-markup-indent-offset 2)
    (web-mode-edit-element-minor-mode))
  )

(use-package web-mode
  :ensure t
  :custom (web-mode-css-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-tag-auto-close-style 2)
  (setq web-mode-enable-auto-quoting t)
  (use-package web-mode-edit-element :ensure t)
  :hook (web-mode . jl/web-mode-hook))

;; TODO: Evaluate, use local binaries from, e.g., yarn, npm
(use-package find-local-executable
  :ensure nil
  :disabled t
  )

(use-package jsonian
  :ensure t
  :after so-long
  :config
  (jsonian-no-so-long-mode)
  :hook (flycheck-mode . jsonian-enable-flycheck)
  )

;; Show changes in fringe
(use-package diff-hl
  :ensure t
  :bind (("C-c g n" . diff-hl-next-hunk)
         ("C-c g p" . diff-hl-previous-hunk))
  :config (unless (display-graphic-p) (diff-hl-margin-mode))
  ;; Highlight changes to the current file in the fringe
  ;; Highlight changed files in the fringe of Dired
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package hl-todo
  :ensure t
  :hook ((after-init . global-hl-todo-mode)))

(use-package whitespace
  :ensure nil
  :commands (whitespace-mode)
  :init (setq whitespace-line-column 80))
;  :hook (asm-mode . whitespace-mode)

;; ANSI colours in compilation buffers
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; --annotate is not needed with the MI interpreter
(when (executable-find "gdb-multiarch")
  (setq gud-gdb-command-name "gdb-multiarch -i=mi"))

(use-package asm-mode
  ;; Not `:custom (tab-width 8)': that changes the global default.
  :hook (asm-mode . (lambda () (setq-local tab-width 8)))
  ;:config (setq asm-comment-char ?#)
)

;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
;;(local-unset-key (vector asm-comment-char))
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
;;  (setq tab-always-indent (default-value 'tab-always-indent)
  ;;      comment-column 40
    ;;    tab-stop-list (number-sequence 8 64 8)
      ;;  ))

(use-package string-inflection
  :ensure t
  )

(use-package arm-mode
  :ensure (arm-mode :type git :host github :repo "charJe/arm-mode"
                      :fork (:host github
                                   :repo "jonatanlinden/arm-mode"))
  ;;:load-path "lisp/arm-mode"
  :mode ("\\.i\\'" "\\.s\\'")
  :config (setq asm-comment-char ?\;)
  ;; Re-add once lisp/xref-posframe exists again:
  ;; :bind (:map arm-mode-map
  ;;             ("M-." . xref-posframe-dwim)
  ;;             ("M-," . xref-posframe-pop))
  )

(defun jl/c-mode-common-hook ()
  (require 'smartparens-c)
  (setq-local fill-column 79)
  (setq-local display-fill-column-indicator-column 79)
  (display-fill-column-indicator-mode)
  )

(use-package hideif
  :diminish hide-ifdef-mode
  :custom (hide-ifdef-shadow 't)
  :hook ((c-mode-common c-ts-base-mode) . hide-ifdef-mode)
  )

(add-hook 'c-mode-common-hook #'jl/c-mode-common-hook)
(add-hook 'c-ts-base-mode-hook #'jl/c-mode-common-hook)

;; FIX prevent bug in smartparens
;; (setq sp-escape-quotes-after-insert nil)

(use-package cc-mode
  :ensure nil
  :defer t
  :bind* (:map c-mode-base-map
               ("C-c C-o" . ff-find-other-file))
  :config
  (setq c-default-style "k&r"
        c-basic-offset 2)
  (keymap-set c++-mode-map "<remap> <kill-sexp>" #'sp-kill-hybrid-sexp))

(use-package sqlite-mode
  :ensure nil
  :mode "\\.db\\'"
  )

;; c-ts-mode.el defines both c-ts-mode and c++-ts-mode; there is no
;; separate c++-ts-mode feature.
(use-package c-ts-mode
  :ensure nil
  :defer t
  :custom
  ;; match the cc-mode style below
  (c-ts-mode-indent-style 'k&r)
  (c-ts-mode-indent-offset 2)
  :hook
  ((c++-mode c++-ts-mode) . jl/c++-mode-hook)
  ((c++-mode c++-ts-mode) . eglot-ensure)
  :config
  (dolist (map (list c-ts-mode-map c++-ts-mode-map))
    (keymap-set map "C-c C-o" #'ff-find-other-file))
  (keymap-set c++-ts-mode-map "<remap> <kill-sexp>" #'sp-kill-hybrid-sexp)
  (keymap-set c++-ts-mode-map "C-c f" #'clang-format-region))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))


(use-package mark-thing-at
  :ensure t
  :hook (after-init . mark-thing-at-mode))


(use-package reveal-in-osx-finder
  :ensure t
  :if *is-mac*
  :bind ("M-o" . reveal-in-osx-finder))

(defun jl/c++-mode-hook ()
  "FIX prevent bug in smartparens."
  (progn
    (setq sp-escape-quotes-after-insert nil)
    (make-local-variable 'sp-escape-quotes-after-insert)))


(use-package clang-format
  :ensure t
  :custom
  (clang-format-style "file")
  (clang-format-fallback-style "Google")
  :after cc-mode
  :commands (clang-format-buffer clang-format-defun)
  :bind* (:map c++-mode-map
               ("C-c f" . clang-format-region)))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :config (setq ibuffer-saved-filter-groups
                '(("Default"
                   ("Dired" (mode . dired-mode))
                   ("Org" (or (mode . org-mode)
                              (filename . "OrgMode")))
                   ("Subversion" (name . "^\\*svn"))
                   ("Magit" (or (name . "^\\*Magit")
                                (name . "^magit")))
                   ("Help" (or (name . "^\\*Help\\*")
                               (name . "^\\*Apropos\\*")
                               (name . "^\\*info\\*")))
                   ("Emacs" (or
                             (name . "^\\*dashboard\\*$"  )
                             (name . "^\\*scratch\\*$"    )
                             (name . "^\\*Messages\\*$"   )
                             (name . "^\\*Backtrace\\*$"  )
                             (name . "^\\*Compile-Log\\*$")
                             (name . "^\\*Flycheck"       )
                             ))
                   ))

              ibuffer-show-empty-filter-groups nil
              ibuffer-default-sorting-mode 'filename/process
              )
  :hook ((ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "Default"))))
  )

(use-package ibuffer-vc
  :ensure t
  )

;; show the cursor when moving after big movements in the window
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :hook (after-init . beacon-mode))

(use-package move-text
  :ensure t
  :bind
  (("M-<up>"    . move-text-up)
   ("M-<down>"  . move-text-down)))

;; edit grep buffers, e.g. from embark-export of consult-ripgrep
(use-package wgrep
  :ensure t
  )

(use-package ws-butler
  :ensure t
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  :diminish ""
  :config
  (ws-butler-global-mode)
)

;; smartparens-strict-mode and rainbow-delimiters are enabled for elisp
;; by the smartparens and rainbow-delimiters blocks.
(use-package lisp-mode
  :ensure nil
  :diminish "L"
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-c" . eval-defun)
        ("C-c C-b" . eval-buffer)))

(use-package request
  :ensure t
  :defer t
  :custom
  (request-curl (if *is-win*  "c:/ProgramData/chocolatey/bin/curl.exe" "curl"))
  (request-message-level 'warn)
  (request-log-level 'warn)
  )

(use-package eldoc
  :diminish eldoc-mode)

;;(use-package yasnippet-snippets
;;  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-minor-mode)
  :hook
  (prog-mode . yas-minor-mode)
  ;; (yas-minor-mode . (lambda ()
  ;;                     (add-to-list
  ;;                      'yas-snippet-dirs
  ;;                      (concat user-emacs-directory "snippets"))))
  :config (yas-reload-all))


(use-package arm-lookup
  :ensure (arm-lookup :type git :host github :repo "jonatanlinden/arm-lookup")
  :after arm-mode
  :custom
  ;; SumatraPDF only exists on Windows; elsewhere use the package
  ;; default, which tries the available viewers in turn
  (arm-lookup-browse-pdf-function (if *is-win*
                                      #'arm-lookup-browse-pdf-sumatrapdf
                                    #'arm-lookup-browse-pdf-any))
  :bind (:map arm-mode-map ("C-M-." . arm-lookup))
  )


(use-package open-in-msvs
  :if *is-win*
  :load-path "lisp/open-in-msvs"
  :commands (open-in-msvs)
  :bind ("M-o" . open-in-msvs)
  )


;;(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
;;  (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))

;; (org-todo-keywords
;;    '((sequence "TODO(t)" "IN PROGRESS(p)" | "DONE(d!)")))


(use-package org-mru-clock
  :after org
  :ensure t
  :custom (org-mru-clock-how-many 10)
  :bind* (("<f8>" . org-mru-clock-in))
  :commands (org-mru-clock-in org-mru-clock-select-recent-task))

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(use-package ox-pandoc
  :disabled t
  :ensure t
  :after ox
  )

(setopt ffap-machine-p-known 'reject)

(use-package alert
  :if window-system
  :ensure t
  :commands (alert)
  :custom (alert-default-style 'mode-line)
  )

(use-package cmake-mode
  :ensure (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :mode "CMakeLists.txt")

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'")
  )

(use-package rust-mode
  :ensure t
  :hook ((rust-mode rust-ts-mode) . eglot-ensure))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))

(use-package flycheck-rust
  :after rust-mode
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))


(use-package ninja-mode
  :mode ("\\.ninja\\'")
  :ensure t)


(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this-symbol)
         ("C-<" . mc/mark-previous-like-this-symbol)
         ("M-C->" . mc/mark-next-like-this-word))
  )

(defun jl/magit-log-edit-mode-hook ()
  (setq fill-column 72)
  (turn-on-auto-fill))

(use-package transient
  :ensure t
  :config (transient-bind-q-to-quit))

(use-package magit
  :after transient
  :ensure t
  :custom (magit-section-initial-visibility-alist '((stashes . hide) (untracked . hide)))
  :config
          (defun jl/magit-display-buffer (buffer)
            "Show Magit status in the same window when called from Dired."
            (if (and (derived-mode-p 'dired-mode)
                     (with-current-buffer buffer
                       (derived-mode-p 'magit-status-mode)))
                (display-buffer buffer '(display-buffer-same-window))
              (magit-display-buffer-traditional buffer)))
          (setq magit-display-buffer-function #'jl/magit-display-buffer)
          (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
          (add-hook 'magit-status-sections-hook #'magit-insert-worktrees t)
  :bind (("C-x g" . magit-status)
         ("C-c g l" . magit-list-repositories)
         )
)


; (use-package forge
;   :ensure t
;   :after magit
;  )

(use-package magit-delta
  :disabled t
  :ensure t
  :hook (magit-mode . magit-delta-mode))

;; git-messenger: popup commit message at current line
;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
  :ensure t
  :bind
  (("C-c g m" . git-messenger:popup-message)
   :map git-messenger-map
   ([(return)] . git-messenger:popup-close))
  :config
  ;; Enable magit-show-commit instead of pop-to-buffer
  (setq git-messenger:show-detail t))

(defun clang-format-defun ()
  "Run clang-format on the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (clang-format (region-beginning) (region-end))))



(use-package esup
  :ensure t
  :init
  (setq esup-depth 0)
  :commands esup
  )

(when (eq window-system 'w32)
  (setq tramp-default-method "plink")
  )

(winner-mode 1)

(defvar ediff-last-windows nil
  "Last ediff window configuration.")

(defun store-pre-ediff-winconfig ()
  (setq ediff-last-windows (current-window-configuration)))

(defun restore-pre-ediff-winconfig ()
  (set-window-configuration ediff-last-windows))

(use-package ediff
  :custom
  (ediff-diff-options "-w")
  (ediff-ignore-similar-regions t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :hook
  (ediff-before-setup . store-pre-ediff-winconfig)
  (ediff-quit . restore-pre-ediff-winconfig)
  (ediff-startup . (lambda () (ws-butler-mode -1)))
  )

(use-package dumb-jump
  :ensure t
  :init
  ;; Use dumb-jump as an xref backend (M-.), as a fallback after
  ;; buffer-local backends such as eglot's.
  (with-eval-after-load 'xref
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
  :config
  (add-to-list 'dumb-jump-language-file-exts '(:language "c++" :ext "tg" :agtype "cc" :rgtype "c")))

(defun jl/comment-box (b e)
"Draw a box comment around the region but arrange for the region
 to extend to at least the fill column. Place the point after the
 comment box."

(interactive "r")

(let ((e (copy-marker e t))
      (fill-column (- fill-column 6))
      )
  (goto-char b)
  (end-of-line)
  (insert-char ?  (- fill-column (current-column)))
  (comment-box b e 1)
  (goto-char e)
  (set-marker e nil)))





;;; in bat mode, treat _ as a word constitutent
(add-hook 'bat-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; super+arrows: C-<left>/C-<right> belong to smartparens slurp/barf
(windmove-default-keybindings 'super)
;; numbered window shortcuts
(use-package winum
  :ensure t
  :config
  (winum-mode))

;; xref-posframe and xref-asm are disabled because lisp/xref-posframe
;; and lisp/xref-asm no longer exist.  Restore them, then remove `:disabled'.
(use-package posframe
  :disabled t
  :after xref-posframe
  :ensure t)

(use-package xref-posframe
  :disabled t
  :after xref-asm
  :load-path "lisp/xref-posframe")

(use-package xref-asm
  :disabled t
  :load-path "lisp/xref-asm"
  :after arm-mode
  :config
  (xref-asm-activate))

(use-package cheatsheet
  :ensure (:wait t)
  :commands (cheatsheet-show)
  )

(use-package powershell
  :ensure t
  :mode ("\\.ps1\\'")
  :custom (powershell-location-of-exe (executable-find "pwsh"))
  )

(use-package nxml-mode
  :mode ("\\.xml\\'"))

(use-package point-history
  :disabled t
  :ensure (point-history :type git :host github :repo "blue0513/point-history")
  :hook after-init
  :bind (("C-c C-/" . point-history-show))
  :init (setq point-history-ignore-buffer "^ \\*Minibuf\\|^ \\*point-history-show*"))

;; (use-package jl-ocaml
;;   :load-path "lisp"
;;   )

(use-package go-mode
  :ensure t
  :bind (
         ;; If you want to switch existing go-mode bindings to use lsp-mode/gopls instead
         ;; uncomment the following lines
         ;; ("C-c C-j" . lsp-find-definition)
         ;; ("C-c C-d" . lsp-describe-thing-at-point)
         )
  :hook (((go-mode go-ts-mode) . eglot-ensure)))

(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'")
  :hook (csv-mode . csv-guess-set-separator))

;; Built-in js-mode (js-ts-mode on Emacs 31) instead of js2-mode, so
;; eglot and tree-sitter apply to .js files.
(use-package js
  :ensure nil
  :defer t
  :custom (js-indent-level 2))


(defun command-line-diff (switch)
  (let ((file0 (pop command-line-args-left))
        (file1 (pop command-line-args-left)))
    (ediff file0 file1)))

(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

(defun command-line-diff3 (switch)
  (let ((file0 (pop command-line-args-left))
        (file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff3 file0 file1 file2)))

(add-to-list 'command-switch-alist '("-diff3" . command-line-diff3))

(cheatsheet-add
 :group 'General
 :key "C-u C-SPC"
 :description "Move to previous mark")

(cheatsheet-add
 :group 'Consult
 :key "M-n"
 :description "In consult-line/consult-ripgrep: insert symbol at point (future history)")

(cheatsheet-add
 :group 'Embark
 :key "C-. E"
 :description "Export candidates to a buffer (e.g. consult-ripgrep -> grep buffer)")

(cheatsheet-add
 :group 'Embark
 :key "C-c C-p"
 :description "In an exported grep buffer: edit the matches with wgrep, C-c C-c to save")

(cheatsheet-add
 :group 'Embark
 :key "C-. i"
 :description "Insert the current candidate into the buffer")

(cheatsheet-add
 :group 'General
 :key "C-u 3 M-x mc/insert-numbers"
 :description "Insert 3 at the first cursor, 4 at the second curser, etc."
 )

(cheatsheet-add
 :group 'Ruby
 :key "C-c {"
 :description "Ruby toggle block type"
 )

(cheatsheet-add
 :group 'VC
 :key "a"
 :description "Previous revision to line"
 )

(cheatsheet-add
 :group 'VC
 :key "l"
 :description "Show commit info")

(cheatsheet-add
 :group 'Movement
 :key "C-x v ]"
 :description "diff-hl-next-chunk: Move to next modified hunk")

(cheatsheet-add
 :group 'Movement
 :key "C-M-n"
 :description "inside brackets, move to after closing bracket")

(cheatsheet-add
 :group 'Movement
 :key "C-M-u"
 :description "inside brackets, move to opening bracket (up in structure)")

(cheatsheet-add
 :group 'Dired
 :key "C-0 w"
 :description "copy absolute path of file under point")

(cheatsheet-add
 :group 'Dired
 :key "C-x C-j"
 :description "If the buffer visits a file, this command will
 move point to that file's line in the Dired buffer it shows")

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
