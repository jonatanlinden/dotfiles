;; -*- lexical-binding: t -*-

;; For inspiration: https://emacs.nasy.moe/
;; https://ladicle.com/post/config
(setq esup-child-profile-require-level 0)
;; List available fonts in *Messages* buffer
;;(message
;; (mapconcat (quote identity)
;;            (sort (font-family-list) #'string-lessp) "\n"))

(setq debug-on-error t)
(defvar *is-mac* (eq system-type 'darwin))
(defvar *is-win* (eq system-type 'windows-nt))


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

(when (> emacs-major-version 28)
 (pixel-scroll-precision-mode))

(when (file-exists-p jonatan-personal-preload)
  (load jonatan-personal-preload))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p jonatan-savefile-dir)
  (make-directory jonatan-savefile-dir))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p jonatan-personal-dir)
  (make-directory jonatan-personal-dir))

(setenv "GPG_AGENT_INFO" nil)
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

(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

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

;; enable y/n answers
(setq-default use-short-answers t)

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

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; handle files with long lines
(global-so-long-mode 1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(setq process-coding-system-alist
  (cons '("ruby-ls" utf-8 . utf-8) process-coding-system-alist))


;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; development version of emacs, set build date
(setq elpaca-core-date '(20250424))

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
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

(setq elpaca-queue-limit 20)

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(setq use-package-verbose t
      ;; use-package-compute-statistics t
      use-package-minimum-reported-time 0.05
      )

(use-package diminish
  :ensure (:wait t))

(use-package general
  :ensure (:wait t)
  :config
  (general-define-key
   [remap delete-char] 'delete-forward-char
  ;; Better than default - from /u/zck
   "M-c" 'capitalize-dwim
   "M-l" 'downcase-dwim
   "M-u" 'upcase-dwim
   "C-x O"  'other-window-prev
   ;; use hippie-expand instead of dabbrev
   "M-/" 'hippie-expand
   "s-/" 'hippie-expand
   ;; align code
  "C-x \\" 'align-regexp
   ;; mark-end-of-sentence is normally unassigned
   "M-h" 'mark-end-of-sentence
   ;; rebind to zap-up-to-char instead of zap-to-char
   "M-z" 'zap-up-to-char
   ;; switch between buffers fast
   "<f1>" 'previous-buffer
   "<f2>" 'next-buffer
   [remap comment-box] 'jl/comment-box
   )

  (general-def prog-mode-map
   "s-f" 'mark-defun
   )


  ;;; open current file in explorer/finder
  (when *is-win*
    (general-define-key "M-O" #'jl/open-folder-in-explorer))
  )



(use-package org :ensure t
  :custom
  (org-table-duration-custom-format 'hours)
  (org-export-backends '(ascii html md))
  (org-directory "c:/work/notes")
  (org-hide-emphasis-markers t)
  (org-clock-persist t)
  (org-duration-format (quote h:mm))
  :bind (("C-c c" . org-capture))
  :config
  (org-clock-persistence-insinuate)
  (setq org-duration-format '(("h" . t) (special . 2)))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  :hook org-mode . (lambda () (electric-indent-mode -1))
  )

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


(use-package no-littering
  :ensure t
  :init
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        custom-file (no-littering-expand-etc-file-name "custom.el")))

(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))


;; load the personal settings
(when (file-exists-p jonatan-personal-dir)
  (message "Loading personal configuration files in %s..." jonatan-personal-dir)
  (mapc 'load (directory-files jonatan-personal-dir 't "^[^#].*el$")))

(use-package utils
  :load-path "lisp"
  :commands (kill-region-or-backward-word)
  :bind (("C-w" . kill-region-or-backward-word)
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
  :init
  (server-mode 1)
  :hook (after-init . (lambda ()
                        (require 'server)
                        (unless (server-running-p)
                          (server-start)))))

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
  :if window-system
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
  :unless window-system
  :ensure t
  :init (load-theme 'sanityinc-tomorrow-night)
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
  (sp-base-key-bindings 'paredit)
  (sp-autoskip-closing-pair 'always)
  (sp-hybrid-kill-entire-symbol nil)
  (sp-show-pair-delay 0)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (sp-use-paredit-bindings)
  (unbind-key "M-?" smartparens-mode-map)
  (show-smartparens-global-mode +1)
  ;; :diminish (smartparens-mode .  "()")
  :diminish smartparens-mode)

(use-package abbrev
  :diminish ""
  :custom (save-abbrevs 'silently)
  :init (setq abbrev-mode t)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


(use-package uniquify
  :custom
  (uniquify-separator "/")
  (uniquify-buffer-name-style 'forward)
  ;; rename after killing uniquified
  (uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (uniquify-ignore-buffers-re "^\\*"))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        )
  (savehist-mode +1))

(use-package recentf
  :custom (recentf-max-saved-items 300)
  (recentf-max-menu-items 15)
  ;; disable recentf-cleanup on Emacs start, because it can cause
  ;; problems with remote files
  (recentf-auto-cleanup 'never)
  (recentf-exclude '(".*-autoloads\\.el\\'" "COMMIT_EDITMSG\\'"))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
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
         ("C-o" . crux-smart-open-line-above)
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
  :diminish ""
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
         ([remap goto-line] . avy-goto-line)
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
  ("M-z" . avy-zap-to-char-dwim)
  ("M-z" . avy-zap-up-to-char-dwim))

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
                 (window-parameters (mode-line-format . none))))

    )

  ;; Consult users will also want the embark-consult package.
  (use-package embark-consult
    :ensure t
    :after (embark consult)
    :demand t ; only necessary if you have the hook below
    ;; if you want to have consult previews as you move around an
    ;; auto-updating embark collect buffer
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))


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
  :ensure (dired-plus :type git :host github repo: "emacsmirror/dired-plus")
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

(use-package company
  :ensure t
  :diminish (company-mode . "(c)")
  :commands company-mode
  :custom (company-minimum-prefix-length 3)
  (company-global-modes '(not text-mode))
  (company-dabbrev-downcase nil)
  (company-echo-delay 0) ; remove annoying blinking
    ;; set default company-backends
  ;; (company-backends
  ;;       '((company-files
  ;;          company-capf
  ;;          company-yasnippet) company-dabbrev company-dabbrev-code))
  :config
  (setq lsp-completion-provider :capf)
  :bind
  (:map company-active-map
        ("C-e" . company-other-backend)
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort))
  :hook ((after-init . global-company-mode)
         (prog-mode . jl/prog-mode-hook))
  )

(use-package company-prescient
  :after (prescient company)
  :ensure t
  :hook (company-mode . company-prescient-mode))


;(use-package expand-region
;  :bind* ("C-," . er/expand-region))
(use-package expand-region
  :ensure t
  :bind
  ("M-2" . er/expand-region)
  ("s-0" . er/expand-region)
  ("C-," . er/mark-word)
  )


(use-package change-inner
  :ensure t
  :bind
  ("M-i" . change-inner)
  ;("M-o" . change-outer)
  :config
  (advice-add 'change-inner* :around #'delete-region-instead-of-kill-region))

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
(which-key-mode)

(use-package discover-my-major
  :ensure t
  :commands (discover-my-major discover-my-mode)
  )

(use-package vundo
  :ensure t
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-roll-back-on-quit nil)
  :bind (("C-z" . 'undo)
         )
  )

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
  :config
  (add-to-list 'flycheck-disabled-checkers 'ruby-reek)
  :hook (prog-mode . flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LANGUAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-headerline-breadcrumb-enable t)
  (lsp-prefer-flymake nil)
  ;;(lsp-auto-configure nil)
  :commands lsp
  :hook ((ruby-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         ))

;; lsp-ui: This contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses.
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :disabled t
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  :after (lsp)
  :bind  (:map lsp-ui-mode-map
               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
               ([remap xref-find-references] . lsp-ui-peek-find-references)
               )
  :config
  (lsp-ui-sideline-mode)
  )

(use-package flycheck-clang-tidy
  :ensure t
  :if (executable-find "clang-tidy")
  :custom (flycheck-clang-tidy-build-path "../../build")
  :after (flycheck)
  :hook (c++-mode . flycheck-clang-tidy-setup)
  )

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
  :hook (ruby-mode . inf-ruby-minor-mode))

(defun treesit-install-all-grammars () (interactive)
       (dolist (lang treesit-language-source-alist)
         (unless (treesit-language-available-p (car lang))
           (treesit-install-language-grammar (car lang)))))

(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((javascript . ("git@github.com:tree-sitter/tree-sitter-javascript.git"))
          (json . ("git@github.com:tree-sitter/tree-sitter-json.git"))
          (ruby . ("git@github.com:tree-sitter/tree-sitter-ruby.git"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  :config
  (treesit-install-all-grammars)
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (css-mode . css-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (javascript-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (html-mode . html-ts-mode))))

(use-package eglot
  :hook
  (typescript-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  (ruby-ts-mode . lsp)
  (json-ts-mode . eglot-ensure)
  (yaml-ts-mode . eglot-ensure)
  (dockerfile-ts-mode . eglot-ensure)
  (css-mode . eglot-ensure)
  (html-mode . eglot-ensure))

(use-package ruby-mode
  :ensure nil
  :mode ("Rakefile" "\\.rb\\'")
  :custom (ruby-align-chained-calls t)
  :config
  (use-package smartparens-ruby)
  :hook
  (ruby-mode . subword-mode)
  (ruby-mode . lsp)
  :interpreter "ruby"
  :bind
  (([(meta down)] . forward-sexp)
   ([(meta up)]   . backward-sexp)
   (("C-c C-e"    . ruby-send-region)))
  )

(use-package yard-mode
  :ensure t
  :diminish yard-mode
  :after ruby-mode
  :hook ruby-mode)

(use-package ruby-tools
  :ensure t
  :commands ruby-tools-mode
  :hook (ruby-mode . ruby-tools-mode)
  :diminish ruby-tools-mode)

(defun gfm-markdown-filter (buffer)
  "Configure a default layout for rendering markdown files."
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(use-package impatient-mode
  :ensure t
  :custom (imp-set-user-filter gfm-markdown-filter)
  :commands impatient-mode
  )

(use-package simple-httpd
  :ensure t
  :custom
  (httpd-port 7070)
;; (setq httpd-host (system-name))
  )



(defun my-markdown-live-preview ()
"Live preview markdown."
(interactive)
(unless (process-status "httpd")
(httpd-start))
(impatient-mode)
(imp-set-user-filter 'my-markdown-filter
(imp-visit-buffer)
)
)

(use-package markdown-mode
  :ensure t
  :custom (markdown-fontify-code-block-natively t)
  (markdown-command "pandoc -t html5 -f gfm --embed-resources --standalone --mathjax --quiet --highlight-style=pygments --template github.html")
  (markdown-live-preview-engine 'pandoc)
  (markdown-header-scaling t)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :hook (my-markdown-live-preview)
  )

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
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
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
  :custom
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

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Support for ansi colors in comint buffers."
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq gud-gdb-command-name "gdb-multiarch -i=mi --annotate=1")

(use-package asm-mode
  :custom (tab-width 8)
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
  :bind (:map arm-mode-map
              ("M-." . xref-posframe-dwim)
              ("M-," . xref-posframe-pop)))

(defun jl/c-mode-common-hook ()
  (require 'smartparens-c)
  (setq-default fill-column 79)
  (setq-default display-fill-column-indicator-column 79)
  (display-fill-column-indicator-mode)
  )

(use-package hideif
  :diminish hide-ifdef-mode
  :custom (hide-ifdef-shadow 't)
  :hook (c-mode-common . hide-ifdef-mode)
  )

(add-hook 'c-mode-common-hook #'jl/c-mode-common-hook)

;; FIX prevent bug in smartparens
;; (setq sp-escape-quotes-after-insert nil)

(use-package cc-mode
  :ensure nil
  :defer t
  :bind* (:map c-mode-base-map
               ("C-c C-o" . ff-find-other-file))
  :config
  (setq c-default-style "k&r"
        c-basic-offset 2))

(use-package sqlite-mode
  :ensure nil
  :mode "\\.db\\'"
  )

(use-package c++-mode
  :ensure nil
  :after smartparens
  :bind
  ([remap kill-sexp] . sp-kill-hybrid-sexp)
  (:map c++-mode-map
        ("C-c C-o" . ff-find-other-file))
  :hook
  (c++-mode . jl/c++-mode-hook)
  (c++-mode . lsp)
  )

(use-package modern-cpp-font-lock
  :after c++-mode
  :ensure t)


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

;; edit grep-buffers, e.g., ivy-occur
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

(defun jl/el-mode-hook ()
  (message "el-mode-hook")
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  )

(use-package lisp-mode
  :ensure nil
  :diminish "L"
:bind
  (:map emacs-lisp-mode-map
        ("C-c C-c" . eval-defun)
        ("C-c C-b" . eval-buffer))
  :hook  ((emacs-lisp-mode . jl/el-mode-hook)
          ;;((eval-expression-minibuffer-setup lisp-interaction-mode emacs-lisp-mode) . eldoc-mode)
          )
  )

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
  (arm-lookup-browse-pdf-function 'arm-lookup-browse-pdf-sumatrapdf)
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
  :commands (org-mru-clock-in org-mru-clock-select-recent-task)
  :config
  (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook)
  )

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(use-package ox-pandoc
  :disabled t
  :ensure t
  :after ox
  )

(csetq ffap-machine-p-known 'reject)

(use-package mediawiki
  :ensure t
  :commands (mediawiki-mode)
  :init
  ;; workaround, unable to run mediawiki-open otherwise
   (setq url-user-agent "EMACS")
  :hook (mediawiki-mode . visual-line-mode)
  )

(use-package alert
  :if window-system
  :ensure t
  :commands (alert)
  :custom (alert-default-style 'mode-line)
  )

(use-package cmake-mode
  :ensure (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :config
  (make-local-variable 'company-backends)
  (push 'company-cmake company-backends)
  :mode "CMakeLists.txt")

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'")
  )

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))

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

(use-package transient :ensure t)

(use-package magit
  :after transient
  :ensure t
  :custom (magit-section-initial-visibility-alist '((stashes . hide) (untracked . hide)))
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

;; Transient commands: replaces the old magit-popup
(use-package transient :defer t
  :config (transient-bind-q-to-quit))

;; git-messenger: popup commit message at current line
;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
  :ensure t
  :custom (git-messenger:use-magit-popup t)
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
  (ediff-startup . (lambda () (ws-butler-mode nil)))
  )

(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-selector 'ivy)
  :config
  (add-to-list 'dumb-jump-language-file-exts '(:language "c++" :ext "tg" :agtype "cc" :rgtype "c"))
  ;;(require 'xref)
  ;;(setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))
  ;;(add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
  ;; (add-to-list'xref-backend-functions #'dumb-jump-xref-activate)
  :hook (prog-mode . (lambda () (add-to-list'xref-backend-functions #'dumb-jump-xref-activate)))
  :hook (prog-mode . dumb-jump-mode)
  )

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

(windmove-default-keybindings 'control)
;; numbered window shortcuts
(use-package winum
  :ensure t
  :defer t
  :config
  (winum-mode))

(use-package posframe
  :after xref-posframe
  :ensure t)

(use-package xref-posframe
  :after xref-asm
  :load-path "lisp/xref-posframe")

(use-package xref-asm
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
  :mode ("\\.xml\\'")
  :config (setq show-smartparens-mode -1))

(use-package point-history
  :disabled t
  :ensure (point-history :type git :host github :repo "blue0513/point-history")
  :hook after-init
  :bind (("C-c C-/" . point-history-show))
  :init (setq point-history-ignore-buffer "^ \\*Minibuf\\|^ \\*point-history-show*"))

(use-package jl-ocaml
  :load-path "lisp"
  )

(use-package go-mode
  :ensure t
  :bind (
         ;; If you want to switch existing go-mode bindings to use lsp-mode/gopls instead
         ;; uncomment the following lines
         ;; ("C-c C-j" . lsp-find-definition)
         ;; ("C-c C-d" . lsp-describe-thing-at-point)
         )
  :hook ((go-mode . lsp-deferred)))

(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'")
  :hook (csv-mode . csv-guess-set-separator))

(use-package js2-mode
  :ensure t
  :mode("\\.js\\'")
  :custom (js2-basic-offset 2)
  )


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
 :group 'Ivy-occur
 :key "C-o"
 :description "Open file at location from an ivy-occur buffer")

(cheatsheet-add
 :group 'Swiper
 :key "M-j"
 :description "Insert word-at-point into the minibuffer. Extend by pressing multiple times")

(cheatsheet-add
 :group 'Swiper
 :key "M-n"
 :description "Insert symbol-at-point into the minibuffer")

(cheatsheet-add
 :group 'Swiper
 :key "M-q"
 :description "Query replace")


(cheatsheet-add
 :group 'Swiper
 :key "M-o i"
 :description "Insert current ivy/swiper/counsel match into the current buffer"
 )

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
