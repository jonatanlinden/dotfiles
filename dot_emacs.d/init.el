;; -*- lexical-binding: t -*-

;; For inspiration: https://emacs.nasy.moe/
;; https://ladicle.com/post/config
(setq esup-child-profile-require-level 0)
;; List available fonts in *Messages* buffer
;;(message
;; (mapconcat (quote identity)
;;            (sort (font-family-list) #'string-lessp) "\n"))

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

;; Avoid eager loading of packages dependent on ...
(setq initial-major-mode 'fundamental-mode)

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
  ((pixel-scroll-precision-mode)))

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

(when *is-mac*
  (add-hook 'ns-system-appearance-change-functions #'jl/apply-theme))


(when *is-win*
  (progn
    (w32-register-hot-key [s-])
    (w32-register-hot-key [s-p])
    (w32-register-hot-key [s-f])
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

;; disable startup screen
(setq inhibit-startup-screen t)

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

;; show trailing whitespace in editor
;;(setq-default show-trailing-whitespace t)

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

(setq process-coding-system-alist
  (cons '("ruby-ls" utf-8 . utf-8) process-coding-system-alist))


;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; It seems this setting has to be before bootstrapping straight, to avoid
;; a "malformed cache"
(setq straight-use-symlinks t
      ;; No other configuration should be necessary to make this work;
      ;; however, you may wish to call straight-prune-build occasionally,
      ;; since otherwise this cache file may grow quite large over time.
      straight-cache-autoloads t)
;; (setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
   (unless (file-exists-p bootstrap-file)
     (with-current-buffer
         (url-retrieve-synchronously
          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
          'silent 'inhibit-cookies)
       (goto-char (point-max))
       (eval-print-last-sexp)))
   (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq use-package-verbose t
      ;; use-package-compute-statistics t
      use-package-minimum-reported-time 0.05
      )

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :straight t)

(use-package bind-key
  :straight t)

;; The command ‘delete-forward-char’ is preferable for interactive
;; use, e.g.  because it respects values of ‘delete-active-region’ and
;; ‘overwrite-mode’.
(bind-key [remap delete-char] 'delete-forward-char)
;; Better than default - from /u/zck
(bind-key "M-c" 'capitalize-dwim)
(bind-key "M-l" 'downcase-dwim)
(bind-key "M-u" 'upcase-dwim)

(use-package no-littering
  :straight t
  :init
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        custom-file (no-littering-expand-etc-file-name "custom.el")))

(load custom-file t)

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
  :straight t)

(use-package page-break-lines
  :straight t)

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


(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :straight t
  ;; make it faster (assuming all envs in .zshenv)
  :custom (exec-path-from-shell-arguments '("-l" "-d"))
  :config
  (exec-path-from-shell-copy-envs '("LC_ALL" "PYTHONPATH"))
  (exec-path-from-shell-initialize))

(use-package solarized-theme
  :if window-system
  :straight t
  :custom
  (solarized-scale-org-headlines nil)
  (solarized-use-variable-pitch nil)
  (solarized-height-minus-1 1)
  (solarized-height-plus-1 1)
  (solarized-height-plus-2 1)
  (solarized-height-plus-3 1)
  (solarized-height-plus-4 1)
  :init
  (load-theme 'solarized-light t)
  ;;(set-face-background 'default "#fdfdf0")
  )

(use-package color-theme-sanityinc-tomorrow
  :unless window-system
  :straight t
  :init (load-theme 'sanityinc-tomorrow-night)
  )

(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-icon nil)
  (setq doom-modeline-height 18)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-mode 1))

(use-package which-func
  :config
  ;; Show the current function name in the header line, not in mode-line
  (setq-default header-line-format '(which-func-mode ("" which-func-format " ")))
  (setq mode-line-misc-info
        (assq-delete-all 'which-function-mode mode-line-misc-info))
  :hook (prog-mode . which-function-mode))

(use-package paren
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; note: in macos, disable shortcuts for mission control to make
;; <c-left> and <c-right> to work correctly
(use-package smartparens
  :straight t
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
  :straight t
  :bind (("C-c o" . crux-open-with)
         ("C-c n" . crux-cleanup-buffer-or-region)
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
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :straight t
  :diminish highlight-parentheses-mode
  :init (setq hl-paren-highlight-adjacent t)
  :hook ((after-init . global-highlight-parentheses-mode)))

(use-package anzu
  :straight t
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
  :straight t
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
  :straight t
  :bind
  ("M-z" . avy-zap-to-char-dwim)
  ("M-z" . avy-zap-up-to-char-dwim))

(use-package amx
  :straight t
  :bind (("<remap> <execute-extended-command>" . amx)))


;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package ivy
  :straight t
  :diminish

  :custom
  (ivy-extra-directories nil)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  :hook
  (after-init . ivy-mode)
  (ivy-mode . counsel-mode)
  :bind
  ("s-b" . ivy-switch-buffer)
  ("H-b" . ivy-switch-buffer)
  ("C-c C-r" . 'ivy-resume)
  (:map ivy-switch-buffer-map
        ("H-k" . ivy-switch-buffer-kill)))

(use-package ace-window
  :straight t
  :bind
  (("s-w" . ace-window)
   ;([remap other-window] . ace-window))
   ))

(use-package swiper
  :straight t
  :custom
  (swiper-action-recenter t))

(use-package counsel
  :straight t
  :diminish counsel-mode ivy-mode
  :custom
  (counsel-find-file-at-point t)
  (counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never %s %s")
  (counsel-grep-swiper-limit 30000)
  (counsel-async-command-delay 0.5)
  :config
  (if *is-win*
      (setq counsel-git-log-cmd "set GIT_PAGER=cat && git log --grep \"%s\""))
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ;;("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c r" . counsel-rg)
   ("C-x l" . counsel-locate)
   ("s-r" . counsel-recentf)
   ([remap isearch-forward]  . swiper-isearch)
   ([remap isearch-backward] . counsel-grep-or-swiper)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)
   :map counsel-find-file-map
   ("C-w" . counsel-up-directory)))


(use-package ivy-prescient
  :after counsel
  :straight t
  :hook (after-init . ivy-prescient-mode))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :diminish
  :straight t
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

(use-package dired-plus
  :straight t
  :after dired
  :bind
  (:map dired-mode-map
        ([remap yank] . diredp-yank-files)))

(put 'dired-find-alternate-file 'disabled nil)

(use-package company
  :straight t
  ;; :diminish (company-mode . "(c)")
  :diminish company-mode
  :commands company-mode
  :custom (company-minimum-prefix-length 3)
  (company-global-modes '(not text-mode))
  (company-idle-delay 0.2) ; decrease delay before autocompletion popup shows
  (company-dabbrev-downcase nil)
  (company-echo-delay 0) ; remove annoying blinking
    ;; set default company-backends
  (company-backends
        '((company-files
           company-capf
           company-yasnippet) company-dabbrev company-dabbrev-code))
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
  :straight t
  :hook (company-mode . company-prescient-mode))

(defun jl/prog-mode-hook ()
  ;; causes projectile to choke?
  ;; (make-local-variable 'company-backends)
  ;; (push 'company-keywords company-backends)
  ;; show trailing whitespace in editor
  ;; (dumb-jump-mode)
  (setq show-trailing-whitespace t)
  ;;(setq show-tabs)
  )


;(use-package expand-region
;  :bind* ("C-," . er/expand-region))
(use-package expand-region
  :straight t
  :bind
  ("M-2" . er/expand-region)
  ("H-§" . er/expand-region)
  ("s-0" . er/expand-region)
  ("C-," . er/mark-word)
  :config
  (unbind-key "M-@" global-map)
  )


(use-package change-inner
  :straight t
  :bind
  ("M-i" . change-inner)
  ;("M-o" . change-outer)
  :config
  (advice-add 'change-inner* :around #'delete-region-instead-of-kill-region))



(use-package symbol-overlay
  :straight t
  :diminish symbol-overlay-mode
  :custom
  (symbol-overlay-idle-time 1.5)
  :bind
  ("M-n" . symbol-overlay-jump-next)
  ("M-p" . symbol-overlay-jump-prev)
  :hook
  (prog-mode . symbol-overlay-mode))


;; show available keybindings after you start typing
(use-package which-key
  :straight t
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  )

(use-package discover-my-major
  :straight t
  :commands (discover-my-major discover-my-mode)
  )

(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :custom
  (undo-tree-enable-undo-in-region t)
  ;; do not autosave the undo-tree history
  (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist
   `((".*" . ,temporary-file-directory)))
  :bind
  (("C-z" . 'undo)
   ("C-S-z" . 'undo-tree-redo))
  :hook (after-init . global-undo-tree-mode))

(use-package projectile
  :straight t
  :disabled t
  :custom
  (projectile-mode-line-prefix " P")
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)
  (projectile-svn-command "find . -type f -not -iwholename '*.svn/*' -print0")
  ;; on windows,
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("s-p r" . projectile-ripgrep))
  :init (projectile-mode +1)
  )

(use-package counsel-projectile
  :straight t
  :after (projectile counsel)
  :hook
  (after-init . counsel-projectile-mode))


(use-package find-file-in-project
  :straight t
  :custom (ffip-use-rust-fd t)
  :bind (("s-f" . find-file-in-project)
         ("s-F". find-file-in-current-directory)
         ("M-s-f" . find-file-in-project-by-selected)))

(use-package counsel-fd
  :straight t
  :after counsel
  :commands (counsel-fd-dired-jump counsel-fd-file-jump))

(use-package flycheck
  :straight t
  :custom
  (flycheck-checker-error-threshold 1605)
  (flycheck-check-syntax-automatically '(save))
  (flycheck-mode-line-prefix "FC")
  :config
  (add-to-list 'flycheck-disabled-checkers 'ruby-reek)
  :hook (prog-mode . flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LANGUAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :straight t
  :custom
  (lsp-headerline-breadcrumb-enable nil)
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
  :straight t
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
  :straight t
  :if (executable-find "clang-tidy")
  :custom (flycheck-clang-tidy-build-path "../../build")
  :after (flycheck)
  :hook (c++-mode . flycheck-clang-tidy-setup)
  )

(use-package groovy-mode
  :straight t
  :custom (groovy-indent-offset 2)
  :mode ("Jenkinsfile" "\\.groovy\\'" )
  )

(use-package inf-ruby
  :after ruby-mode
  :straight t
  :config
  ; (add-to-list 'inf-ruby-implementations '("ruby" . "irb --prompt default --noreadline -r irb/completion"))
  (setq inf-ruby-default-implementation "ruby")
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package ruby-mode
  :straight t
  :mode ("Rakefile" "\\.rb\\'")
  :custom (ruby-align-chained-calls t)
  :config
  (use-package smartparens-ruby)
  (which-func-mode -1)
  :hook
  (ruby-mode . subword-mode)
  (ruby-mode . lsp)
  :interpreter "ruby"
  :bind
  (([(meta down)] . ruby-forward-sexp)
   ([(meta up)]   . ruby-backward-sexp)
   (("C-c C-e"    . ruby-send-region)))
  )

(use-package yard-mode
  :straight t
  :diminish yard-mode
  :after ruby-mode
  :hook ruby-mode)

(use-package ruby-tools
  :straight t
  :commands ruby-tools-mode
  :hook (ruby-mode . ruby-tools-mode)
  :diminish ruby-tools-mode)


(use-package markdown-mode
  :straight t
  :custom (markdown-fontify-code-block-natively t)
    :mode (("\\.md\\'" . gfm-mode)
           ("\\.markdown\\'" . gfm-mode)))

(use-package yaml-mode
  :straight t
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package css-mode
  :mode ("\\.css\\'" "\\.scss\\'" "\\.sass\\'")
  :custom (css-indent-offset 2)
  )

(use-package com-css-sort
  :straight t
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
  :straight t
  :custom (web-mode-css-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-tag-auto-close-style 2)
  (setq web-mode-enable-auto-quoting t)
  (use-package web-mode-edit-element :straight t)
  :hook (web-mode . jl/web-mode-hook))

;; TODO: Evaluate, use local binaries from, e.g., yarn, npm
(use-package find-local-executable
  :disabled t
  )



(use-package jsonian
  :straight t
  :mode ("\\.json\\'" "\\.tmpl\\'" "\\.eslintrc\\'")
  :after so-long
  :custom
  (jsonian-no-so-long-mode))

;; Show changes in fringe
(use-package diff-hl
  :straight t
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
  :straight t
  :hook ((after-init . global-hl-todo-mode)))

(use-package whitespace
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
  ;;:config (setq asm-comment-char ?#)
)

;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
;;(local-unset-key (vector asm-comment-char))
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
;;  (setq tab-always-indent (default-value 'tab-always-indent)
  ;;      comment-column 40
    ;;    tab-stop-list (number-sequence 8 64 8)
      ;;  ))

(use-package string-inflection
  :straight t
  )

(use-package arm-mode
  :straight (arm-mode :type git :host github :repo "charJe/arm-mode"
                      :fork (:host github
                                   :repo "jonatanlinden/arm-mode"))
  ;;:load-path "lisp/arm-mode"
  :mode ("\\.i\\'" "\\.s\\'")
  :bind (:map arm-mode-map
              ("M-." . xref-posframe-dwim)
              ("M-," . xref-posframe-pop)))

(defun jl/c-mode-common-hook ()
  (require 'smartparens-c)
  )

(use-package hideif
  :diminish hide-ifdef-mode
  :custom (hide-ifdef-shadow 't)
  :hook (c-mode-common . hide-ifdef-mode)
  )

(add-hook 'c-mode-common-hook #'jl/c-mode-common-hook)

;; FIX prevent bug in smartparens
;; (setq sp-escape-quotes-after-insert nil)

(use-package irony
  :straight t
  :commands irony-mode
  :bind ((:map irony-mode-map
      ([remap completion-at-point] . counsel-irony))
         )
  :config
  (unless (or *is-win* (irony--find-server-executable))
    (call-interactively #'irony-install-server))
  (setq w32-pipe-read-delay 0)
  :hook ((irony-mode . irony-cdb-autosetup-compile-options)
         (c++-mode . irony-mode))
  )

(use-package company-irony-c-headers
  :after (irony company)
  :straight t
  )

(use-package company-irony
  :after (irony company)
  :straight t
  :hook (irony-mode . (lambda ()
                        (add-to-list (make-local-variable 'company-backends) '(company-irony-c-headers company-irony)))))



(use-package irony-eldoc
  :after irony
  :disabled t
  :straight t
  :hook irony-mode)

(use-package cc-mode
  :defer t
  :bind* (:map c-mode-base-map
               ("C-c C-o" . ff-find-other-file))
  :config
  (setq c-default-style "k&r"
        c-basic-offset 2)
  )

(use-package sqlite-mode
  :mode "\\.db\\'"
  )

(use-package c++-mode
  :after smartparens
  :bind
  ([remap kill-sexp] . sp-kill-hybrid-sexp)
  (:map c++-mode-map
        ("C-c C-o" . ff-find-other-file))
  :hook (c++-mode . jl/c++-mode-hook)
  (c++-mode . lsp-mode)
  )

(use-package modern-cpp-font-lock
  :after c++-mode
  :straight t)

(use-package general
  :straight t
)


(general-define-key
 "C-w" 'kill-region-or-backward-word
 "C-x O" '(other-window-prev :which-key "previous window")
 ;; use hippie-expand instead of dabbrev
 "M-/" 'hippie-expand
 "s-/" 'hippie-expand
 ;; align code
 "C-x \\" 'align-regexp
 ;; mark-end-of-sentence is normally unassigned
 "M-h" 'mark-end-of-sentence
 ;; rebind to zap-up-to-char instead of zap-to-char
 "M-z" 'zap-up-to-char)

(general-define-key
 :keymaps 'prog-mode-map
 "s-f" 'mark-defun)

(use-package mark-thing-at
  :straight t
  :hook (after-init . mark-thing-at-mode))

;;; open current file in explorer/finder
(when *is-win*
      (general-define-key
       "M-O" #'jl/open-folder-in-explorer
       ))

(use-package reveal-in-osx-finder
  :straight t
  :if *is-mac*
  :bind ("M-o" . reveal-in-osx-finder))

(defun jl/c++-mode-hook ()
  "FIX prevent bug in smartparens."
  (progn
    (setq sp-escape-quotes-after-insert nil)
    (make-local-variable 'sp-escape-quotes-after-insert)))


(use-package clang-format
  :straight t
  :after cc-mode
  :commands (clang-format-buffer clang-format-defun)
  :bind* (:map c++-mode-map
               ("C-c f" . clang-format-region)))

(use-package ibuffer
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
  :straight t
  )

;; show the cursor when moving after big movements in the window
(use-package beacon
  :straight t
  :diminish beacon-mode
  :hook (after-init . beacon-mode))

(use-package move-text
  :straight t
  :bind
  (("M-<up>"    . move-text-up)
   ("M-<down>"  . move-text-down)))

;; edit grep-buffers, e.g., ivy-occur
(use-package wgrep
  :commands wgrep-mode
  :init (add-hook 'ivy-occur-grep-mode-hook
	                (lambda () (toggle-truncate-lines 1)))
  )

(use-package ws-butler
  :straight t
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
  :straight t
  :defer t
  :custom
  (request-curl (if *is-win*  "c:/ProgramData/chocolatey/bin/curl.exe" "curl"))
  (request-message-level 'warn)
  (request-log-level 'warn)
  )

(use-package eldoc
  :diminish eldoc-mode)

;;(use-package yasnippet-snippets
;;  :straight t)

(use-package yasnippet
  :straight t
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
  :straight (arm-lookup :type git :host github :repo "jonatanlinden/arm-lookup")
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

(use-package org
  :straight t
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-export-backends '(ascii html md))
  (org-directory "d:/work/notes")
  (org-hide-emphasis-markers t)
  :bind (("C-c c" . org-capture))
  :config
  (setq org-id-track-globally t)
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline "d:/work/notes/todo.org" "Tasks")
           "* TODO %i%? %a")
          ))
  (setq org-todo-keywords '((sequence "OPEN" "IN PROGRESS" "|" "CLOSED")))
  ;; :hook (org-mode . visual-line-mode) Doesn't play nice with ejira
  )

(use-package org-mru-clock
  :straight t
  :bind* (("C-c C-x i" . org-mru-clock-in)
          ("C-c C-x C-j" . org-mru-clock-select-recent-task))
  :custom
  (org-mru-clock-how-many 10)
  (org-mru-clock-completing-read #'ivy-completing-read))

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(use-package ox-pandoc
  :disabled t
  :straight t
  :after ox
  )

(csetq ffap-machine-p-known 'reject)

(use-package mediawiki
  :straight t
  :commands (mediawiki-mode)
  :init
  ;; workaround, unable to run mediawiki-open otherwise
   (setq url-user-agent "EMACS")
  :hook (mediawiki-mode . visual-line-mode)
  )

(use-package alert
  :if window-system
  :straight t
  :commands (alert)
  :custom (alert-default-style 'mode-line)
  )

(use-package cmake-mode
  :straight (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :config
  (make-local-variable 'company-backends)
  (push 'company-cmake company-backends)
  :mode "CMakeLists.txt")

(use-package toml-mode
  :straight t
  :mode ("\\.toml\\'")
  )

(use-package rust-mode
  :straight t
  :hook (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after rust
  :straight t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package multiple-cursors
  :straight t
  :bind (("C-c m c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this-symbol)
         ("C-<" . mc/mark-previous-like-this-symbol)
         ("M-C->" . mc/mark-next-like-this-word))
  )

(defun jl/magit-log-edit-mode-hook ()
  (setq fill-column 72)
  (turn-on-auto-fill))


(use-package magit
  :straight t
  :custom (magit-section-initial-visibility-alist '((stashes . hide) (untracked . hide)))
  :bind (("C-x g" . magit-status)
         ("C-c g l" . magit-list-repositories)
         )
  :hook (magit-mode . magit-svn-mode))

;; Transient commands: replaces the old magit-popup
(use-package transient :defer t
  :config (transient-bind-q-to-quit))

;; git-messenger: popup commit message at current line
;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
  :straight t
  :custom (git-messenger:use-magit-popup t)
  :bind
  (("C-c g m" . git-messenger:popup-message)
   :map git-messenger-map
   ([(return)] . git-messenger:popup-close))
  :config
  ;; Enable magit-show-commit instead of pop-to-buffer
  (setq git-messenger:show-detail t))

(use-package magit-svn
  :straight t
  :diminish magit-svn-mode
  :commands (magit-svn-mode)
  )


(defun clang-format-defun ()
  "Run clang-format on the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (clang-format (region-beginning) (region-end))))



(use-package esup
  :straight t
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
  :straight t
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

;;; in bat mode, treat _ as a word constitutent
(add-hook 'bat-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(windmove-default-keybindings 'control)
;; numbered window shortcuts
(use-package winum
  :straight t
  :defer t
  :config
  (winum-mode))

(use-package posframe
  :after xref-posframe
  :straight t)

(use-package xref-posframe
  :after xref-asm
  :load-path "lisp/xref-posframe")

(use-package xref-asm
  :load-path "lisp/xref-asm"
  :after arm-mode
  :config
  (xref-asm-activate))

(use-package cheatsheet
  :straight t
  :commands (cheatsheet-show)
  )

(use-package powershell
  :straight t
  :mode ("\\.ps1\\'")
  :custom (powershell-location-of-exe (executable-find "pwsh"))
  )

(use-package nxml-mode
  :mode ("\\.xml\\'")
  :config (setq show-smartparens-mode -1))

(use-package point-history
  :disabled t
  :straight (point-history :type git :host github :repo "blue0513/point-history")
  :hook after-init
  :bind (("C-c C-/" . point-history-show))
  :init (setq point-history-ignore-buffer "^ \\*Minibuf\\|^ \\*point-history-show*"))

(use-package jl-ocaml
  :load-path "lisp"
  )

(use-package go-mode
  :straight t
  :bind (
         ;; If you want to switch existing go-mode bindings to use lsp-mode/gopls instead
         ;; uncomment the following lines
         ;; ("C-c C-j" . lsp-find-definition)
         ;; ("C-c C-d" . lsp-describe-thing-at-point)
         )
  :hook ((go-mode . lsp-deferred)))

(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'")
  :hook (csv-mode . csv-guess-set-separator))

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
