;; tuareg: Major mode for editing OCaml code
(use-package tuareg
  :straight t
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :init
  ;; Make OCaml-generated files invisible to filename completion
  (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".cmti" ".annot"))
    (add-to-list 'completion-ignored-extensions ext))

  (with-eval-after-load 'smartparens
    ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
    (sp-local-pair 'tuareg-mode "'" nil :actions nil)
    (sp-local-pair 'tuareg-mode "`" nil :actions nil)))

(defun jl/ocaml-mode-hook()
  (set (make-local-variable 'company-backends)
       '((merlin-company-backend company-files :with company-yasnippet)
         (company-dabbrev-code company-dabbrev))))

;; merlin: Context sensitive completion for OCaml in Vim and Emacs
(use-package merlin
  :straight t
  :custom
  (merlin-completion-with-doc t)
  ;; Use opam switch to lookup ocamlmerlin binary
  (merlin-command 'opam)
  :hook ((tuareg-mode . merlin-mode)
  (tuareg-mode . jl/ocaml-mode-hook)
  (tuareg-mode . company-mode)))


(defun ocaml-init-flycheck ()
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)
  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))

;; OCaml support for Flycheck using Merlin
(use-package flycheck-ocaml
  :straight t
  :hook (merlin-mode . ocaml-init-flycheck))

;; utop is an improved toplevel for OCaml. It can run in a terminal or in
;; Emacs. It supports line editing, history, real-time and context sensitive
;; completion, colors, and more.
  (use-package utop
    :defer t
    :straight t
    :init
    (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
    (add-hook 'tuareg-mode-hook 'utop-minor-mode)
    :config
    (setq utop-command "opam config exec -- utop -emacs")
    )

;; ocp-indent: Indentation tool for OCaml, to be used from editors like Emacs
(use-package ocp-indent
  :straight t
  :hook (tuareg-mode . ocp-setup-indent))

(provide 'jl-ocaml)
;;; jl-ocaml.el ends here
