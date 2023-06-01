;;; init-lsp.el --- lsp configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Language Server Protocol (LSP) configuration.
;;

;;; Code:

(use-package eglot
  :bind (("C-c l l" . eglot)
         ("C-c l a" . eglot-code-actions)
         ("C-c l c" . eglot-show-workspace-configuration)
         ("C-c l f" . eglot-format)
         ("C-c l q" . eglot-shutdown)
         ("C-c l Q" . eglot-shutdown-all)
         ("C-c l r" . eglot-rename))
  :config
  ;; Elixir.
  (when my-mac-p
    (add-to-list 'eglot-server-programs
                 '((elixir-ts-mode heex-ts-mode) . ("elixir-ls")))))

(provide 'init-lsp)

;;; init-lsp.el ends here
