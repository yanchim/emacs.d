;;; init-lsp.el --- lsp configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Language Server Protocol (LSP) configuration.
;;

;;; Code:

(with-eval-after-load 'eglot
  ;; Elixir.
  (when my-mac-p
    (add-to-list 'eglot-server-programs
                 '((elixir-ts-mode heex-ts-mode) . ("elixir-ls")))))

(provide 'init-lsp)

;;; init-lsp.el ends here
