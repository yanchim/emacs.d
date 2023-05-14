;;; init-prog.el --- Programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Generic Programming Configuration.
;;

;;; Code:

(use-package treesit
  :when (treesit-available-p)
  :init
  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c.git"))
          (cmake      . ("https://github.com/uyha/tree-sitter-cmake.git"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
          (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css.git"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git"))
          (elixir     . ("https://github.com/elixir-lang/tree-sitter-elixir.git"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go.git"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (heex       . ("https://github.com/phoenixframework/tree-sitter-heex.git"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html.git"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json.git"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python.git"))
          (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby.git"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml.git"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml.git"))))
  :custom
  (major-mode-remap-alist
   '((c-mode          . c-ts-mode)
     (c++-mode        . c++-ts-mode)
     (c-or-c++-mode   . c-or-c++-ts-mode)
     (conf-toml-mode  . toml-ts-mode)
     (csharp-mode     . csharp-ts-mode)
     (css-mode        . css-ts-mode)
     (html-mode       . html-ts-mode)
     (java-mode       . java-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-json-mode    . json-ts-mode)
     (js-mode         . js-ts-mode)
     (python-mode     . python-ts-mode)
     (ruby-mode       . ruby-ts-mode)
     (sh-mode         . bash-ts-mode)))
  :config
  ;; Add `*-ts-mode' to `auto-mode-alist'.
  (dolist (list `((cmake      . (,(rx (or "CMakeLists.txt" ".cmake") eos) . cmake-ts-mode))
                  (dockerfile . (,(rx "Dockerfile" (opt "." (zero-or-more nonl)) eos) . dockerfile-ts-mode))
                  (elixir     . (,(rx (or ".elixir" (seq ".ex" (opt "s")) "mix.lock") eos) . elixir-ts-mode))
                  (go         . (,(rx ".go" eos) . go-ts-mode))
                  (gomod      . (,(rx "/go.mod" eos) . go-mod-ts-mode))
                  (heex       . (,(rx "." (opt (any "hl")) "eex" eos) . heex-ts-mode))
                  (rust       . (,(rx ".rs" eos) . rust-ts-mode))
                  (tsx        . (,(rx ".tsx" eos) . tsx-ts-mode))
                  (typescript . (,(rx ".ts" eos) . typescript-ts-mode))
                  (yaml       . (,(rx ".y" (opt "a") "ml" eos) . yaml-ts-mode))))
    (let ((parser (car list))
          (alist (cdr list)))
      (when (treesit-ready-p parser 'message)
        (add-to-list 'auto-mode-alist alist)))))

(use-package compile
  :bind (("C-c c k" . compile)
         ("C-c c r" . recompile))
  :custom
  (compilation-ask-about-save nil)
  (compilation-always-kill t)
  (compilation-scroll-output 'first-error)
  :config
  ;; Colorize output of Compilation Mode.
  ;; https://stackoverflow.com/a/3072831/355252
  (require 'ansi-color)

  (defun my--colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    ;; Don't mess with child modes such as grep, ack, ag, etc.
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))

  (add-hook 'compilation-filter-hook #'my--colorize-compilation-buffer)

  (defvar my-last-compilation-buffer nil
    "The last buffer in which compilation took place.")

  (defun my--save-compilation-buffer (&rest _)
    "Save the last compilation buffer to find it later."
    (setq my-last-compilation-buffer next-error-last-buffer))

  (advice-add 'compilation-start :after #'my--save-compilation-buffer)

  (defun my--find-prev-compilation (orig &optional edit-command)
    "Find the previous compilation buffer, if present, and recompile there."
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             my-last-compilation-buffer
             (buffer-live-p (get-buffer my-last-compilation-buffer)))
        (with-current-buffer my-last-compilation-buffer
          (funcall orig edit-command))
      (funcall orig edit-command)))

  (advice-add 'recompile :around #'my--find-prev-compilation))

(use-package etags
  :defer t
  :custom (tags-revert-without-query t))

(use-package subword
  :hook ((prog-mode text-mode) . subword-mode))

(use-package xml-mode
  :mode "\\.[^.]*proj\\'"
  :mode "\\.xaml\\'"
  :mode "\\.p\\(?:list\\|om\\)\\'"
  :mode "\\.xs\\(?:d\\|lt\\)\\'"
  :mode "\\.rss\\'")

(use-package eldoc-box
  :when (display-graphic-p)
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :custom
  (eldoc-box-only-multi-line t)
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-use-visible-frame-map t)
  :bind (("C-c h h" . eldoc-box-help-at-point)
         (:map eldoc-box-visible-frame-map
               ("M-n" . eldoc-box-scroll-up)
               ("M-p" . eldoc-box-scroll-down)
               ("M-a" . eldoc-box-beginning)
               ("M-e" . eldoc-box-end)))
  :config
  (setq eldoc-doc-buffer-separator
        (concat "\n"
                (propertize "-"
                            'display '(space :align-to right)
                            'face '(:strike-through t)
                            'font-lock-face '(:strike-through t))
                "\n")))

(provide 'init-prog)

;;; init-prog.el ends here
