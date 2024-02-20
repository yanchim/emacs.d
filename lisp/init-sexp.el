;;; init-sexp.el --- S-expression -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for dealing with S-expressions.
;;

;;; Code:

(defun my-endless-sharp ()
  "Insert #\\=' unless in a string or comment.

URL `https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html'."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?\'))
      (insert "'"))))

(keymap-set emacs-lisp-mode-map "#" #'my-endless-sharp)

(defun my-eval-print-last-sexp (&optional arg)
  "Evaluate sexp before point, insert output below following an arrow.
With a `\\[universal-argument]' prefix argument ARG, delete the
sexp before point and insert output into current position."
  (interactive "P")
  (let ((value (eval (elisp--preceding-sexp))))
    (save-excursion
      (cond
       ((not arg)
        (newline-and-indent)
        (if (and (stringp value) (string-match-p "\n" value))
            ;; If return value is a multiline string.
            (insert (format
                     ";; =>\n;; %S"
                     (replace-regexp-in-string "\n" "\n;; " value)))
          (insert (format "%s%S" ";; => " value))))
       ((equal arg '(4))
        (backward-kill-sexp)
        (insert (format "%S" value)))))))

(dolist (map (list emacs-lisp-mode-map
                   lisp-interaction-mode-map))
  (keymap-set map "C-c C-p" #'my-eval-print-last-sexp))

(use-package clojure-mode
  :mode
  ("\\.\\(cljd?\\|edn\\)\\'" . clojure-mode)
  ("\\.cljs\\'" . clojurescript-mode))

(use-package clojure-ts-mode
  :when (and (treesit-available-p) (treesit-ready-p 'clojure 'message))
  :mode
  ("\\.\\(clj\\|edn\\)\\'" . clojure-ts-mode)
  ("\\.cljs\\'" . clojure-ts-clojurescript-mode)
  ("\\.cljd\\'" . clojure-ts-clojuredart-mode))

(use-package cider
  :after (:any clojure-mode clojure-ts-mode)
  :bind (:map cider-start-map
              ("r" . cider-restart)
              ("C-r" . cider-restart))
  :config
  ;; https://github.com/clojure-emacs/cider/issues/3588
  (when (string= "powershell" cider-clojure-cli-command)
    (setq cider-clojure-cli-command "pwsh")))

(use-package sly
  :bind ((:map sly-mode-map
               ;; Unbind completion in `sly-mode-map'.
               ("C-c C-i" . nil)
               ("C-c C-x C-j" . sly)
               ("C-c C-x C-q" . sly-disconnect))
         (:map sly-doc-map
               ("C-l" . sly-documentation)))
  :custom (inferior-lisp-program "sbcl"))

(use-package racket-mode
  :bind (:map racket-mode-map
              ("C-c C-x C-j" . racket-run)
              ("C-c C-x C-x" . racket-xp-mode)
              ("C-c C-x C-e" . racket-eval-last-sexp)))

(use-package fennel-mode
  :mode ("\\.fnl\\'" . fennel-mode))

(provide 'init-sexp)

;;; init-sexp.el ends here
