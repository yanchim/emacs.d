;;; init-prog.el --- Programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Generic Programming Configuration.
;;

;;; Code:

(when (treesit-available-p)
  (require 'treesit)
  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (c-or-c++-mode   . c-or-c++-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (csharp-mode     . csharp-ts-mode)
          (css-mode        . css-ts-mode)
          (java-mode       . java-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (js-mode         . js-ts-mode)
          (python-mode     . python-ts-mode)
          (ruby-mode       . ruby-ts-mode)
          (sh-mode         . bash-ts-mode))))

(defvar my-last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(with-eval-after-load 'compile
  ;; Save before compiling.
  (setq compilation-ask-about-save nil)
  ;; Kill old compile processes before starting the new one.
  (setq compilation-always-kill t)
  ;; Automatically scroll to first error.
  (setq compilation-scroll-output 'first-error)

  (defun my--save-compilation-buffer (&rest _)
    "Save the compilation buffer to find it later."
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

(keymap-global-set "C-c c k" #'compile)
(keymap-global-set "C-c c r" #'recompile)

;; Colorize output of Compilation Mode.
;; https://stackoverflow.com/a/3072831/355252
(with-eval-after-load 'compile
  (require 'ansi-color)

  (defun my--colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    ;; We don't want to mess with child modes such as grep-mode, ack, ag, etc.
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))

  (add-hook 'compilation-filter-hook #'my--colorize-compilation-buffer))

(defun my--generic-prog-mode-hook-setup ()
  "Generic configuration for `prog-mode'."
  ;; Camel case aware editing operations.
  (subword-mode +1))

(add-hook 'prog-mode-hook #'my--generic-prog-mode-hook-setup)

(provide 'init-prog)

;;; init-prog.el ends here
