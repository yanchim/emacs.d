;;; init-markup.el --- markup languages configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Markup language configuration.
;;

;;; Code:

(use-package toc-org
  :hook ((org-mode markdown-mode) . toc-org-mode)
  :config
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map
                (kbd "C-c C-o")
                #'toc-org-markdown-follow-thing-at-point)))

;; Pixel-perfect visual alignment for Org and Markdown tables.
(use-package valign
  :when (display-graphic-p)
  :hook ((org-mode markdown-mode) . valign-mode)
  :config
  (defun my-valign-fancy-bar ()
    "Toggle valign fancy bar."
    (interactive)
    (setq valign-fancy-bar
          (not valign-fancy-bar)))

  ;; compatible with `outline-mode'
  (define-advice outline-show-entry (:override nil)
    "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
    (interactive)
    (save-excursion
      (outline-back-to-heading t)
      (outline-flag-region (max (point-min) (1- (point)))
                           (progn
                             (outline-next-preface)
                             (if (= 1 (- (point-max) (point)))
                                 (point-max)
                               (point)))
                           nil))))

(provide 'init-markup)

;;; init-markup.el ends here
