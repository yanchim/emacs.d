;;; init-vc.el --- version control -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Version control system.
;;

;;; Code:

(with-eval-after-load 'magit
  ;; add module section into the status buffer
  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit-insert-modules
                          #'magit-insert-stashes #'append))

(require 'diff-hl)
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook #'diff-hl-dired-mode)

(with-eval-after-load 'diff-hl
  (diff-hl-flydiff-mode +1)

  (setq diff-hl-margin-symbols-alist
        '((insert . "+") (delete . "-") (change . "=")
          (unknown . "?") (ignored . "!")))

  (unless (display-graphic-p)
    ;; fall back to margin since fringe is unavailable in terminal
    (diff-hl-margin-mode +1)
    ;; avoid restoring `diff-hl-margin-mode' when using `desktop.el'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil))))

  ;; integration with `magit'
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package git-modes
  :mode ("/\\.dockerignore\\'" . gitignore-mode))

(use-package git-link
  :bind (("C-c v l l" . git-link)
         ("C-c v l c" . git-link-commit)
         ("C-c v l h" . git-link-homepage)))

(use-package git-timemachine
  :bind ("C-c v t" . git-timemachine))

(provide 'init-vc)

;;; init-vc.el ends here
