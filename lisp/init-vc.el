;;; init-vc.el --- version control -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Version control system.
;;

;;; Code:

(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-c v g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c v d" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch)
         ("C-c v f" . magit-file-dispatch))
  :config
  ;; Add module section into the status buffer.
  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit-insert-modules
                          #'magit-insert-stashes #'append))

;; Access GIT forges from `magit'.
(use-package forge
  :after magit
  :config
  (setq forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
          ("Title" 50 t nil title  nil)
          ("State" 10 t nil state nil)
          ("Updated" 10 t nil updated nil))))

(use-package diff-hl
  :hook (after-init . global-diff-hl-mode)
  :config
  ;; Highlight on-the-fly.
  (diff-hl-flydiff-mode +1)

  (setq diff-hl-margin-symbols-alist
        '((insert . "+") (delete . "-") (change . "=")
          (unknown . "?") (ignored . "!")))

  (if (display-graphic-p)
      (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
    ;; Fall back to margin since fringe is unavailable in terminal.
    (add-hook 'dired-mode-hook #'diff-hl-margin-mode)
    ;; Avoid restoring `diff-hl-margin-mode' when using `desktop.el'.
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil))))

  ;; Integrate with `magit'.
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package git-modes
  :mode ("/\\.dockerignore\\'" . gitignore-mode))

(use-package git-link
  :bind (("C-c v l l" . git-link)
         ("C-c v l c" . git-link-commit)
         ("C-c v l h" . git-link-homepage)
         ("C-c v l t" . (lambda ()
                          "Toggle `git-link-use-commit'."
                          (interactive)
                          (if (bound-and-true-p git-link-use-commit)
                              (progn
                                (setq git-link-use-commit nil)
                                (message "Use the branch name."))
                            (setq git-link-use-commit t)
                            (message "Use the commit hash."))))))

(use-package git-timemachine
  :bind ("C-c v t" . git-timemachine))

(provide 'init-vc)

;;; init-vc.el ends here
