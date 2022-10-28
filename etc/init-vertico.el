;;; init-vertico.el --- vertico configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  vertico + orderless + consult + embark
;;

;;; Code:

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom (vertico-cycle t))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (defun my--orderless-regexp (str)
    "Enhance `orderless-regexp' when searching STR."
    (require 'zh-lib)
    (setf (car str) (zh-lib-build-regexp-string (car str)))
    str)
  (advice-add 'orderless-regexp :filter-args #'my--orderless-regexp))

(use-package consult
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add 'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind
  (([remap apropos-command] . consult-apropos)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap locate] . consult-locate)
   ([remap load-theme] . consult-theme)
   ([remap man] . consult-man)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ;; register access
   ([remap abbrev-prefix-mark] . consult-register-store)
   ("M-#" . consult-register-load)
   ("C-M-#" . consult-register)
   ;; other short keybindings
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-L" . consult-line)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("C-c s I" . consult-imenu-multi)
   ("C-c s f" . my-consult-find)
   ("C-c s F" . consult-find)
   ("C-c s L" . consult-locate)
   ("C-c s g" . my-consult-grep)
   ("C-c s G" . consult-grep)
   ("C-c s v" . consult-git-grep)
   ("C-c s r" . my-consult-ripgrep)
   ("C-c s R" . consult-ripgrep)
   ("C-c s l" . consult-line-multi)
   ("C-c s m" . consult-multi-occur)
   ("C-c s k" . consult-focus-lines)
   ("C-c s K" . consult-keep-lines)
   ;; minibuffer history
   (:map minibuffer-local-map
         ([remap next-matching-history-element] . consult-history)
         ([remap previous-matching-history-element] . consult-history))
   (:map isearch-mode-map
         ("M-s e" . consult-isearch-history)
         ("M-l" . consult-line)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ([remap isearch-edit-string] . consult-isearch-history)))

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; Relevant when use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  ;; ---------
  ;; customize
  ;; ---------
  (defun my--consult-zh-builder (input)
    "Add Zhongwen support for `consult' when searching INPUT."
    (require 'zh-lib)
    (let* ((str (car input))
           (len (length str)))
      (cond
       ;; do nothing
       ((<= len 0))
       ;; Detect the first character entered, if it matches `:',
       ;; convert the subsequent characters into Zhongwen regexp.
       ;; For expmale, input `:zw' matches ‘中文’, ‘植物’ and etc.
       ((string= (substring str 0 1) ":")
        (setf (car input) (zh-lib-build-regexp-string
                           (substring str 1 len))))))
    input)

  (dolist (func '(consult--find-builder
                  consult--git-grep-builder
                  consult--grep-builder
                  consult--locate-builder
                  consult--ripgrep-builder))
    (advice-add func :filter-args #'my--consult-zh-builder))

  (defun my-consult-find (&optional DIR)
    "Modify `consult-find' functions to search files in DIR."
    (interactive "DDirectory: ")
    (consult-find DIR))

  (defun my-consult-grep (&optional DIR)
    "Modify `consult-grep' functions to search files in DIR."
    (interactive "DDirectory: ")
    (consult-grep DIR))

  (defun my-consult-ripgrep (&optional DIR)
    "Modify `consult-ripgrep' functions to search files in DIR."
    (interactive "DDirectory: ")
    (consult-ripgrep DIR))

  (when my-win-p

    ;; ;; if Windows does not support utf-8 globally in CN environment
    ;; (add-to-list 'process-coding-system-alist
    ;;              '("[rR][gG]" . (utf-8 . gbk-dos)))

    ;; https://github.com/minad/consult/issues/475
    (defun my--consult-find-win (&optional dir initial)
      "Use `consult-find' in Windows with msys2."
      (let* ((w32-quote-process-args ?\\)  ; or (w32-quote-process-args ?*)
             (consult-find-args "C:\\msys64\\usr\\bin\\find.exe . -not ( -wholename */.* -prune )")
             (prompt-dir (consult--directory-prompt "Find" dir))
             (default-directory (cdr prompt-dir)))
        (find-file (consult--find
                    (car prompt-dir)
                    #'consult--find-builder
                    initial))))
    (advice-add 'consult-find :override #'my--consult-find-win))

  ;; The narrowing key.
  ;; Both `<' and `C-+' work reasonably well.
  (setq consult-narrow-key "<"))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-vertico)

;;; init-vertico.el ends here
