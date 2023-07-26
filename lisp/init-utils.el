;;; init-utils.el --- utility -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Utility configuration.
;;

;;; Code:

(defconst my-linux-p (eq system-type 'gnu/linux)
  "Running on GNU/Linux.")

(defconst my-mac-p (eq system-type 'darwin)
  "Running on Mac system.")

(defconst my-cygwin-p (eq system-type 'cygwin)
  "Running on Cygwin system.")

(defconst my-win-p (eq system-type 'windows-nt)
  "Running on Windows system.")

(defconst my-mac-x-p (and (display-graphic-p) my-mac-p)
  "Running under X on Mac system.")

(defconst my-linux-x-p (and (display-graphic-p) my-linux-p)
  "Running under X on GNU/Linux system.")

(defconst my-root-p (string-equal "root" (getenv "USER"))
  "Root user.")

;; Fix PATH problem on macOS when using GUI Emacs.
(when my-mac-x-p
  (setenv "LANG" "en_US.UTF-8")
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path
              (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err)))))

;; Use GNU ls as `gls' from `coreutils' if available.
(when my-mac-p
  (let ((gls (executable-find "gls")))
    (if gls
        (setq insert-directory-program gls)
      ;; Suppress the Dired warning when not using GNU ls.
      (setq dired-use-ls-dired nil))))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html
(prefer-coding-system 'utf-8)

;; Shutdown the startup screen.
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

(defun my--initial-scratch-message ()
  "Customize `initial-scratch-message'."
  (let ((fortune-prog (executable-find "fortune")))
    (format
     ;; Comment first line and add two new lines in the end.
     ";; %s\n\n"
     (replace-regexp-in-string
      ;; Comment each line below first line.
      "\n"
      "\n;; "
      (cond
       (fortune-prog
        (replace-regexp-in-string
         ;; Remove extra escape sequences.
         (rx (or (seq ?\n eol)
                 (seq ?\C-\[ ?\[ (0+ digit) ?m)))
         ""
         (shell-command-to-string fortune-prog)))
       (t
        (concat "Now, trailblazers"
                "\nKeep credos in mind"
                "\n(I won't say it twice!)"
                "\nOne! Stop staying within the lines"
                "\nTwo! We always align"
                "\nThree! Even if we don't gain the upper hand, we'll fight for right"
                "\nFour! Never care a rap for hindsight"
                "\nFive! Let us light the night"
                "\nSix! Even when there are wheels within wheels, go ahead!"
                "\nGet it pulverized")))))))

(setq-default initial-scratch-message (my--initial-scratch-message))

;; Nice scrolling.
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Disable the annoying bell ring.
(setq ring-bell-function #'ignore)

;; Use y/n instead of yes/no.
(setq use-short-answers t)

;; Repeating C-SPC after popping mark pops it again.
(setq set-mark-command-repeat-pop t)

;; Make mouse clicks more precise.
(setq mouse-prefer-closest-glyph t)

;;; Tab and Space
;; Indent with spaces.
(setq-default indent-tabs-mode nil)
;; But maintain correct appearance.
(setq-default tab-width 8)
;; Smart tab behavior - indent or complete.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)
;; TAB cycle if there are only few candidates.
(setq completion-cycle-threshold 3)

;; Enable narrowing commands.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Enabled change region case commands.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable erase-buffer command.
(put 'erase-buffer 'disabled nil)

;; Disable annoying blink.
(blink-cursor-mode -1)

;; Delete the selection with a key press.
(delete-selection-mode +1)

;; Fix Emacs performance when edit so-long files.
(global-so-long-mode +1)

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode +1)

;; Pairs...
(electric-pair-mode +1)

;; Show matching parentheses.
(show-paren-mode +1)
(setq show-paren-context-when-offscreen 'overlay)

;; Clean up obsolete buffers automatically.
(require 'midnight)

;;; Use-package.
(setq use-package-always-ensure t)
;; It must be set before loading `use-package'.
(setq use-package-enable-imenu-support t)

;; Do NOT make backups of files, not safe.
;; https://github.com/joedicastro/dotfiles/tree/master/emacs
(use-package files
  :ensure nil
  :custom
  (auto-save-default nil)
  (make-backup-files nil))

(use-package uniquify
  :ensure nil
  :custom
  ;; Don't muck with special buffers.
  (uniquify-ignore-buffers-re "^\\*"))

(use-package winner
  :hook (after-init . winner-mode)
  :custom
  (winner-boring-buffers '("*Apropos*" "*Buffer List*"
                           "*Completions*" "*Compile-Log*"
                           "*Help*" "*Ibuffer*"
                           "*inferior-lisp*")))

(use-package recentf
  :hook (after-init . recentf-mode)
  :bind (("C-c f f" . recentf-open-files)
         ("C-c f l" . recentf-load-list))
  :custom
  (recentf-max-saved-items 100)
  ;; It can be laggy when cleanup remote files.
  (recentf-auto-cleanup 'never)
  :config
  (dolist (regexp '("^/\\(?:ssh\\|su\\|sudo\\)?x?:"
                    "/\\.?TAGS\\'" "/\\.?tags\\'"))
    (add-to-list 'recentf-exclude regexp)))

;; Automatically reload files was modified by external program.
(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

(use-package whitespace
  :custom
  ;; Search {zero,full}-width space also.
  (whitespace-space-regexp "\\( +\\|　+\\|​+\\)")
  :config
  ;; Show zero-width space.
  (add-to-list 'whitespace-display-mappings '(space-mark #x200b [?.])))

(with-eval-after-load 'tramp
  (push (cons tramp-file-name-regexp nil) backup-directory-alist)

  ;; ;; https://github.com/syl20bnr/spacemacs/issues/1921
  ;; ;; If you tramp is hanging, you can uncomment below line.
  ;; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  (setq tramp-chunksize 8192))

;; Change the default behavior of hippie-expand.
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
;; Use `hippie-expand' instead of `dabbrev'.
(keymap-global-set "<remap> <dabbrev-expand>" #'hippie-expand)

(with-eval-after-load 'comint
  ;; Don't echo passwords when communicating with interactive programs:
  ;; Github prompt is like "Password for 'https://user@github.com/':"
  (setq comint-password-prompt-regexp
        (format "%s\\|^ *Password for .*: *$" comint-password-prompt-regexp))
  (add-hook 'comint-output-filter-functions
            #'comint-watch-for-password-prompt))

;; Security.
(setq auth-sources '("~/.authinfo.gpg"))

(with-eval-after-load 'epa
  ;; With GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to
  ;; prompt for the key passphrase.
  (setq epg-pinentry-mode 'loopback))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

;; Be able to M-x without meta.
(keymap-global-set "C-c m x" #'execute-extended-command)
;; Zero width space.
(keymap-global-set "C-c 8 z" (lambda ()
                               (interactive)
                               (insert-char #x200b)))
;; Ideographic space.
(keymap-global-set "C-c 8 i" (lambda ()
                               (interactive)
                               (insert-char #x3000)))

;; Toggle.
(keymap-global-set "C-c t A" #'abbrev-mode)
(keymap-global-set "C-c t a" #'auto-fill-mode)
(keymap-global-set "C-c t f f" #'toggle-frame-fullscreen)
(keymap-global-set "C-c t f m" #'toggle-frame-maximized)
(keymap-global-set "C-c t g" #'glasses-mode)
(keymap-global-set "C-c t h" #'global-hl-line-mode)
(keymap-global-set "C-c t i" #'display-fill-column-indicator-mode)
(keymap-global-set "C-c t j" #'toggle-truncate-lines)
(keymap-global-set "C-c t k" #'visual-line-mode)
(keymap-global-set "C-c t l" #'display-line-numbers-mode)
(keymap-global-set "C-c t r" #'cua-rectangle-mark-mode)
(keymap-global-set "C-c t s" #'subword-mode)
(keymap-global-set "C-c t t" #'load-theme)
(keymap-global-set "C-c t v" #'view-mode)
(keymap-global-set "C-c t w" #'whitespace-mode)

;; Abbrevs.
(setq save-abbrevs 'silently)

;; Search.
(keymap-global-set "C-c s d" #'find-dired)
(keymap-global-set "C-c s i" #'imenu)
(keymap-global-set "C-c s g" #'grep)

;; Isearch.
(keymap-global-set "C-M-s" #'isearch-forward-regexp)
(keymap-global-set "C-M-r" #'isearch-backward-regexp)
;; Activate occur easily inside isearch.
(keymap-set isearch-mode-map "C-o" #'isearch-occur)

;; Align code in a pretty way.
;; http://ergoemacs.org/emacs/emacs_align_and_sort.html
(keymap-global-set "C-x \\" #'align-regexp)

;; Open header file under cursor.
(keymap-global-set "C-x C-o" #'ffap)

;; A complementary binding to the apropos-command (C-h a).
(keymap-set 'help-command "A" #'apropos)

(keymap-set 'help-command "C-f" #'find-function)
(keymap-set 'help-command "C-k" #'find-function-on-key)
(keymap-set 'help-command "C-v" #'find-variable)
(keymap-set 'help-command "C-l" #'find-library)

(keymap-set 'help-command "C-i" #'info-display-manual)

(keymap-global-set "<remap> <just-one-space>" #'cycle-spacing)
(keymap-global-set "C-x M-u" #'revert-buffer)
(keymap-global-set "C-x M-c" #'capitalize-region)

(keymap-global-set "M-z" #'zap-up-to-char)
(keymap-global-set "M-Z" #'zap-to-char)

(keymap-global-set "M-s M-j" #'scroll-other-window)
(keymap-global-set "M-s M-k" #'scroll-other-window-down)

(provide 'init-utils)

;;; init-utils.el ends here
