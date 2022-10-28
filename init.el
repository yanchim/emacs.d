;;; init.el --- Emacs start file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Emacs start-up configuration.
;;

;;; Code:

;; ;;; Packages
;; (unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
;;   (setq package-enable-at-startup nil)          ; To prevent initializing twice
;;   (package-initialize))

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

;; HTTPS URLs should be used where possible
;; as they offer superior security
(with-eval-after-load 'package
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    (setq package-archives
          `(

            ;; ;; official
            ;; ,(cons "gnu"    (concat proto "://elpa.gnu.org/packages/"))
            ;; ,(cons "nongnu" (concat proto "://elpa.nongnu.org/nongnu/"))
            ;; ,(cons "melpa"  (concat proto "://melpa.org/packages/"))
            ;; ;; ,(cons "melpa-stable" (concat proto "://stable.melpa.org/packages/"))
            ;; ,(cons "org"    (concat proto "://orgmode.org/elpa/"))

            ;; ;; emacs-china
            ;; ,(cons "gnu"    (concat proto "://elpa.emacs-china.org/gnu/"))
            ;; ,(cons "nongnu" (concat proto "://elpa.emacs-china.org/nongnu/"))
            ;; ,(cons "melpa"  (concat proto "://elpa.emacs-china.org/melpa/"))
            ;; ;; ,(cons "melpa-stable" (concat proto "://elpa.emacs-china.org/stable-melpa/"))
            ;; ,(cons "org"    (concat proto "://elpa.emacs-china.org/org/"))

            ;; ;; 163
            ;; ,(cons "gnu"    (concat proto "://mirrors.163.com/elpa/gnu/"))
            ;; ,(cons "nongnu" (concat proto "://mirrors.163.com/elpa/nongnu/"))
            ;; ,(cons "melpa"  (concat proto "://mirrors.163.com/elpa/melpa/"))
            ;; ;; ,(cons "melpa-stable" (concat proto "://mirrors.163.com/elpa/stable-melpa/"))
            ;; ,(cons "org"    (concat proto "://mirrors.163.com/elpa/org/"))

            ;; tuna
            ,(cons "gnu"    (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
            ,(cons "nongnu" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"))
            ,(cons "melpa"  (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
            ;; ,(cons "melpa-stable" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/"))
            ,(cons "org"    (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))

            ))))

;;; Define necessary directories

(setq user-init-file (or load-file-name buffer-file-name))
(setq user-emacs-directory (file-name-directory user-init-file))

(defvar my-config-d (expand-file-name "etc/" user-emacs-directory)
  "Directory of configuration files.")

(defvar my-library-d (expand-file-name "lib/" user-emacs-directory)
  "Directory of packages, whether from ELPA or Github.")

(defvar my-optional-d (expand-file-name "opt/" user-emacs-directory)
  "Directory of optional files.")

(defvar my-cache-d (expand-file-name "var/" user-emacs-directory)
  "Directory of dotfiles created by packages.")

(unless (file-directory-p my-cache-d) (mkdir my-cache-d))

;;; Garbage Collection
;; https://www.reddit.com/r/emacs/comments/brc05y/is_lspmode_too_slow_to_use_for_anyone_else/eofulix/
(defvar my--gc-cons-threshold-up-limit (* 100 1024 1024)
  "Best up-limit GC threshold value.  Should NOT be too big!")

(defvar my--gc-cons-threshold-default (* 20 1024 1024)
  "Default GC threshold value.")

(defun my--inc-gc-cons-threshold ()
  "Increase `gc-cons-threshold' to `my--gc-cons-threshold-up-limit'."
  (setq gc-cons-threshold my--gc-cons-threshold-up-limit))

(defun my--reset-gc-cons-threshold ()
  "Rest `gc-cons-threshold' to `my--gc-cons-threshold-default'."
  (setq gc-cons-threshold my--gc-cons-threshold-default))

;; Avoid Emacs do GC during the initializing
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(progn
  (my--inc-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (my--reset-gc-cons-threshold)
              (add-hook 'minibuffer-setup-hook
                        #'my--inc-gc-cons-threshold)
              (add-hook 'minibuffer-exit-hook
                        #'my--reset-gc-cons-threshold))))

;;; Configuration
(push (expand-file-name my-config-d) load-path)

;; Load configs for specific features and modes
(require 'init-utils)
(require 'init-modeline)
(require 'init-funcs)
(require 'init-dired)
(require 'init-org)
(require 'init-ibuffer)
(require 'init-package)
(require 'init-edit)
(require 'init-vc)
;; (require 'init-ido)
(require 'init-vertico)
(require 'init-complete)
(require 'init-prog)
(require 'init-check)

;; handy tools though not must have
(when (display-graphic-p)
  (require 'init-gui))
(require 'init-misc)
(require 'init-term)
(require 'init-markup)
(require 'init-chinese)

;; program
(require 'init-tex)
(require 'init-sexp)
(require 'init-cc)
(require 'init-js)
(require 'init-python)

;; personal setup, other major-mode specific setup need it.
(load (expand-file-name "~/.custom.el") t nil)

;; https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
;; See `custom-file' for details.
(load (setq custom-file
            (expand-file-name (concat user-emacs-directory
                                      "custom-set-variables.el")))
      t t)

(message "*** Emacs loaded in %s with %d garbage collections. ***"
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))
         gcs-done)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init.el ends here
