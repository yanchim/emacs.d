;;; init-theme.el --- Theme for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Theme configuration.
;;

;;; Code:

(use-package standard-themes)

(use-package ef-themes)

(use-package modus-themes)

(use-package color-theme-sanityinc-solarized)

(use-package color-theme-sanityinc-tomorrow)

(use-package dracula-theme)

(use-package monokai-pro-theme)

(use-package tao-theme)

(use-package zenburn-theme)

(use-package sinolor-themes
  :vc (:url "https://github.com/dalugm/sinolor-themes"))

(provide 'init-theme)

;;; init-theme.el ends here
