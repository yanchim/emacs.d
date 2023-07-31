;;; init-theme.el --- Theme for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Theme configuration.
;;

;;; Code:

(use-package standard-themes)

(use-package doom-themes)

(use-package sinolor-themes
  :vc (:url "https://github.com/dalugm/sinolor-themes" :rev :newest))

(provide 'init-theme)

;;; init-theme.el ends here
