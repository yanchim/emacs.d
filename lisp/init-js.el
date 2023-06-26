;;; init-js.el --- JavaScript programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  JavaScript configuration.
;;

;;; Code:

(use-package js
  :ensure nil
  :mode ("\\.\\(m\\|c\\)?js\\'" . js-mode)
  :custom (js-indent-level 2))

(provide 'init-js)

;;; init-js.el ends here
