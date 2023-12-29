;;; init-web.el --- Web develop configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for web develop.
;;

;;; Code:

(use-package emmet-mode
  :vc (:url "https://github.com/dalugm/emmet-mode" :rev :newest)
  :hook (css-mode
         heex-ts-mode
         html-mode
         js-jsx-mode
         js-mode
         js-ts-mode
         nxml-mode
         sgml-mode
         tsx-ts-mode
         web-mode))

(use-package web-mode
  :mode "\\.\\(?:cs\\|[px]\\)?html?\\'"
  :mode "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.eco\\'"
  :mode "\\.erb\\'"
  :mode "\\.hbs\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.jsp\\'"
  :mode "\\.mustache\\'"
  :mode "\\.svelte\\'"
  :mode "\\.twig\\'"
  :mode "\\.wxml\\'"
  :mode "\\.[nu]?vue\\'"
  :mode "templates/.+\\.php\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  ;; `web-mode-enable-auto-*' features only enabled in graphic mode
  ;; which is related on pasting issues on terminal.
  ;; https://github.com/fxbois/web-mode/issues/1175
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-auto-pairing nil))

(provide 'init-web)

;;; init-web.el ends here
