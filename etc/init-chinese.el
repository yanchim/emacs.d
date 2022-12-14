;;; init-chinese.el --- Chinese integration for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Chinese integration.
;;

;;; Code:

;; ---------
;; Emacs IME
;; ---------

(use-package pyim
  :bind (("C-\\" . toggle-input-method)
         ("M-j" . pyim-convert-string-at-point)
         (:map pyim-mode-map
               ("," . pyim-page-previous-page)
               ("." . pyim-page-next-page)))
  :custom (default-input-method "pyim")
  :config
  (setq pyim-default-scheme 'quanpin)

  ;; compatible with terminal
  (setq pyim-page-tooltip 'minibuffer)
  (setq pyim-page-style 'two-lines)
  (setq pyim-page-length 9)
  (setq pyim-fuzzy-pinyin-alist
        '(("en" "eng")
          ("in" "ing")))

  ;; ;; Rime config
  ;; (liberime-start
  ;;   (if my-mac-p
  ;;       "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"
  ;;     "/usr/share/rime-data")
  ;;   (expand-file-name "rime/" my-cache-d))
  ;; (liberime-select-schema "luna_pinyin")

  ;; use memory efficient pyim engine
  (require 'pyim-dregcache)
  (setq pyim-dcache-backend 'pyim-dregcache)

  ;; change input method automatically
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; ----
  ;; dict
  ;; ----
  (defvar my-pyim-directory (expand-file-name "pyim/" my-cache-d)
    "The directory containing pyim related files.")

  (unless (file-directory-p my-pyim-directory) (mkdir my-pyim-directory))

  ;; pyim-bigdict is recommended (20M).
  ;; There are too many useless words in pyim-greatdict
  ;; which also slows down pyim performance.
  ;; curl -L https://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim.gz | zcat > path/to/my-pyim-directory/pyim-bigdict.pyim
  ;; load all "*.pyim" under `my-pyim-directory'.
  (let ((files (directory-files-recursively
                my-pyim-directory
                "\\.pyim\\'"))
        disable-basedict)
    (when (and files (> (length files) 0))
      (setq pyim-dicts
            (mapcar
             (lambda (f)
               (list :name (file-name-base f) :file f))
             files))
      ;; disable basedict if a local dict is used
      (setq disable-basedict t))
    (unless disable-basedict (pyim-basedict-enable))))

(provide 'init-chinese)

;;; init-chinese.el ends here
