;;; init-python.el --- programming in python -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Python configuration.
;;

;;; Code:

(with-eval-after-load 'python
  (setq python-indent-guess-indent-offset nil)
  (setq python-indent-offset 4))

(provide 'init-python)

;;; init-python.el ends here
