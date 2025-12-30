;;; early-init.el --- `early-init-file' -*- lexical-binding: t -*-

;; Prevent outdated byte code files from being loaded.
(setq load-prefer-newer t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . nil) default-frame-alist)
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(with-eval-after-load 'package
  (setq package-install-upgrade-built-in t)
  (setq package-archives
        '(

          ;; ;; Official.
          ;; ("gnu"          . "https://elpa.gnu.org/packages/")
          ;; ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
          ;; ("gnu-devel"    . "https://elpa.gnu.org/devel/")
          ;; ("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/")
          ;; ("melpa"        . "https://melpa.org/packages/")
          ;; ;; ("melpa-stable" . "https://stable.melpa.org/packages/")

          ;; ;; Emacs-china.
          ;; ("gnu"          . "https://elpa.emacs-china.org/gnu/")
          ;; ("nongnu"       . "https://elpa.emacs-china.org/nongnu/")
          ;; ("gnu-devel"    . "https://elpa.emacs-china.org/gnu-devel/")
          ;; ("nongnu-devel" . "https://elpa.emacs-china.org/nongnu-devel/")
          ;; ("melpa"        . "https://elpa.emacs-china.org/melpa/")
          ;; ;; ("melpa-stable" . "https://elpa.emacs-china.org/stable-melpa/")

          ;; ;; 163.
          ;; ("gnu"          . "https://mirrors.163.com/elpa/gnu/")
          ;; ("nongnu"       . "https://mirrors.163.com/elpa/nongnu/")
          ;; ("gnu-devel"    . "https://mirrors.163.com/elpa/gnu-devel/")
          ;; ("nongnu-devel" . "https://mirrors.163.com/elpa/nongnu-devel/")
          ;; ("melpa"        . "https://mirrors.163.com/elpa/melpa/")
          ;; ;; ("melpa-stable" . "https://mirrors.163.com/elpa/stable-melpa/")

          ;; Tuna.
          ("gnu"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ("nongnu"       . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
          ("gnu-devel"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu-devel/")
          ("nongnu-devel" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu-devel/")
          ("melpa"        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ;; ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")

          ))
  )

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
