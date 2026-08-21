;;; early-init.el --- `early-init-file' -*- lexical-binding: t -*-

;; Prevent outdated byte code files from being loaded
(setq load-prefer-newer t)

;; Make installed packages available before loading `init.el', so
;; Auto-Compile can protect the init file and packages loaded early.
(package-initialize)

(when (require 'auto-compile nil t)
  (setopt auto-compile-use-mode-line nil
          auto-compile-display-buffer nil
          auto-compile-toggle-deletes-nonlib-dest t
          auto-compile-source-recreate-deletes-dest t)
  (auto-compile-on-load-mode +1)
  (auto-compile-on-save-mode +1))

;; Prevent the glimpse of un-styled Emacs
(push '(menu-bar-lines . nil) default-frame-alist)
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
