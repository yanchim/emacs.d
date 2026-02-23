;;; early-init.el --- `early-init-file' -*- lexical-binding: t -*-

;; Prevent outdated byte code files from being loaded
(setq load-prefer-newer t)

;; Prevent the glimpse of un-styled Emacs
(push '(menu-bar-lines . nil) default-frame-alist)
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
