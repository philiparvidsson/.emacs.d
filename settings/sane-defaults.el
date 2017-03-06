;;------------------------------------------------------------------------------

;; Set frame title.
(setq frame-title-format '("%b"))

;; Disable tooltips.
(tooltip-mode -1)

(setq echo-keystrokes 0.1)
(setq shift-select-mode nil)

;; Don't require full yes or full no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show column and line numbers.
(setq column-number-mode t)
(setq line-number-mode t)

;; We don't really want lines wider than 80 characters.
(setq-default fill-column 80)

;; Don't use tabs.
(setq-default indent-tabs-mode nil)

;; Suppress the scratch buffer message.
(setq initial-scratch-message "")

;; Prefer utf-8.
(prefer-coding-system 'utf-8)

;; Prefer Unix line endings.
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Delete selection when typing.
(delete-selection-mode 1)

;; Match parentheses automatically.
(electric-pair-mode t)

;; Stop on 'subwords'.
(global-subword-mode 1)

;; Store backup files in /tmp/
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq backup-by-copying t)

;; Don't create .#-files.
(setq create-lockfiles nil)

;; Tweak mouse wheel scrolling.
;;(setq-default mouse-wheel-scroll-amount '(1))
;;(setq-default mouse-wheel-progressive-speed nil)
;; No, disable it altogether.
(dolist (k mwheel-installed-bindings)
  (global-set-key k 'ignore))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'sane-defaults)
