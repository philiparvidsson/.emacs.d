;;------------------------------------------------------------------------------

;; Custom bell function.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
			   (invert-face 'mode-line)
			   (run-with-timer 0.05 nil 'invert-face 'mode-line)))


;; Don't abort screen updates because of user input.
(setq redisplay-dont-pause t)

;; Don't blink the cursor.
(blink-cursor-mode -1)

;; Highlight current line.
(global-hl-line-mode 1)

;; Highlight match parentheses.
(show-paren-mode 1)

;; Always show line numbers.
(global-linum-mode t)

;; Wider 'gutter'.
(setq-default linum-format "%5d   ")

;; Disable fringes.
(fringe-mode '(0 . 0))

;; Path for custom themes.
;;(setq custom-theme-directory (concat user-emacs-directory "themes"))

;; Font is super important! ;-)
(set-frame-font "Liberation Mono-10.0:antialias=subpixel")

;; More line spacing makes code easier on the eyes.
(setq-default line-spacing 5)

;; Set up initial frame size if running in windowed mode.
(when (display-graphic-p)
  (set-frame-size (selected-frame) 118 62))

(provide 'appearance)
