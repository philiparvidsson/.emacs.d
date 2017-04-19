;;------------------------------------------------------------------------------

;; 'Alt-tab' between two buffers with C-b
(global-set-key (kbd "C-b") (lambda () (interactive) (switch-to-buffer nil)))

;; Use C-k to kill entire line.
(global-set-key (kbd "C-k") 'kill-whole-line)

;; Shortcut to align lines by regexp.
(global-set-key (kbd "M-p") 'align-regexp)

;; Move between windows easily.
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)

;; Open current directory with Thunar.
(global-set-key (kbd "M-e")
                (lambda () (interactive)
                  (call-process "thunar" nil 0 nil default-directory)))

(when (display-graphic-p)
  (global-set-key
   (kbd "C-x 3")
   (lambda
     ()
     (interactive)
     (set-frame-size (selected-frame) 196 62) (split-window-right))))

(provide 'key-bindings)
