;; Used to remember the last active buffer in the other window in `toggle-dual-window-view'.
(defvar my-other-window-buffer nil)

(defun toggle-dual-window-view ()
  "Toggle between displaying one window (normal) and two windows (side-by-side)."
  (interactive)
  (if (eq (length (window-list)) 2)
      ;; Two windows open, so save the buffer that is open in the other window and close it, then
      ;; halve the frame width.
      (progn
        (setq my-other-window-buffer (save-window-excursion (other-window 1) (current-buffer)))
        (delete-other-windows)
        (set-frame-size nil (/ (frame-width) 2) (frame-height)))
    (progn
      ;; One (probably) window open, so double the frame width and display the last shown buffer
      ;; in the other window.
      (set-frame-size nil (* (frame-width) 2) (frame-height))
      (split-window-right)
      (when (buffer-live-p my-other-window-buffer)
        (other-window 1)
        (set-window-buffer (selected-window) my-other-window-buffer)
        (other-window 1)))))

(defun toggle-presentation-mode ()
  "Toggle presentation (large text and fullscreen) mode."
  (interactive)
  ;; I'm using `progn' below because fullscreen has to be toggled in a certain order to preserve
  ;; frame dimensions.
  (let ((fullscreen (frame-parameter nil 'fullscreen)))
    (if (memq fullscreen '(fullscreen fullboth))
        (progn
          (text-scale-decrease 2)
          (toggle-frame-fullscreen))
      (progn
        (toggle-frame-fullscreen)
        (text-scale-increase 2)))))

(defun kill-other-buffers (&optional arg)
  "Kill all buffers except the current one (and *scratch*).  If ARG is non-nil, kill all buffers."
  (interactive "P")
  (let* ((scratch-buffer (get-buffer "*scratch*"))
         (all-buffers-except-scratch (delq scratch-buffer (buffer-list))))
    (if arg
        (mapc 'kill-buffer all-buffers-except-scratch)
      (mapc 'kill-buffer (delq (current-buffer) all-buffers-except-scratch)))))

;; Easy way to type symbol without enabling `xah-math-input-mode'.
(global-set-key (kbd "C-<return>") 'xah-math-input-change-to-symbol)

;; Shortcut to align lines by regexp.
(global-set-key (kbd "C-c a") 'align-regexp)

;; Switch between buffers easily.
(global-set-key (kbd "C-c b") 'ivy-switch-buffer)

;; Comment easily.
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Toggle double frame view.
(global-set-key (kbd "C-c d") 'toggle-dual-window-view)

;; Open current directory with the configured file manager.
(global-set-key (kbd "C-c e") 'my-open-file-manager)

;; Presentation mode.
(global-set-key (kbd "C-c f") 'toggle-presentation-mode)

;; Move point to a line quickly.
(global-set-key (kbd "C-c g") 'goto-line)

;; Toggle syntax highlighting.
(global-set-key (kbd "C-c h") 'global-font-lock-mode)

;; Indent region.
(global-set-key (kbd "C-c i") 'indent-region)

;; Kill other buffers easily.
(global-set-key (kbd "C-c k") 'kill-other-buffers)

;; Sort lines.
(global-set-key (kbd "C-c o") 'sort-lines)

;; Close automatically opened window (from, e.g., search).
(global-set-key (kbd "C-c q") 'delete-other-windows-vertically)

;; Hydra for navigating errors/search results.
(defhydra hydra-navigate-errors (global-map "C-c r")
  "Navigate errors"
  ("e" (previous-error))
  ("r" (next-error))
  ("<escape>" delete-other-windows-vertically :exit t))

;; Hydra for `multiple-cursors'.
(defhydra hydra-multiple-cursors (global-map "C-c m")
  "Manage cursors"
  ("a" (mc/mark-all-dwim (use-region-p)))
  ("b" (mc/cycle-backward))
  ("f" (mc/cycle-forward))
  ("i" (mc/insert-numbers 1))
  ("m" (mc/mark-next-like-this 1))
  ("n" (mc/mark-next-like-this-symbol 1))
  ("<escape>" nil :exit t))

;; Open terminal in current directory.
(global-set-key (kbd "C-c t") 'my-open-terminal)

;; Easy access to Magit.
(global-set-key (kbd "C-c v") 'magit-status)

;; I don't like C-x 5 0.
(global-set-key (kbd "C-c w") 'delete-frame)

;; Move faster through text when holding the shift key.
(global-set-key (kbd "C-S-b") 'left-word)
(global-set-key (kbd "C-S-f") 'right-word)
(global-set-key (kbd "C-S-n") 'forward-paragraph)
(global-set-key (kbd "C-S-p") 'backward-paragraph)

;; Search bindings.
(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)
(global-set-key (kbd "C-c s") 'swiper-all)

;; Easy evaluation of regions in `emacs-lisp-mode'.
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (local-set-key (kbd "C-<return>") 'eval-region)))

;; https://www.emacswiki.org/emacs/NeoTree#toc11
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;; NeoTree.
(global-set-key (kbd "C-c n") 'neotree-project-dir)
