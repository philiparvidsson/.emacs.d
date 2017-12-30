;;; init.el --- My personal Emacs configuration.

;; Author: Philip Arvidsson <hello@philiparvidsson.com>
;; URL: https://github.com/philiparvidsson/My-Emacs-Config

;;; License:

;;
;; Copyright 2018 Philip Arvidsson
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
;; associated documentation files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge, publish, distribute,
;; sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all copies or
;; substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
;; NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;

;;; Code:

;;;;------------------------------------
;;;; 1. Pre-initialization
;;;;------------------------------------

;; Set initial window size (108 columns x 52 rows).
(setq initial-frame-alist '((width . 108) (height . 52)))

;; Disable GUI stuff that I don't want or need.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; No startup message and suppress the scratch buffer message.
(setq inhibit-startup-message t
      initial-scratch-message nil)

;; Separate 'custom.el' file (otherwise it will be appended to this file).
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Set frame title.
(setq frame-title-format '("%b"))

;; Increase GC threshold to run GC less often.
(setq gc-cons-threshold 5000000)

;; Variables used for making decisions during initialization.
(setq running-on-windows (string-equal system-type "windows-nt"))

;;;;------------------------------------
;;;; 2. Setup sane defaults
;;;;------------------------------------

;; Automatically reload files when changes are detected.
(global-auto-revert-mode t)

;; Display keyboard shortcuts quickly in the echo area.
(setq echo-keystrokes 0.1)

;; Use C-<space> to set mark and select text instead of shift.
(setq shift-select-mode nil)

;; Delete selection (if any) when typing.
(delete-selection-mode t)

;; Show column- and line numbers in mode line.
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Don't require full yes or full no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Display column guide at 100 chars and wordwrap to 100 chars with M-q.
(setq-default fill-column 100
              whitespace-line-column 100
              whitespace-style '(face tabs lines-tail))

(global-whitespace-mode)

;; Don't use tab characters.
(setq-default indent-tabs-mode nil)

;; Prefer UTF-8 and Unix line endings.
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Remove trailing whitespace from all lines on save (but make sure there's a linebreak at the end
;; of the file).
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Match parentheses automatically.
(electric-pair-mode t)

;; Stop on 'subwords' (point will stop on capital letters in single words).
(global-subword-mode t)

;; Store backup files in /tmp/ and don't create .#-files.
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      backup-by-copying t
      create-lockfiles nil)

;; Setup sane scrolling.
(setq mouse-wheel-follow-mouse 't
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(3 ((shift) . 1)))
(setq scroll-step 1)

;; Use C++ mode in .h-files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(c-set-offset 'innamespace [0])
(setq-default c-basic-offset 2)
(setq-default css-indent-offset 2)
(setq-default js-indent-level 2)
(setq-default python-indent-offset 2)

;;;;------------------------------------
;;;; 3. Functions
;;;;------------------------------------

(defvar other-window-buffer nil)
(defun toggle-two-window-view ()
  "Toggle between displaying one frame (normal) and two frames (side-by-side)."
  (if (eq (length (window-list)) 2)
      (progn
        (setq other-window-buffer (save-window-excursion (other-window 1) (current-buffer)))
        (set-frame-size nil (/ (frame-width) 2) (frame-height))
        (delete-other-windows))
    (progn
      (set-frame-size nil (* (frame-width) 2) (frame-height))
      (split-window-right)
      (if (buffer-live-p other-window-buffer)
          (progn
            (other-window 1)
            (set-window-buffer (selected-window) other-window-buffer)
            (other-window 1)
            (progn))))))

;;;;------------------------------------
;;;; 4. Appearance
;;;;------------------------------------

;; Set up font if running in windowed mode.
(when (window-system)
  (if running-on-windows
      (set-frame-font "Consolas-10.0:antialias=subpixel")))

;; Custom bell function that inverts mode-line for a short period of time.
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line))
      visible-bell nil)

;; Don't blink the cursor.
(blink-cursor-mode -1)

;; Highlight matching parentheses.
(show-paren-mode t)

;; Disable fringes.
(fringe-mode '(0 . 0))

;;;;------------------------------------
;;;; 5. Packages
;;;;------------------------------------

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(defun packages-install (packages)
  (progn
    (unless package-archive-contents
      (package-refresh-contents))
    (dolist (it packages)
      (if (not (package-installed-p it))
          (package-install it)))))

;; Install packages from (M)ELPA.
(packages-install
 '(company
   csharp-mode
   diminish
   glsl-mode
   groovy-mode
   js2-mode
   lua-mode
   magit
   multiple-cursors
   projectile
   spacemacs-theme
   web-mode))

;;;;------------------------------------
;;;; 5. Key-bindings
;;;;------------------------------------

;; Disable C-t.
(global-set-key (kbd "C-t") nil)

;; Toggle syntax highlighting with C-z.
(global-set-key (kbd "C-z") 'global-font-lock-mode)

;; Switch between two buffers with C-b
(global-set-key (kbd "C-b") (lambda () (interactive) (switch-to-buffer nil)))

;; Shortcut to align lines by regexp.
(global-set-key (kbd "M-p") 'align-regexp)

;; Move between windows easily.
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)

;; Open current directory with File Explorer.
(if running-on-windows
    (global-set-key (kbd "M-e")
                    (lambda () (interactive)
                      (call-process "explorer" nil 0 nil "."))))

;; Toggle double frame view with C-S-<tab>.
(if (window-system)
    (global-set-key (kbd "C-S-<tab>")
                    (lambda () (interactive) (toggle-two-window-view))))

;; Backspace with C-S-d
(global-set-key (kbd "C-S-d") (lambda () (interactive) (backward-delete-char 1)))

;; Move to previous chars with C-S-f
(global-set-key (kbd "C-S-f") (lambda () (interactive) (forward-char -1)))

;; Move to previous line with C-S-n
(global-set-key (kbd "C-S-n") (lambda () (interactive) (previous-line)))

;; Scroll up with C-S-v
(global-set-key (kbd "C-S-v") (lambda () (interactive) (scroll-down-command)))

;; Iterate backwards through the kill ring with C-M-y.
(global-set-key (kbd "C-M-y") (lambda () (interactive) (yank-pop -1)))

;;;;------------------------------------
;;;; 7. Package setup
;;;;------------------------------------

;;; company
(setq company-idle-delay 0.1
      company-minimum-prefix-length 2)

(add-hook 'prog-mode-hook 'global-company-mode)

;;; diminish
(eval-after-load "abbrev"             '(diminish 'abbrev-mode))
(eval-after-load "auto-fill-function" '(diminish 'auto-fill-function))
(eval-after-load "company"            '(diminish 'company-mode))
(eval-after-load "projectile"         '(diminish 'projectile-mode))
(eval-after-load "subword"            '(diminish 'subword-mode))
(eval-after-load "whitespace"         '(diminish 'global-whitespace-mode))

(add-hook 'auto-revert-mode-hook (lambda () (interactive) (diminish 'auto-revert-mode)))

;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js2-strict-missing-semi-warning nil)

;;; multiple-cursors
(require 'multiple-cursors)

(global-set-key (kbd "M-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "M-<up>"  ) 'mc/mark-next-like-this-symbol)

;;; projectile
(require 'projectile)

;; Make sure Projectile ignores irrelevant directories.
(setq projectile-globally-ignored-directories
      (append '(".git" ".svn" ".vs" "bin" "elpa" "node_modules" "obj")
              projectile-globally-ignored-directories))

(setq projectile-globally-ignored-files
      (append '("#*#" "*.#*" "*.dll" "*.exe" "*.pyc" "*~")
              projectile-globally-ignored-files))

;; Use native (to Emacs) indexing.
(setq projectile-indexing-method 'native)

;; Use Projectile without project files.
(setq projectile-require-project-root nil)

;; Enable Projectile everywhere.
(projectile-mode)

;; Make it easy to find files with Projectile.
(global-set-key (kbd "C-p") 'projectile-find-file)

;;; spacemacs-theme

(setq spacemacs-theme-comment-bg nil)
(load-theme 'spacemacs-light t)

;; Fix for whitespace-mode.
(set-face-attribute 'whitespace-line nil :background "#fae9c3" :foreground nil)

;;; web-mode
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;;; init.el ends here
