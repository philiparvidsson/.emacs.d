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

;; Set initial window size (104 columns x 58 rows).
(setq initial-frame-alist '((width . 104) (height . 58)))

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

;; Increase GC threshold to run GC less often (default is 800000).
(setq gc-cons-threshold 5000000)

;; Variables used for making decisions during initialization.
(setq is-linux   (eq system-type 'gnu/linux))
(setq is-windows (eq system-type 'windows-nt))

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
;; of the file). Markdown files need to keep their trailing spaces so they're excluded.
(add-hook 'before-save-hook
          (lambda ()
            (unless (string= (file-name-extension buffer-file-name) "md")
              (delete-trailing-whitespace))))

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

;; Basic indentation stuff.
(c-set-offset 'innamespace [0])
(setq-default c-basic-offset 2)
(setq-default css-indent-offset 2)
(setq-default js-indent-level 2)
(setq-default python-indent-offset 2)

;;;;------------------------------------
;;;; 3. Functions
;;;;------------------------------------

(defun packages-install (packages)
  "Install each package specified in the list, unless it is already installed."
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (it packages)
    (unless (package-installed-p it)
      (package-install it))))

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
      (when (buffer-live-p other-window-buffer)
        (other-window 1)
        (set-window-buffer (selected-window) other-window-buffer)
        (other-window 1)))))

;;;;------------------------------------
;;;; 4. Appearance
;;;;------------------------------------

;; Set frame title.
(setq frame-title-format '("%b"))

;; Set up font if running in windowed mode.
(when (window-system)
  (if is-linux
      (set-frame-font "Liberation Mono-9.0:antialias=subpixel"))
  (if is-windows
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

;; Install packages from (M)ELPA.
(packages-install
 '(company
   company-jedi
   csharp-mode
   diminish
   flycheck
   glsl-mode
   groovy-mode
   js2-mode
   lua-mode
   magit
   multiple-cursors
   omnisharp
   projectile
   spacemacs-theme
   web-mode))

;;;;------------------------------------
;;;; 5. Key-bindings
;;;;------------------------------------

;; Shortcut to align lines by regexp.
(global-set-key (kbd "C-c a") 'align-regexp)

;; Switch between two buffers with C-c b
(global-set-key (kbd "C-c b") (lambda () (interactive) (switch-to-buffer nil)))

;; Toggle double frame view.
(if (window-system)
    (global-set-key (kbd "C-c d")
                    (lambda () (interactive) (toggle-two-window-view))))

;; Open current directory with a sensible file browser.
(if is-linux
    (global-set-key (kbd "C-c e")
                    (lambda () (interactive) (call-process "thunar" nil 0 nil "."))))
(if is-windows
    (global-set-key (kbd "C-c e")
                    (lambda () (interactive) (call-process "explorer" nil 0 nil "."))))

;; Go to line.
(global-set-key (kbd "C-c g") 'goto-line)

;; Toggle syntax highlighting.
(global-set-key (kbd "C-c h") 'global-font-lock-mode)

;; Indent region.
(global-set-key (kbd "C-c i") 'indent-region)

;; Sort lines.
(global-set-key (kbd "C-c s") 'sort-lines)

;; Delete active window.
(global-set-key (kbd "C-c w") 'delete-window)

;;;;------------------------------------
;;;; 7. Package setup
;;;;------------------------------------

;;; company
(setq company-idle-delay 0.1
      company-minimum-prefix-length 2
      company-tooltip-align-annotations t)

;;; company-jedi
(eval-after-load 'company '(add-to-list 'company-backends 'company-jedi))

(add-hook 'prog-mode-hook 'global-company-mode)

;;; diminish
(eval-after-load "abbrev"             '(diminish 'abbrev-mode))
(eval-after-load "auto-fill-function" '(diminish 'auto-fill-function))
(eval-after-load "company"            '(diminish 'company-mode))
(eval-after-load "eldoc"              '(diminish 'eldoc-mode))
(eval-after-load "flycheck"           '(diminish 'flycheck-mode))
(eval-after-load "omnisharp"          '(diminish 'omnisharp-mode))
(eval-after-load "projectile"         '(diminish 'projectile-mode))
(eval-after-load "racer"              '(diminish 'racer-mode))
(eval-after-load "subword"            '(diminish 'subword-mode))
(eval-after-load "whitespace"         '(diminish 'global-whitespace-mode))

(add-hook 'auto-revert-mode-hook (lambda () (interactive) (diminish 'auto-revert-mode)))

;;; flycheck
(global-flycheck-mode)

;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js2-strict-missing-semi-warning nil)

;;; multiple-cursors
(require 'multiple-cursors)

;; Helper functions for multiple-cursors (while remaining in multiple-cursors key-binding mode).
(defun mc-cycle-backward             () (interactive) (mc-mode) (mc/cycle-backward))
(defun mc-cycle-forward              () (interactive) (mc-mode) (mc/cycle-forward))
(defun mc-insert-numbers             () (interactive) (mc-mode) (mc/insert-numbers 1))
(defun mc-mark-all-like-this         () (interactive) (mc-mode) (mc/mark-all-dwim (use-region-p)))
(defun mc-mark-next-like-this        () (interactive) (mc-mode) (mc/mark-next-like-this 1))
(defun mc-mark-next-like-this-symbol () (interactive) (mc-mode) (mc/mark-next-like-this-symbol 1))

(defun mc-mode ()
  "Enters multiple-cursors key-binding mode for easy access."
  (interactive)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<escape>") 'ignore)
     (define-key map (kbd "a") 'mc-mark-all-like-this)
     (define-key map (kbd "b") 'mc-cycle-backward)
     (define-key map (kbd "f") 'mc-cycle-forward)
     (define-key map (kbd "i") 'mc-insert-numbers)
     (define-key map (kbd "m") 'mc-mark-next-like-this)
     (define-key map (kbd "n") 'mc-mark-next-like-this-symbol)
     map)))

;; Shortcut for entering multiple-cursors key-binding mode.
(global-set-key (kbd "C-c m") 'mc-mode)

;;; omnisharp
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(eval-after-load 'company '(add-to-list 'company-backends 'company-omnisharp))

;;; projectile
(require 'projectile)

;; Make sure Projectile ignores irrelevant directories.
(setq projectile-globally-ignored-directories
      (append '(".git" ".svn" ".vs" "bin" "Debug" "elpa" "node_modules" "obj" "Release")
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
