;;; init.el --- My personal Emacs configuration.

;;; Commentary:

;; This is my personal Emacs configuration file.
;;
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
;;;; Constants.
;;;;------------------------------------

(defconst init--start-time (current-time))

(defconst init--is-linux   (eq system-type 'gnu/linux))
(defconst init--is-windows (eq system-type 'windows-nt))

(defconst init--frame-width 104)
(defconst init--frame-height 58)

(defconst init--font (cond (init--is-linux   "Liberation Mono-9.0")
                           (init--is-windows "Consolas-10.0")))

(defconst init--indent-offset 2)
(defconst init--line-width 100)

(defconst init--file-manager (cond (init--is-linux   "thunar")
                                   (init--is-windows "explorer")))
(defconst init--file-manager-args ".")

(defconst init--terminal "Cmder")
(defconst init--terminal-args "/single")


;;;;------------------------------------
;;;; Variables.
;;;;------------------------------------

;; Used to remember the last active buffer in the other window in `init--toggle-dual-window-view'.
(defvar init--other-window-buffer nil)

;;;;------------------------------------
;;;; Initialization.
;;;;------------------------------------

;; Disable GC during initialization and then enable it again when we're done.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook '(lambda () (setq gc-cons-threshold 2000000)))

;; Separate 'custom.el' file (otherwise it will be appended to this file).
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Prefer UTF-8 and Unix line endings.
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;;;;------------------------------------
;;;; Packages.
;;;;------------------------------------

(require 'package)
(package-initialize)

;; Make sure we have downloaded the package archive metadata and installed all packages we need.
(unless package-archive-contents
  ;; Enable MELPA package repostiory.
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-refresh-contents)

  ;; Install packages from (M)ELPA.
  (dolist (it '(company
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
                spaceline
                spacemacs-theme
                web-mode))
    (unless (package-installed-p it)
      (package-install it))))

;;;;------------------------------------
;;;; User interface.
;;;;------------------------------------

;; Disable GUI stuff that I don't want or need.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; Disable fringes.
(fringe-mode 0)

;; Set initial frame size.
(setq initial-frame-alist `((width . ,init--frame-width) (height . ,init--frame-height)))

;; Set up font if running in a window (not terminal).
(if (window-system)
    (set-frame-font (concat init--font ":antialias=subpixel")))

;; Set frame title.
(setq frame-title-format '("%b"))

;; No startup message or welcome screen.
(setq inhibit-startup-message t)

;; Don't blink the cursor.
(blink-cursor-mode -1)

;; Display keyboard shortcuts quickly in the echo area.
(setq echo-keystrokes 0.1)

;; Custom bell function that inverts the mode line for a short period of time, making it flash once.
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Show column- and line numbers in mode line, as well as file size.
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

;; Highlight long lines.
(setq-default whitespace-line-column init--line-width
              whitespace-style '(face tabs lines-tail))
(global-whitespace-mode t)

;; Highlight matching parentheses.
(show-paren-mode t)

;; Hide some modes from the mode line - I don't need to see them.
(with-eval-after-load "abbrev"             (diminish 'abbrev-mode))
(with-eval-after-load "auto-fill-function" (diminish 'auto-fill-function))
(with-eval-after-load "company"            (diminish 'company-mode))
(with-eval-after-load "eldoc"              (diminish 'eldoc-mode))
(with-eval-after-load "flycheck"           (diminish 'flycheck-mode))
(with-eval-after-load "omnisharp"          (diminish 'omnisharp-mode))
(with-eval-after-load "projectile"         (diminish 'projectile-mode))
(with-eval-after-load "subword"            (diminish 'subword-mode))
(with-eval-after-load "whitespace"         (diminish 'global-whitespace-mode))
;;(add-hook 'auto-revert-mode-hook '(diminish 'auto-revert-mode))

;; Load and configure the theme.
(setq spacemacs-theme-comment-bg nil)
(load-theme 'spacemacs-light t)

;; Fix for `whitespace-mode' when using the `spacemacs-light' theme.
(set-face-attribute 'whitespace-line nil :background "#fae9c3" :foreground nil)

;; Fancy mode line.
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;;;;------------------------------------
;;;; Behavior.
;;;;------------------------------------

;; Don't require full yes or full no, just y or n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatically reload files when changes are detected.
(global-auto-revert-mode t)

;; Use C-<space> to set mark and select text instead of shift.
(setq shift-select-mode nil)

;; Delete selection (if any) when typing.
(delete-selection-mode t)

;; Word wrap long lines.
(setq-default fill-column init--line-width)

;; Don't use tab characters.
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespace from all lines on save (but make sure there's a linebreak at the end
;; of the file).  Markdown files need to keep their trailing spaces so they're excluded.
(add-hook 'before-save-hook
          '(lambda ()
             (unless (string= (file-name-extension buffer-file-name) "md")
               (delete-trailing-whitespace))))
(setq require-final-newline t)

;; Match parentheses automatically.
(electric-pair-mode t)

;; Stop on 'subwords' (point will stop on capital letters in single words).
(global-subword-mode t)

;; Store backup files in the temp directory and don't create .#-files.
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      backup-by-copying t
      create-lockfiles nil)

;; Set up sane scrolling.
(setq mouse-wheel-follow-mouse t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(3 ((shift) . 1)))
(setq scroll-step 1)

;; Mode mappings.
(dolist (it '(("\\.h\\'"   . c++-mode)  ; <-- Emacs will use `c-mode' in .h-files without this.
              ("\\.js\\'"  . js2-mode)
              ("\\.jsx\\'" . web-mode)
              ("\\.ts\\'"  . web-mode)
              ("\\.tsx\\'" . web-mode)))
  (add-to-list 'auto-mode-alist it))

;; Indentation stuff.
(setq-default c-basic-offset                init--indent-offset
              css-indent-offset             init--indent-offset
              groovy-indent-offset          init--indent-offset
              js-indent-level               init--indent-offset
              python-indent-offset          init--indent-offset
              web-mode-code-indent-offset   init--indent-offset
              web-mode-css-indent-offset    init--indent-offset
              web-mode-markup-indent-offset init--indent-offset)

(c-set-offset 'innamespace [0])

;; Set up Company.
(with-eval-after-load "company"
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2)
  (add-to-list 'company-backends 'company-omnisharp))

;; Enable Flycheck for on-the-fly syntax checking.
(with-eval-after-load "flycheck" (global-flycheck-mode))

;; Set up Projectile.
(with-eval-after-load "projectile"
  ;; Make sure Projectile ignores irrelevant directories.
  (setq projectile-globally-ignored-directories
        (append '(".git" ".svn" ".vs" "bin" "Debug" "elpa" "node_modules" "obj" "Release")
                projectile-globally-ignored-directories)

        projectile-globally-ignored-files
        (append '("#*#" "*.#*" "*.dll" "*.exe" "*.pyc" "*~")
                projectile-globally-ignored-files)

        ;; Use native (to Emacs) indexing.
        projectile-indexing-method 'alien

        ;; Use Projectile without project files.n
        projectile-require-project-root nil))

;; Enable Company and Projectile when any `prog-mode' is activated.
(add-hook 'prog-mode-hook
          '(lambda ()
             (company-mode)
             (projectile-mode)))

;; Use OmniSharp in C# buffers.
(add-hook 'csharp-mode-hook 'omnisharp-mode)

;;;;------------------------------------
;;;; Functions.
;;;;------------------------------------

(defun init--open-file-manager ()
  "Run the configured external file manager executable."
  (interactive)
  (call-process init--file-manager nil 0 nil init--file-manager-args))

(defun init--open-terminal ()
  "Run the configured external terminal executable."
  (interactive)
  (call-process init--terminal nil 0 nil init--terminal-args))

(defun init--toggle-dual-window-view ()
  "Toggle between displaying one window (normal) and two windows (side-by-side)."
  (interactive)
  (if (eq (length (window-list)) 2)
      ;; Two windows open, so save the buffer that is open in the other window and close it, then
      ;; halve the frame width.
      (progn
        (setq init--other-window-buffer (save-window-excursion (other-window 1) (current-buffer)))
        (delete-other-windows)
        (set-frame-size nil (/ (frame-width) 2) (frame-height)))
    (progn
      ;; One (probably) window open, so double the frame width and display the last shown buffer
      ;; in the other window.
      (set-frame-size nil (* (frame-width) 2) (frame-height))
      (split-window-right)
      (when (buffer-live-p init--other-window-buffer)
        (other-window 1)
        (set-window-buffer (selected-window) init--other-window-buffer)
        (other-window 1)))))

(defun err-mode ()
  "Enter error-cycling key-binding mode for cycling between errors."
  (interactive)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<escape>") (lambda () (interactive) (delete-other-windows-vertically)))
     (define-key map (kbd "e") (lambda () (interactive) (err-mode) (previous-error)))
     (define-key map (kbd "r") (lambda () (interactive) (err-mode) (next-error)))
     map)))

;; Helper functions for multiple-cursors (while remaining in multiple-cursors key-binding mode).
(defun mc-cycle-backward             () (interactive) (mc-mode) (mc/cycle-backward))
(defun mc-cycle-forward              () (interactive) (mc-mode) (mc/cycle-forward))
(defun mc-insert-numbers             () (interactive) (mc-mode) (mc/insert-numbers 1))
(defun mc-mark-all-like-this         () (interactive) (mc-mode) (mc/mark-all-dwim (use-region-p)))
(defun mc-mark-next-like-this        () (interactive) (mc-mode) (mc/mark-next-like-this 1))
(defun mc-mark-next-like-this-symbol () (interactive) (mc-mode) (mc/mark-next-like-this-symbol 1))

(defun mc-mode ()
  "Enter multiple-cursors key-binding mode for easy access."
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

;;;;------------------------------------
;;;; Key-bindings.
;;;;------------------------------------

;; Shortcut to align lines by regexp.
(global-set-key (kbd "C-c a") 'align-regexp)

;; Switch back and forth between the two last buffers.
(global-set-key (kbd "C-c b") 'mode-line-other-buffer)

;; Comment easily.
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Toggle double frame view.
(if (window-system)
    (global-set-key (kbd "C-c d") 'init--toggle-dual-window-view))

;; Open current directory with the configured file manager.
(global-set-key (kbd "C-c e") 'init--open-file-manager)

;; Move point to a line quickly.
(global-set-key (kbd "C-c g") 'goto-line)

;; Toggle syntax highlighting.
(global-set-key (kbd "C-c h") 'global-font-lock-mode)

;; Indent region.
(global-set-key (kbd "C-c i") 'indent-region)

;; Shortcut for entering multiple-cursors key-binding mode.
(global-set-key (kbd "C-c m") 'mc-mode)

;; Close automatically opened window (from, e.g., search).
(global-set-key (kbd "C-c q") 'delete-other-windows-vertically)

;; Enter error cycling mode.
(global-set-key (kbd "C-c r") 'err-mode)

;; Sort lines.
(global-set-key (kbd "C-c s") 'sort-lines)

;; Open terminal in current directory.
(global-set-key (kbd "C-c t") 'init--open-terminal)

;; Delete active window.
(global-set-key (kbd "C-c w") 'delete-window)

;;;;------------------------------------
;;;; Finalization.
;;;;------------------------------------

(let ((elapsed (float-time (time-subtract (current-time) init--start-time))))
  (setq initial-scratch-message (format ";; Emacs initialized in %.3fs\n\n" elapsed)))

;;; init.el ends here
