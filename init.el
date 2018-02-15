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

(defconst init--fonts (cond (init--is-linux   '("Liberation Mono-9.0"))
                            (init--is-windows '("Consolas-10.0"
                                                "Symbola monospacified for Consolas-10.0"
                                                "SimSun-10.0"))))

(defconst init--indent-offset 2)
(defconst init--line-width 100)

(defconst init--file-manager (cond (init--is-linux   "thunar")
                                   (init--is-windows "xyplorer")))
(defconst init--file-manager-args '((file-name-directory buffer-file-name)))

(defconst init--terminal "Cmder")
(defconst init--terminal-args '("/single"))

;;;;------------------------------------
;;;; Variables.
;;;;------------------------------------

;; Used to remember the last active buffer in the other window in `init--toggle-dual-window-view'.
(defvar init--other-window-buffer nil)

;;;;------------------------------------
;;;; Initialization.
;;;;------------------------------------

;; Disable GC during initialization and then enable it again when we're done. Speeds up
;; initialization somewhat.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 2000000)))

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
                hydra
                ivy
                js2-mode
                lua-mode
                magit
                multiple-cursors
                omnisharp
                projectile
                rainbow-mode
                spacemacs-theme
                swiper
                web-mode
                xah-math-input))
    (unless (package-installed-p it)
      (package-install it))))

(require 'multiple-cursors)

;;;;------------------------------------
;;;; Functions.
;;;;------------------------------------

(defun init--open-file-manager ()
  "Run the configured external file manager executable."
  (interactive)
  (let ((args (mapcar 'eval init--file-manager-args)))
    (apply 'call-process (append (list init--file-manager nil 0 nil) args))))

(defun init--open-terminal ()
  "Run the configured external terminal executable."
  (interactive)
  (let ((args (mapcar 'eval init--terminal-args)))
    (apply 'call-process (append (list init--terminal nil 0 nil) args))))

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

;; When doing `kill-line', also remove whitespace from the beginning of the next line.
(advice-add 'kill-line :after
            (lambda (&optional arg)
              (if (not (bolp))
                  (delete-region (point) (progn (skip-chars-forward " \t") (point))))))

;; Word wrap long lines.
(setq-default fill-column init--line-width)

;; Don't use tab characters.
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespace from all lines on save (but make sure there's a linebreak at the end
;; of the file).  Markdown files need to keep their trailing spaces so they're excluded.
(add-hook 'before-save-hook
          (lambda ()
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

;; Don't indent the first level inside namespaces.
(c-set-offset 'innamespace [0])

;; Enable Flycheck for on-the-fly syntax checking.
(with-eval-after-load "flycheck" (global-flycheck-mode))

;; Set up Company for auto-completion when typing.
(with-eval-after-load "company"
  (setq company-dabbrev-downcase nil
        company-idle-delay 0.1
        company-minimum-prefix-length 2)
  (add-to-list 'company-backends 'company-omnisharp))

;; Set up Projectile for easy finding of files in the same project.
(with-eval-after-load "projectile"
  (setq
   ;; Use Ivy for finding files in Projectiles.
   projectile-completion-system 'ivy

   ;; Make sure Projectile ignores irrelevant directories.
   projectile-globally-ignored-directories
   (append '(".git" ".svn" ".vs" "bin" "Debug" "elpa" "node_modules" "obj" "Release")
           projectile-globally-ignored-directories)

   projectile-globally-ignored-files
   (append '("#*#" "*.#*" "*.dll" "*.exe" "*.pyc" "*~")
           projectile-globally-ignored-files)

   ;; Enable Projectile to be used anywhere (even without project files).
   projectile-require-project-root nil))

;; Enable Company and Projectile when any `prog-mode' is activated.
(add-hook 'prog-mode-hook (lambda () (company-mode) (projectile-mode)))

;; Use OmniSharp in C# buffers.
(add-hook 'csharp-mode-hook 'omnisharp-mode)

;; Use Rainbow mode when `css-mode' or `web-mode' is activated.
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)

;; Disable warnings for missing semicolons when using `js2-mode'.
(setq js2-strict-missing-semi-warning nil)

;; Don't indent on yank in `web-mode'.
(setq web-mode-enable-auto-indentation nil)

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

;; Set up fonts if running in a window (not terminal).
(if (window-system)
    (dolist (fontset (fontset-list))
      (dolist (font-name (reverse init--fonts))
        (let ((fs (font-spec :name (concat font-name ":antialias=subpixel"))))
          (set-fontset-font fontset 'unicode fs nil 'prepend)))))

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
(setq whitespace-line-column init--line-width
      whitespace-style '(face tabs lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'web-mode-hook 'whitespace-mode) ;; <-- Unsure why this is needed!

;; Highlight matching parentheses.
(show-paren-mode t)

;; Hide some modes from the mode line - I don't need to see them.
(with-eval-after-load "abbrev"             (diminish 'abbrev-mode))
(with-eval-after-load "auto-fill-function" (diminish 'auto-fill-function))
(with-eval-after-load "company"            (diminish 'company-mode))
(with-eval-after-load "eldoc"              (diminish 'eldoc-mode))
(with-eval-after-load "omnisharp"          (diminish 'omnisharp-mode))
(with-eval-after-load "projectile"         (diminish 'projectile-mode))
(with-eval-after-load "rainbow-mode"       (diminish 'rainbow-mode))
(with-eval-after-load "subword"            (diminish 'subword-mode))
(with-eval-after-load "whitespace"         (diminish 'whitespace-mode))
(with-eval-after-load "xah-math-input"     (diminish 'xah-math-input-mode))
;;(add-hook 'auto-revert-mode-hook '(diminish 'auto-revert-mode))

;; Load and configure the theme.
(setq spacemacs-theme-comment-bg nil)
(load-theme 'spacemacs-light t)

;; Fix for `whitespace-mode' when using the `spacemacs-light' theme.
(add-hook 'whitespace-mode-hook (lambda ()
                                  (set-face-attribute 'whitespace-line nil
                                                      :background "#fae9c3"
                                                      :foreground nil)))

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
  ("<escape>" ignore :exit t))

;; Sort lines.
(global-set-key (kbd "C-c s") 'sort-lines)

;; Open terminal in current directory.
(global-set-key (kbd "C-c t") 'init--open-terminal)

;; Easy access to Magit.
(global-set-key (kbd "C-c v") 'magit-status)

;; Move faster through text when holding the shift key.
(global-set-key (kbd "C-S-b") 'left-word)
(global-set-key (kbd "C-S-f") 'right-word)
(global-set-key (kbd "C-S-n") 'forward-paragraph)
(global-set-key (kbd "C-S-p") 'backward-paragraph)

;; Swiper is much better than i-search.
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-S-s") 'swiper-all)

;;;;------------------------------------
;;;; Finalization.
;;;;------------------------------------

(let ((elapsed (float-time (time-subtract (current-time) init--start-time))))
  (setq initial-scratch-message (format ";; Emacs initialized in %.3fs\n\n" elapsed)))

;;; init.el ends here
