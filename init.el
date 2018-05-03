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

;; Used to measure the time taken to initialize Emacs.
(defconst p--start-time (current-time))

;; Figure out what system we're running on.
(defconst p--is-linux   (eq system-type 'gnu/linux))
(defconst p--is-windows (eq system-type 'windows-nt))

;; Specifies the initial width and height (in number of characters) of the Emacs frame.
(defconst p--frame-width 104)
(defconst p--frame-height 58)

;; The font size (in points) to use.
(defconst p--font-size "10.0")

;; Specifies the fonts to use.  This is a list because all fonts don't contain all charactes.  The
;; font at the top will be prioritized.
(defconst p--fonts (cond (p--is-linux   '("Liberation Mono"))
                         (p--is-windows '("Consolas"
                                          "Symbola monospacified for Consolas"
                                          "SimSun"))))

;; Indentation (in number of spaces).
(defconst p--indent-offset 2)

;; Maximum preferred line width (affects word paragraph filling and `whitespace-mode', etc.).
(defconst p--line-width 100)

;; File manager to use when C-c e is pressed.
(defconst p--file-manager (cond (p--is-linux   "thunar")
                                (p--is-windows "xyplorer")))

;; Arguments to pass to the file manager when it's launched from Emacs.
(defconst p--file-manager-args '((file-name-directory buffer-file-name)))

;; Terminal to use when C-c t is pressed.
(defconst p--terminal "cmder.bat")

;; Arguments to pass to the terminal when it's launched from Emacs.
(defconst p--terminal-args '("/single" (file-name-directory buffer-file-name)))

;;;;------------------------------------
;;;; Variables.
;;;;------------------------------------

;; Used to remember the last active buffer in the other window in `p--toggle-dual-window-view'.
(defvar p--other-window-buffer nil)

;; Whether we're in presentation mode (F11-key toggles it).
(defvar p--is-presentation-mode nil)

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
  (dolist (it '(auctex
                company
                csharp-mode
                diminish
                flycheck
                glsl-mode
                groovy-mode
                hydra
                ivy
                js2-mode
                julia-mode
                lua-mode
                magit
                multiple-cursors
                omnisharp
                projectile
                rainbow-mode
                spacemacs-theme
                spaceline
                swiper
                vlf
                web-mode
                xah-math-input))
    (unless (package-installed-p it)
      (package-install it)))

  ;; Make sure to install OmniSharp server here so I don't have to do it manually.
  (omnisharp--install-server nil t))

(require 'multiple-cursors)

;; Use VLF (View Large Files) for large files so we can edit them in Emacs without hanging.
(require 'vlf-setup)

;;;;------------------------------------
;;;; Functions.
;;;;------------------------------------

(defun p--open-file-manager ()
  "Run the configured external file manager executable."
  (interactive)
  (let ((args (mapcar 'eval p--file-manager-args)))
    (apply 'call-process (append (list p--file-manager nil 0 nil) args))))

(defun p--open-terminal ()
  "Run the configured external terminal executable."
  (interactive)
  (let ((args (mapcar 'eval p--terminal-args)))
    (apply 'call-process (append (list p--terminal nil 0 nil) args))))

(defun p--set-fonts (font-names font-size)
  ;; Set up fonts if running in a window (not terminal).
  (if (window-system)
      (dolist (fontset (fontset-list))
        (dolist (font-name (reverse font-names))
          (let ((fs (font-spec :name (concat font-name "-" font-size ":antialias=subpixel"))))
            (set-fontset-font fontset 'unicode fs nil 'prepend))))))

(defun p--toggle-dual-window-view ()
  "Toggle between displaying one window (normal) and two windows (side-by-side)."
  (interactive)
  (if (eq (length (window-list)) 2)
      ;; Two windows open, so save the buffer that is open in the other window and close it, then
      ;; halve the frame width.
      (progn
        (setq p--other-window-buffer (save-window-excursion (other-window 1) (current-buffer)))
        (delete-other-windows)
        (set-frame-size nil (/ (frame-width) 2) (frame-height)))
    (progn
      ;; One (probably) window open, so double the frame width and display the last shown buffer
      ;; in the other window.
      (set-frame-size nil (* (frame-width) 2) (frame-height))
      (split-window-right)
      (when (buffer-live-p p--other-window-buffer)
        (other-window 1)
        (set-window-buffer (selected-window) p--other-window-buffer)
        (other-window 1)))))

(defun p--toggle-presentation-mode ()
  "Toggle presentation (large text and fullscreen) mode."
  (interactive)
  ;; I'm using `progn' below because fullscreen has to be toggled in a certain order to preserve
  ;; frame dimensions.
  (if p--is-presentation-mode
      (progn
        (p--set-fonts p--fonts p--font-size)
        (toggle-frame-fullscreen))
    (progn
      (toggle-frame-fullscreen)
      (p--set-fonts p--fonts "18.0")))
  (setq p--is-presentation-mode (not p--is-presentation-mode)))

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
(setq-default fill-column p--line-width)

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
(setq-default c-basic-offset                p--indent-offset
              css-indent-offset             p--indent-offset
              groovy-indent-offset          p--indent-offset
              js-indent-level               p--indent-offset
              julia-indent-offset           p--indent-offset
              python-indent-offset          p--indent-offset
              web-mode-code-indent-offset   p--indent-offset
              web-mode-css-indent-offset    p--indent-offset
              web-mode-markup-indent-offset p--indent-offset)

;; Don't indent the first level inside namespaces.
(c-set-offset 'innamespace 0)

;; Enable Flycheck for on-the-fly syntax checking.
;; Feb 16, 2018: I'm disabling `flycheck-mode' when `groovy-mode' is enabled because there seemse to
;;               be a bug in startGroovy.bat on Windows 10 causing an infinite loop, effectively
;;               hanging Flycheck. See https://github.com/flycheck/flycheck/issues/1395 for more
;;               information.
(with-eval-after-load "flycheck"
  (setq flycheck-global-modes '(not groovy-mode))
  (global-flycheck-mode))

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

;; Enable Projectile mode globally after initialization.
(add-hook 'after-init-hook 'projectile-mode)

;; Enable Company mode.
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'text-mode-hook 'company-mode)
(add-hook 'web-mode-hook  'company-mode) ;; <-- Seems `web-mode' is not a derived mode!

;; Use OmniSharp in C# buffers.
(add-hook 'csharp-mode-hook 'omnisharp-mode)

;; Use Rainbow mode when `css-mode' or `web-mode' is activated.
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)

;; Disable warnings for missing semicolons when using `js2-mode'.
(setq js2-strict-missing-semi-warning nil)

;; Don't indent on yank in `web-mode'.
(setq web-mode-enable-auto-indentation nil)

;; Automatically use VLF for large files.
(setq vlf-application 'dont-ask)

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
(setq initial-frame-alist `((width . ,p--frame-width) (height . ,p--frame-height)))

;; Set up the configured fonts.
(p--set-fonts p--fonts p--font-size)

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
(setq whitespace-line-column p--line-width
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
(with-eval-after-load "flycheck"           (diminish 'flycheck-mode)) ;; <-- Only with `spaceline'!
(with-eval-after-load "omnisharp"          (diminish 'omnisharp-mode))
(with-eval-after-load "projectile"         (diminish 'projectile-mode))
(with-eval-after-load "rainbow-mode"       (diminish 'rainbow-mode))
(with-eval-after-load "subword"            (diminish 'subword-mode))
(with-eval-after-load "whitespace"         (diminish 'whitespace-mode))
(with-eval-after-load "xah-math-input"     (diminish 'xah-math-input-mode))
;;(add-hook 'auto-revert-mode-hook '(diminish 'auto-revert-mode))

;; Load and configure the theme.
(setq spacemacs-theme-comment-bg nil)
;;      spacemacs-theme-comment-italic t
;;      spacemacs-theme-custom-colors '((comment-light . "#2aa1ae")))
(load-theme 'spacemacs-light t)

;; Fix for `whitespace-mode' when using the `spacemacs-light' theme.
(add-hook 'whitespace-mode-hook (lambda ()
                                  (set-face-attribute 'whitespace-line nil
                                                      :background "#fae9c3"
                                                      :foreground nil)))

;; I don't like how `LaTeX-mode' changes font sizes, does subscripts, etc.
(with-eval-after-load "latex"
  (setq font-latex-fontify-script nil
        font-latex-fontify-sectioning 'color))

;; Make the mode line look much better. Unforunately, this adds a significant amount of time to the
;; initialization phase. :-(
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;;;;------------------------------------
;;;; Key-bindings.
;;;;------------------------------------

;; Presentation mode.
(global-set-key (kbd "<f11>") 'p--toggle-presentation-mode)

;; Shortcut to align lines by regexp.
(global-set-key (kbd "C-c a") 'align-regexp)

;; Switch back and forth between the two last buffers.
(global-set-key (kbd "C-c b") 'mode-line-other-buffer)

;; Comment easily.
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Toggle double frame view.
(if (window-system)
    (global-set-key (kbd "C-c d") 'p--toggle-dual-window-view))

;; Open current directory with the configured file manager.
(global-set-key (kbd "C-c e") 'p--open-file-manager)

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
  ("<escape>" nil :exit t))

;; Sort lines.
(global-set-key (kbd "C-c o") 'sort-lines)

;; Open terminal in current directory.
(global-set-key (kbd "C-c t") 'p--open-terminal)

;; Easy access to Magit.
(global-set-key (kbd "C-c v") 'magit-status)

;; Move faster through text when holding the shift key.
(global-set-key (kbd "C-S-b") 'left-word)
(global-set-key (kbd "C-S-f") 'right-word)
(global-set-key (kbd "C-S-n") 'forward-paragraph)
(global-set-key (kbd "C-S-p") 'backward-paragraph)

;; Swiper is much better than i-search.
(global-set-key (kbd "C-c s") 'swiper)

;;;;------------------------------------
;;;; Finalization.
;;;;------------------------------------

(let ((elapsed (float-time (time-subtract (current-time) p--start-time))))
  (setq initial-scratch-message (format ";; Emacs initialized in %.3fs\n\n" elapsed)))

;;; init.el ends here
