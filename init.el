;;; init.el --- My personal Emacs configuration.

;;; Commentary:

;; This is my personal Emacs configuration file.
;;
;; Author: Philip Arvidsson <hello@philiparvidsson.com>
;; URL: https://github.com/philiparvidsson/My-Emacs-Config

;;; License:

;;
;; Copyright 2019 Philip Arvidsson
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
(defconst my-start-time (current-time))

;; Figure out what system we're running on.
(defconst my-is-linux   (eq system-type 'gnu/linux))
(defconst my-is-windows (eq system-type 'windows-nt))

;; Specifies the initial width and height (in number of characters) of the Emacs frame.
(defconst my-frame-width 106)
(defconst my-frame-height (if (string= (system-name) "PHILIP-XPS") 28 48))

;; The font size (in points) to use.
(defconst my-font-size "12.0")

;; Specifies the fonts to use.  This is a list because all fonts don't contain all charactes.  The
;; font at the top will be prioritized.
(defconst my-fonts (cond (my-is-linux   '("DejaVu Sans Mono"))
                         (my-is-windows '("Consolas"))))

;; Indentation (in number of spaces).
(defconst my-indent-offset 4)

;; Maximum preferred line width (affects word paragraph filling and `whitespace-mode', etc.).
(defconst my-line-width 100)

;; File manager to use when C-c e is pressed.
(defconst my-file-manager (cond (my-is-linux   "thunar")
                                (my-is-windows "xyplorer")))

;; Arguments to pass to the file manager when it's launched from Emacs.
(defconst my-file-manager-args '((file-name-directory buffer-file-name)))

;; Terminal to use when C-c t is pressed.
(defconst my-terminal "cmder.bat")

;; Arguments to pass to the terminal when it's launched from Emacs.
(defconst my-terminal-args '("/single" (file-name-directory buffer-file-name)))

;;;;------------------------------------
;;;; Variables.
;;;;------------------------------------

;; Used to remember the last active buffer in the other window in `my-toggle-dual-window-view'.
(defvar my-other-window-buffer nil)

;; Used to remember the last evaluated line in `ess-mode'.
(defvar my-last-ess-eval "")

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
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; Because I use Dropbox...
(setq server-name (system-name))

;;;;------------------------------------
;;;; Packages.
;;;;------------------------------------

(require 'package)
;;(setq package-user-dir "C:/Users/Philip/.emacs.d/packages")
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
                doom-modeline
                doom-themes
                elm-mode
                ess
                flycheck
                fsharp-mode
                glsl-mode
                groovy-mode
                hy-mode
                hydra
                ivy
                js2-mode
                lua-mode
                magit
                markdown-mode
                multiple-cursors
                neotree
                omnisharp
                phi-search
                projectile
                rainbow-mode
                spacemacs-theme
                swiper
                tide
                vlf
                web-mode
                xah-math-input))
    (unless (package-installed-p it)
      (package-install it)))

  ;; Make sure to install OmniSharp server here so I don't have to do it manually.
  (omnisharp--install-server nil t))

;; (require 'unicode-fonts)
;; (unicode-fonts-setup)

;; Hy-mode won't auto-load on .hy-files without this.
(require 'hy-mode)

;; The multiple cursor keybindings won't work without this.
(require 'multiple-cursors)

;; The  C-<return> key-binding won't work without this.
(require 'xah-math-input)

;; Use VLF (View Large Files) for large files so we can edit them in Emacs without hanging.
(require 'vlf-setup)

;;;;------------------------------------
;;;; Functions.
;;;;------------------------------------

;; https://www.emacswiki.org/emacs/NeoTree#toc11
(defun my-neotree-project-dir ()
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

(defun my-kill-other-buffers (&optional arg)
  "Kill all buffers except the current one (and *scratch*).  If ARG is non-nil, kill all buffers."
  (interactive "P")
  (let* ((scratch-buffer (get-buffer "*scratch*"))
         (all-buffers-except-scratch (delq scratch-buffer (buffer-list))))
    (if arg
        (mapc 'kill-buffer all-buffers-except-scratch)
      (mapc 'kill-buffer (delq (current-buffer) all-buffers-except-scratch)))))

;; Patch for `ess-mode'. See https://github.com/emacs-ess/ESS/issues/620 for more information.
(defun my-ess-eval-line-and-newline (&optional vis)
  "Evaluate the current line using ESS.  If VIS is non-nil, show output in ESS terminal."
  (interactive "P")
  (setq my-last-ess-eval (thing-at-point 'line))
  (ess-eval-line vis)
  (if (search-forward "\n" nil t)
      (progn
        (backward-char)
        (ess-next-code-line 1))
    (progn
      (search-forward "\n" nil 1)
      (ess-newline-and-indent))))

(defun my-ess-eval-last (&optional arg)
  "Evaluate last evaluated command using ESS."
  (interactive)
  (when (not (string= my-last-ess-eval ""))
    (ess-eval-linewise my-last-ess-eval nil)
    (message my-last-ess-eval)))

(defun my-open-file-manager ()
  "Run the configured external file manager executable."
  (interactive)
  (let ((args (mapcar 'eval my-file-manager-args)))
    (apply 'call-process (append (list my-file-manager nil 0 nil) args))))

(defun my-open-terminal ()
  "Run the configured external terminal executable."
  (interactive)
  (let ((args (mapcar 'eval my-terminal-args)))
    (apply 'call-process (append (list my-terminal nil 0 nil) args))))

(defun my-set-fonts (font-names font-size)
  "Set up Emacs to use the fonts specified in FONT-NAMES, using the specified FONT-SIZE."
  ;; Set up fonts if running in a window (not terminal).
  (if (display-graphic-p)
      (dolist (fontset (fontset-list))
        (dolist (font-name (reverse font-names))
          (let ((fs (font-spec :name (concat font-name "-" font-size ":antialias=subpixel"))))
            (set-fontset-font fontset 'unicode fs nil 'prepend))))))

(defun my-tide-mode ()
  "Set up Tide mode in Emacs."
  (web-mode)
  (tide-setup)
  (eldoc-mode))

(defun my-toggle-dual-window-view ()
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

(defun my-toggle-presentation-mode ()
  "Toggle presentation (large text and fullscreen) mode."
  (interactive)
  ;; I'm using `progn' below because fullscreen has to be toggled in a certain order to preserve
  ;; frame dimensions.
  (let ((fullscreen (frame-parameter nil 'fullscreen)))
    (if (memq fullscreen '(fullscreen fullboth))
        (progn
          (my-set-fonts my-fonts my-font-size)
          (toggle-frame-fullscreen))
      (progn
        (toggle-frame-fullscreen)
        (my-set-fonts my-fonts "18.0")))))

(defun my-setup-ess-mode ()
  "Set up `ess-mode'."
  (local-set-key (kbd "C-<return>") 'my-ess-eval-line-and-newline)
  (local-set-key (kbd "C-M-<return>") 'my-ess-eval-last)
  (setq ess-ask-for-ess-directory nil
        ess-fancy-comments        nil
        ess-indent-offset         my-indent-offset
        inferior-R-args           "--no-save --quiet"))

(defun my-setup-prettify-symbols-mode ()
  "Set up `prettify-symbols-mode'."
  (setq prettify-symbols-unprettify-at-point t)
  (mapc (lambda (it)
          (push it prettify-symbols-alist))
          '(("!!"      .    "‼")
            ("!="      .    "≠")
            ("&&"      .    "∧")
            ("->"      .    "→")
            ("/="      .    "≠")
            ("<-"      .    "←")
            ("<<"      .    "≪")
            ("<<<"     .    "⋘")
            ("<="      .    "≤")
            ("<=>"     .    "⇔")
            ("<>"      .    "◇")
            ("<|"      .    "◁")
            ("<~"      .    "⇜")
            ("<~>"     .    "↭")
            ("<~~"     .    "⬳")
            ("=="      .    "＝")
            ("==="     .    "≡")
            ("=>"      .    "⇒")
            (">="      .    "≥")
            (">>"      .    "≫")
            (">>>"     .    "⋙")
            ("Alpha"   .    "Α")
            ("Beta"    .    "Β")
            ("Chi"     .    "Χ")
            ("Delta"   .    "Δ")
            ("Epsilon" .    "Ε")
            ("Eta"     .    "Η")
            ("Gamma"   .    "Γ")
            ("Iota"    .    "Ι")
            ("Kappa"   .    "Κ")
            ("Lambda"  .    "Λ")
            ("Mu"      .    "Μ")
            ("Nu"      .    "Ν")
            ("Omega"   .    "Ω")
            ("Omicron" .    "Ο")
            ("Phi"     .    "Φ")
            ("Pi"      .    "Π")
            ("Psi"     .    "Ψ")
            ("Rho"     .    "Ρ")
            ("Sigma"   .    "Σ")
            ("Tau"     .    "Τ")
            ("Theta"   .    "Θ")
            ("Upsilon" .    "Υ")
            ("Xi"      .    "Ξ")
            ("Zeta"    .    "Ζ")
            ("_|_"     .    "⊥")
            ("alpha"   .    "α")
            ("beta"    .    "β")
            ("chi"     .    "χ")
            ("delta"   .    "γ")
            ("epsilon" .    "ε")
            ("eta"     .    "η")
            ("gamma"   .    "γ")
            ("iota"    .    "ι")
            ("kappa"   .    "κ")
            ("lambda"  .    "λ")
            ("mu"      .    "μ")
            ("nu"      .    "ν")
            ("omega"   .    "ω")
            ("omicron" .    "ο")
            ("phi"     .    "φ")
            ("pi"      .    "π")
            ("psi"     .    "ψ")
            ("rho"     .    "ρ")
            ("sigma"   .    "σ")
            ("tau"     .    "τ")
            ("theta"   .    "θ")
            ("upsilon" .    "υ")
            ("xi"      .    "ξ")
            ("zeta"    .    "ζ")
            ("|->"     .    "↦")
            ("|>"      .    "▷")
            ("||"      .    "∨")
            ("~>"      .    "⇝")
            ("~~>"     .    "⟿")

            ;; ("!!!"    .    "!!!")
            ;; ("!=<"    .    "!=<")
            ;; ("!=="    .    "!==")
            ;; ("!>"     .    "!>")
            ;; ("!≡"     .    "!≡")
            ;; ("!≡≡"    .    "!≡≡")
            ;; ("##"     .    "##")
            ;; ("#("     .    "#(")
            ;; ("#>"     .    "#>")
            ;; ("#?"     .    "#?")
            ;; ("#_"     .    "#_")
            ;; ("#_("    .    "#_(")
            ;; ("#{"     .    "#{")
            ;; ("$>"     .    "$>")
            ;; ("%<%"    .    "%<%")
            ;; ("%="     .    "%=")
            ;; ("%>"     .    "%>")
            ;; ("%>%"    .    "%>%")
            ;; ("&%"     .    "&%")
            ;; ("&&&"    .    "&&&")
            ;; ("&*"     .    "&*")
            ;; ("&+"     .    "&+")
            ;; ("&-"     .    "&-")
            ;; ("&/"     .    "&/")
            ;; ("&="     .    "&=")
            ;; ("&>"     .    "&>")
            ;; ("***"    .    "***")
            ;; ("*/"     .    "*/")
            ;; ("*="     .    "*=")
            ;; ("*>"     .    "*>")
            ;; ("++"     .    "++")
            ;; ("+++"    .    "+++")
            ;; ("++="    .    "++=")
            ;; ("+="     .    "+=")
            ;; ("+>"     .    "+>")
            ;; ("-+-"    .    "-+-")
            ;; ("--"     .    "–")
            ;; ("---"    .    "—")
            ;; ("-->"    .    "-->")
            ;; ("-<"     .    "-<")
            ;; ("-<<"    .    "-<<")
            ;; ("-<|"    .    "-<|")
            ;; ("-="     .    "-=")
            ;; ("->>"    .    "->>")
            ;; ("-\\/"   .    "-\\/")
            ;; ("-|>"    .    "-|>")
            ;; (".."     .    "..")
            ;; ("..."    .    "...")
            ;; ("..<"    .    "..<")
            ;; (".="     .    ".=")
            ;; (".>"     .    ".>")
            ;; (".~"     .    ".~")
            ;; ("/*"     .    "/*")
            ;; ("/**"    .    "/**")
            ;; ("//"     .    "//")
            ;; ("///"    .    "///")
            ;; ("/=="    .    "/==")
            ;; ("/>"     .    "/>")
            ;; (":("     .    ":(")
            ;; (":)"     .    ":)")
            ;; (":-("    .    ":-(")
            ;; (":-)"    .    ":-)")
            ;; (":/"     .    ":/")
            ;; (":3"     .    ":3")
            ;; ("::"     .    "::")
            ;; (":::"    .    ":::")
            ;; (":<:"    .    ":<:")
            ;; (":="     .    ":=")
            ;; (":=>"    .    ":=>")
            ;; (":>"     .    ":>")
            ;; (":>:"    .    ":>:")
            ;; (":D"     .    ":D")
            ;; (":P"     .    ":P")
            ;; (":\\"    .    ":\\")
            ;; (":≡"     .    ":≡")
            ;; ("<!"     .    "<!")
            ;; ("<!--"   .    "<!--")
            ;; ("<!>"    .    "<!>")
            ;; ("<#"     .    "<#")
            ;; ("<#>"    .    "<#>")
            ;; ("<$"     .    "<$")
            ;; ("<$>"    .    "<$>")
            ;; ("<%"     .    "<%")
            ;; ("<%>"    .    "<%>")
            ;; ("<&"     .    "<&")
            ;; ("<&>"    .    "<&>")
            ;; ("<*"     .    "<*")
            ;; ("<**>"   .    "<**>")
            ;; ("<*>"    .    "<*>")
            ;; ("<+"     .    "<+")
            ;; ("<+>"    .    "<+>")
            ;; ("<--"    .    "<--")
            ;; ("<-->"   .    "<-->")
            ;; ("<-<"    .    "<-<")
            ;; ("<->"    .    "<->")
            ;; ("<."     .    "<.")
            ;; ("<.>"    .    "<.>")
            ;; ("</"     .    "</")
            ;; ("</>"    .    "</>")
            ;; ("<:"     .    "<:")
            ;; ("<:>"    .    "<:>")
            ;; ("<<-"    .    "<<-")
            ;; ("<<="    .    "<<=")
            ;; ("<<=="   .    "<<==")
            ;; ("<<^"    .    "<<^")
            ;; ("<<|"    .    "<<|")
            ;; ("<<~"    .    "<<~")
            ;; ("<=<"    .    "<=<")
            ;; ("<=="    .    "<==")
            ;; ("<==>"   .    "<==>")
            ;; ("<?"     .    "<?")
            ;; ("<?>"    .    "<?>")
            ;; ("<@"     .    "<@")
            ;; ("<@>"    .    "<@>")
            ;; ("<\""    .    "<\"")
            ;; ("<\">"   .    "<\">")
            ;; ("<\\"    .    "<\\")
            ;; ("<\\>"   .    "<\\>")
            ;; ("<^"     .    "<^")
            ;; ("<^>"    .    "<^>")
            ;; ("<|-"    .    "<|-")
            ;; ("<|>"    .    "<|>")
            ;; ("<~<"    .    "<~<")
            ;; ("=/="    .    "=/=")
            ;; ("==<"    .    "==<")
            ;; ("==>"    .    "==>")
            ;; ("==>>"   .    "==>>")
            ;; ("=>>"    .    "=>>")
            ;; ("=~"     .    "=~")
            ;; ("=~="    .    "=~=")
            ;; (">!="    .    ">!=")
            ;; (">-"     .    ">-")
            ;; (">->"    .    ">->")
            ;; (">=="    .    ">==")
            ;; (">=>"    .    ">=>")
            ;; (">>-"    .    ">>-")
            ;; (">>^"    .    ">>^")
            ;; (">>|"    .    ">>|")
            ;; ("?."     .    "?.")
            ;; ("?="     .    "?=")
            ;; ("?>"     .    "?>")
            ;; ("??"     .    "??")
            ;; ("???"    .    "???")
            ;; ("?~"     .    "?~")
            ;; ("@>"     .    "@>")
            ;; ("[["     .    "[[")
            ;; ("\">"    .    "\">")
            ;; ("\\/-"   .    "\\/-")
            ;; ("\\>"    .    "\\>")
            ;; ("\\\\"   .    "\\\\")
            ;; ("]]"     .    "]]")
            ;; ("^."     .    "^.")
            ;; ("^.."    .    "^..")
            ;; ("^<<"    .    "^<<")
            ;; ("^="     .    "^=")
            ;; ("^>"     .    "^>")
            ;; ("^>>"    .    "^>>")
            ;; ("^?"     .    "^?")
            ;; ("fun"    .    "λ")
            ;; ("|+|"    .    "|+|")
            ;; ("|-"     .    "|-")
            ;; ("|-->"   .    "|-->")
            ;; ("|<<"    .    "|<<")
            ;; ("|="     .    "|=")
            ;; ("|==>"   .    "|==>")
            ;; ("|=>"    .    "|=>")
            ;; ("|>-"    .    "|>-")
            ;; ("|>>"    .    "|>>")
            ;; ("||-"    .    "||-")
            ;; ("||>"    .    "||>")
            ;; ("|||"    .    "|||")
            ;; ("~="     .    "~=")
            ;; ("~>>"    .    "~>>")
            ;; ("≡:≡"    .    "≡:≡")
            ;; ("≡≡"     .    "≡≡")
            ;; ("≡≡≡"    .    "≡≡≡")
            ))
  (prettify-symbols-mode))

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
(setq-default fill-column my-line-width)

;; Don't use tab characters.
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespace from all lines on save (but make sure there's a linebreak at the end
;; of the file).  Markdown files need to keep their trailing spaces so they're excluded.
(add-hook 'before-save-hook (lambda ()
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
(dolist (it '(("\\.h\\'"    . c++-mode)  ; <-- Emacs will use `c-mode' in .h-files without this.
              ("\\.jsx?\\'" . js2-mode)
              ("\\.tsx?\\'" . my-tide-mode)))
  (add-to-list 'auto-mode-alist it))

;; Indentation stuff.
(setq-default c-basic-offset                my-indent-offset
              css-indent-offset             my-indent-offset
              ess-indent-offset             my-indent-offset
              groovy-indent-offset          my-indent-offset
              js-indent-level               my-indent-offset
              python-indent-offset          my-indent-offset
              web-mode-code-indent-offset   my-indent-offset
              web-mode-css-indent-offset    my-indent-offset
              web-mode-markup-indent-offset my-indent-offset)

;; Don't indent the first level inside namespaces.
(c-set-offset 'innamespace 0)

;; Enable Flycheck for on-the-fly syntax checking.
;; Feb 16, 2018: I'm disabling `flycheck-mode' when `groovy-mode' is enabled because there seems to
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

   ;; Native indexing seems to not respect globally ignored files or directories...
   projectile-indexing-method 'alien

   ;; Make sure Projectile ignores irrelevant directories.
   projectile-globally-ignored-directories
   (append '("$tf" ".git" ".gradle" ".svn" ".vs" ".vscode" "bin" "Debug" "dist" "elpa"
             "gradle" "node_modules" "obj" "Release")
           projectile-globally-ignored-directories)

   projectile-globally-ignored-files
   (append '("#*#" "*.#*" "*.dll" "*.exe" "*.mp4" "*.pyc" "*~" "gradlew" "gradlew.bat")
           projectile-globally-ignored-files)

   ;; Enable Projectile to be used anywhere (even without project files).
   projectile-require-project-root nil)

  ;; Projectile removed its default key-bindings, so setting them up manually here.
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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

;; Set up ESS to be sane when loaded.
(add-hook 'ess-mode-hook 'my-setup-ess-mode)

;; Make Phi Search work in a more intuitive manner.
(setq-default phi-search-case-sensitive 'guess)

;; Set up pretty symbols.
(add-hook 'prog-mode-hook 'my-setup-prettify-symbols-mode)

;; Automatic code-formatting on save.
(add-hook 'after-save-hook
          (lambda ()
            (let ((filename buffer-file-name)
                  (file-ext (file-name-extension buffer-file-name)))

              ;; Python; format files on save using `black'.
              (if (string= file-ext "py")
                  (call-process "black" nil nil nil filename))

              ;; Elm; format files on save using `elm-format'.
              (if (string= file-ext "elm")
                  (call-process "elm-format" nil nil nil "--yes" filename))

              ;; F#; format files on save using `fantomas-tool'.
              (if (string= file-ext "fs")
                  (call-process "fantomas" nil nil nil filename))

              ;; JavaScript; format files on save using `prettier'.
              (if (string= file-ext "js")
                  (call-process "prettier" nil nil nil "--write" filename))

              ;; HTML; format files on save using `prettier'.
              (if (string= file-ext "html")
                  (call-process "prettier" nil nil nil "--write" filename)))))

;;;;------------------------------------
;;;; Appearance.
;;;;------------------------------------

;; Disable GUI stuff that I don't want or need.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; ;; Display line numbers (Emacs 26+).
;; (setq-default display-line-numbers-width 3) ; <-- Prevent 'jump' in margin size at 100 LOC.
;; (global-display-line-numbers-mode t)

;; More visual ease by increasing the line spacing.
(setq-default line-spacing 4)

;; Disable fringes (they don't work well with high-DPI displays anyway).
(fringe-mode 0)

;; Set initial frame size and font.
(setq default-frame-alist `((width . ,my-frame-width) (height . ,my-frame-height) nil))

;; Set up the configured fonts.
(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
                                            (with-selected-frame frame
                                              (my-set-fonts my-fonts my-font-size))))
  (my-set-fonts my-fonts my-font-size))

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
(setq whitespace-line-column my-line-width
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
;;(with-eval-after-load "flycheck"           (diminish 'flycheck-mode)) ;; <-- Only with `spaceline'!
(with-eval-after-load "omnisharp"          (diminish 'omnisharp-mode))
(with-eval-after-load "projectile"         (diminish 'projectile-mode))
(with-eval-after-load "rainbow-mode"       (diminish 'rainbow-mode))
(with-eval-after-load "subword"            (diminish 'subword-mode))
(with-eval-after-load "tide"               (diminish 'tide-mode))
(with-eval-after-load "whitespace"         (diminish 'whitespace-mode))
(with-eval-after-load "xah-math-input"     (diminish 'xah-math-input-mode))
;;(add-hook 'auto-revert-mode-hook '(diminish 'auto-revert-mode)) ;; <-- Doesn't seem to be needed?

;; Load and configure the theme.
(setq spacemacs-theme-comment-bg nil
      spacemacs-theme-comment-italic t
      spacemacs-theme-custom-colors '((comment-light . "#2aa1ae")))

(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
                                            (with-selected-frame frame
                                              (load-theme 'spacemacs-light t))))
  (load-theme 'spacemacs-light t))

;; Fix for `whitespace-mode' when using the `spacemacs-light' theme.
(add-hook 'whitespace-mode-hook (lambda ()
                                  (set-face-attribute 'whitespace-line nil
                                                      :background "#fae9c3"
                                                      :foreground nil)))

;; Fix for `fsharp-mode' when using the `spacemacs-light' theme.
(add-hook 'fsharp-mode-hook (lambda ()
                              (set-face-attribute 'fsharp-ui-operator-face nil
                                                  :foreground "#ba2f59")))

;; I don't like how `LaTeX-mode' changes font sizes, does subscripts, etc.
(with-eval-after-load "latex"
  (setq font-latex-fontify-script nil
        font-latex-fontify-sectioning 'color))

;; Make the mode line look much better. Unforunately, this adds a significant amount of time to the
;; initialization phase. :-(
(doom-themes-neotree-config)
;; (doom-themes-visual-bell-config)
(doom-modeline-mode 1)
(setq doom-modeline-buffer-file-name-style 'relative-to-project
      doom-modeline-major-mode-icon nil)

;; Make sure NeoTree follows current file automatically.
(setq-default neo-autorefresh t)

;; Center frame on display.
(add-hook 'window-setup-hook
          (lambda ()
            (let ((frame (selected-frame)))
              (set-frame-position
               frame
               (/ (- (display-pixel-width) (frame-outer-width frame)) 2)
               (/ (- (display-pixel-height) (frame-outer-height frame)) 2)))))

;;;;------------------------------------
;;;; Key-bindings.
;;;;------------------------------------

;; Easy way to type symbol without enabling `xah-math-input-mode'.
(global-set-key (kbd "C-<return>") 'xah-math-input-change-to-symbol)

;; Shortcut to align lines by regexp.
(global-set-key (kbd "C-c a") 'align-regexp)

;; Switch between buffers easily.
(global-set-key (kbd "C-c b") 'ivy-switch-buffer)

;; Comment easily.
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Toggle double frame view.
(global-set-key (kbd "C-c d") 'my-toggle-dual-window-view)

;; Open current directory with the configured file manager.
(global-set-key (kbd "C-c e") 'my-open-file-manager)

;; Presentation mode.
(global-set-key (kbd "C-c f") 'my-toggle-presentation-mode)

;; Move point to a line quickly.
(global-set-key (kbd "C-c g") 'goto-line)

;; Toggle syntax highlighting.
(global-set-key (kbd "C-c h") 'global-font-lock-mode)

;; Indent region.
(global-set-key (kbd "C-c i") 'indent-region)

;; Kill other buffers easily.
(global-set-key (kbd "C-c k") 'my-kill-other-buffers)

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

;; NeoTree.
(global-set-key (kbd "C-c n") 'my-neotree-project-dir)

;;;;------------------------------------
;;;; Finalization.
;;;;------------------------------------

(let ((elapsed (float-time (time-subtract (current-time) my-start-time)))
      (message ";; Emacs %s initialized in %.3fs\n\n"))
  (setq initial-scratch-message (format message emacs-version elapsed)))

;;; init.el ends here
