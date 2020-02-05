;; Prefer UTF-8 and Unix line endings.
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)


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
(setq-default fill-column max-line-width)

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

;; Store backup files in the temp directory and don't create .#-files, etc.
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      backup-by-copying t
      create-lockfiles nil
      delete-old-versions t)

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

;; Set up sane scrolling.
(setq mouse-wheel-follow-mouse t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(3 ((shift) . 1)))
(setq scroll-step 1)

;; Make Phi Search work in a more intuitive manner.
(setq-default phi-search-case-sensitive 'guess)

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

(defun setup-tide-mode ()
  "Set up Tide mode in Emacs."
  (web-mode)
  (tide-setup)
  (eldoc-mode))

;; Mode mappings.
(dolist (it '(("\\.h\\'"    . c++-mode)  ; <-- Emacs will use `c-mode' in .h-files without this.
              ("\\.jsx?\\'" . js2-mode)
              ("\\.tsx?\\'" . setup-tide-mode)))
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
