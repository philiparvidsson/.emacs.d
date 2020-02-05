(require 'package)
(package-initialize)

;; Make sure we have downloaded the package archive metadata and installed all packages we need.
(unless package-archive-contents
  ;; Enable MELPA package repostiory.
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
   (package-refresh-contents)

  ;; Install packages from (M)ELPA.
  (dolist (it '(
		auctex
                company
                csharp-mode
                diminish
                doom-modeline
                doom-themes
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
                swiper
                spacemacs-theme
                tide
                ;unicode-fonts
                vlf
                web-mode
                xah-math-input
		))
    (unless (package-installed-p it)
      (package-install it))))

;; The multiple cursor keybindings won't work without this.
(require 'multiple-cursors)

;; Use VLF (View Large Files) for large files so we can edit them in Emacs without hanging.
(require 'vlf-setup)
