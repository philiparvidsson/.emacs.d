;;(package-initialize)
(require 'cl)

;; -----------------------------------------------
;; Disable much of GUI.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; -----------------------------------------------
;; No startup message please...
(setq inhibit-startup-message t)

;; -----------------------------------------------
;; Setup global variables.
;;(setq is-linux (equal system-type 'gnu/linux))
;;(setq is-mac (equal system-type 'darwin))

;; -----------------------------------------------
;; Setup paths.
(setq settings-dir
      (concat user-emacs-directory "settings"))

(setq custom-file (concat user-emacs-directory "custom.el"))
;;(load custom-file)

(add-to-list 'load-path settings-dir)

;; -----------------------------------------------
;; Setup sane defaults.
(require 'sane-defaults)

;; Setup base configuration.
(require 'appearance)
(require 'key-bindings)
(require 'mode-mappings)
(require 'setup-package)

;; -----------------------------------------------
;; Setup intrinsic modes.
(require 'setup-c++-mode)

;; -----------------------------------------------
;; Install and setup packages from (M)ELPA.
(packages-install
 '(auctex
   company
   csharp-mode
   diminish
   fill-column-indicator
   flycheck
   github-theme
   haskell-mode
   highlight-numbers
   highlight-parentheses
   hlinum
   hy-mode
   magit
   multiple-cursors
   projectile
   rainbow-mode
   web-mode))
