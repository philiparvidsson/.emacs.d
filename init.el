(defconst startup-time (current-time))

;; Disable GC during initialization and then enable it again when we're done. Speeds up
;; initialization somewhat.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 2000000)))

(defconst emacs-d (file-name-directory load-file-name))

(mapc 'load
      (directory-files
       (expand-file-name "elisp" emacs-d)
       t                                ; <-- Full paths.
       "\\.el$"))

(let ((elapsed (float-time (time-subtract (current-time) startup-time)))
      (message ";; Emacs %s initialized in %.3fs\n\n"))
  (setq initial-scratch-message (format message emacs-version elapsed)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (spacemacs-theme xah-math-input web-mode vlf tide swiper solaire-mode rainbow-mode projectile phi-search omnisharp neotree multiple-cursors markdown-mode magit lua-mode js2-mode hydra hy-mode groovy-mode glsl-mode fsharp-mode ess doom-themes doom-modeline diminish company auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
