;; todo: how home dir is shown in eshell
;;       eshell-pcomplete

;; Where I keep all my lisp and configurations for emacs
(setq dotfiles-dir (expand-file-name "/home/odabai/.emacs.d/"))

;; configuration for various modes
(add-to-list 'load-path (concat dotfiles-dir "lisp"))

;; Subpackages specific to task (e.g. mode, theme)
(setq pkg-full
      '(odabai-defuns
	odabai-backup
	odabai-dired
	odabai-ediff
	odabai-elpa
	odabai-ido
	odabai-org
	odabai-vlf
	odabai-persp
	odabai-projectile
        odabai-desktop
	odabai-prog
	odabai-magit
	odabai-matlab
	odabai-keybindings
	odabai-eshell
	odabai-slime
	odabai-helm
	odabai-cpp
	;; ;; odabai-autopair
        odabai-smartparens
	odabai-auctex
	odabai-theme
        odabai-snippets
        odabai-ac
        odabai-iedit))

;; Now load other things
(dolist (file pkg-full)
  (require file))

;; =======================================================================================================
;; Put here by emacs
;; =======================================================================================================
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((reftex-default-bibliography "egbib.bib") (TeX-master . "../latex2014") (modee . latex)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; mode: lisp
;; End:

