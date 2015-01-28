;; todo: how home dir is shown in eshell
;;       -> redshank -> eldoc/cl-doc -> elisp/slime nav ->highlight-cl
;;       -> erase backup files after 29days
;;       -> why does it not recognise pm-suspend ?
;;       -> stumpwm: see how to get number of program once started to move them like windowlist
;;       -> when I do C-u C-k to delete a whole line it saves only whitespaces in the yank buffer

;; )Where I keep all my lisp and configurations for emacs
(setq dotfiles-dir (expand-file-name "/home/odabai/.emacs.d/"))

;; configuration for various modes
(add-to-list 'load-path (concat dotfiles-dir "lisp"))

;; Subpackages specific to task (e.g. mode, theme)
(setq pkg-full
      '(odabai-defuns
	odabai-elpa
	odabai-backup
	odabai-dired
	odabai-ediff
	odabai-ido
	odabai-org
	odabai-vlf
	;; odabai-projectile !
	odabai-persp
        ;; Right now I prefer to use perspective for sessions saving
        ;; odabai-desktop !
	odabai-prog
	odabai-magit
	odabai-matlab
        odabai-smartparens
	odabai-keybindings
        odabai-ac
	odabai-eshell
	;; odabai-slime !
        ;; very time consuming
	;; odabai-helm !
	odabai-cpp
	;; odabai-autopair !
	odabai-auctex
	odabai-theme
        ;; odabai-snippets !
        odabai-iedit))

;; Now load other things
(dolist (file pkg-full)
  (require file))

;; =======================================================================================================
;; Put here by emacs
;; =======================================================================================================
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; =======================================================================================================
;; Helpful stuff
;; =======================================================================================================
;; delete-trailing-whitespace

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((eval font-lock-add-keywords nil (\` (((\, (concat "(" (regexp-opt (quote ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")) t) "\\_>")) 1 (quote font-lock-variable-name-face))))) (reftex-default-bibliography "egbib.bib") (TeX-master . "../latex2014") (modee . latex)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Local Variables:
;; mode: lisp
;; End:

