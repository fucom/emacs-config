;; todo: how home dir is shown in eshell
;;       -> redshank -> eldoc/cl-doc -> elisp/slime nav ->highlight-cl
;;       -> stumpwm: see how to get number of program once started to move them like windowlist
;;       -> stumpwm: gmail notifier
;;       -> when I do C-u C-k to delete a whole line it saves only whitespaces in the yank buffer
;;       -> Problem with remembering eshell password
;;          http://emacs.stackexchange.com/questions/5608/how-to-let-eshell-remember-sudo-password-for-two-minutes
;;       -> use multiple cursors

;; Where I keep all my lisp and configurations for emacs
(setq dotfiles-dir (expand-file-name "/home/odabai/.emacs.d/"))

;; configuration for various modes
(add-to-list 'load-path (concat dotfiles-dir "lisp"))

;; location of emacs source code (c files)
(setq source-directory "/home/odabai/.emacs.d/emacs-24.4/")

;; Subpackages specific to task (e.g. mode, theme)
(setq pkg-full
      '( odabai-defuns
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
	odabai-slime
        ;; very time consuming
	;; odabai-helm !
	odabai-cpp
	;; odabai-autopair !
	odabai-auctex
        ;; odabai-snippets !
        odabai-iedit
        odabai-stumpwm
	odabai-theme
        odabai-mu4e
        ))

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(popup-isearch-match ((t (:inherit isearch)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((reftex-default-bibliography "egbib.bib")))))
;; Local Variables:
;; mode: lisp
;; End:

