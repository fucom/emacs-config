;; todo: how home dir is shown in eshell
;;       -> redshank -> eldoc/cl-doc -> elisp/slime nav ->highlight-cl
;;       -> stumpwm: see how to get number of program once started to move them like windowlist
;;       -> stumpwm: gmail notifier
;;       -> when I do C-u C-k to delete a whole line it saves only whitespaces in the yank buffer
;;       -> Problem with remembering eshell password
;;          http://emacs.stackexchange.com/questions/5608/how-to-let-eshell-remember-sudo-password-for-two-minutes
;;       -> use multiple cursors
;;       -> go back to previous eshel with F12.
;;       -> adapt region color in solarized theme if smart-parens highlights sexp.
;;       -> clean solarized theme
;;       -> how to make persp-mode use helm (not really of importance)
;;       -> Add to current persp every newly opened buffer (e.g. compilation, grep,...)
;;       -> Backup directory
;;       -> Helm sucks: Why is the buffer list not sorted in last opened buffer
;;       -> better commeting using dwim. Advice it
;;       -> copy whole sexp with C-M-<space> and normal behaviour with prefix argument.
;;       -> killing whole line, to beginning of line,...
;;       -> adapt ansi-color for solarized
;;       -> improve compile command to dwim (http://www.emacswiki.org/emacs/CompileCommand)
;;       -> write the module as helm-mt for eshell

;; Where I keep all my lisp and configurations for emacs
(setq dotfiles-dir (expand-file-name "/home/odabai/.emacs.d/"))

;; configuration for various modes
(add-to-list 'load-path (concat dotfiles-dir "lisp"))

;; location of emacs source code (c files)
(setq source-directory "/home/odabai/.emacs.d/emacs-24.4/")

;; make my packages as safe
(if (boundp 'custom-enabled-themes)
    (custom-set-variables
     '(custom-safe-themes
       (quote
        ("3e8bea8a29d13ca8d345517d2461a0243c4fdd4d25739bc1f67dc00004657943"
         "dc68acc61849ee7b03405acdd6c2999cc3874b7ba9fbb6ec6c4254dfda503a56"
         default)))))

;; Subpackages specific to task (e.g. mode, theme)
(setq pkg-full
      '(
        odabai-defuns
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
	;; odabai-helm
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
 '(custom-enabled-themes (quote (odabai-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("cbef37d6304f12fb789f5d80c2b75ea01465e41073c30341dc84c6c0d1eb611d" "3e8bea8a29d13ca8d345517d2461a0243c4fdd4d25739bc1f67dc00004657943" "dc68acc61849ee7b03405acdd6c2999cc3874b7ba9fbb6ec6c4254dfda503a56" default)))
 '(inhibit-startup-screen t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/thesis.org" "~/Dropbox/org/general.org")))
 '(safe-local-variable-values (quote ((reftex-default-bibliography "egbib.bib")))))
;; Local Variables:
;; mode: lisp
;; End:

