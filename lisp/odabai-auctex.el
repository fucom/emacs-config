;; ===============================================================================================
;; Latex configuration
;; ===============================================================================================
;; C-c C-e : starting new environment
;; C-c C-m : for macros like \footnote
;; M-/     : for completion
(ensure-package-installed 'auctex)
(ensure-package-installed 'reftex)
(ensure-package-installed 'latex-preview-pane)

;; Auto-capitalize: Capitalizes the beginning of a sentence.
;; ATTENTION (emacs 24.3.1):
;;   change the line in file .emacs.d/elpa/auto-capi./auto-capitalize.elc
;;   (defalias 'auto-capitalize #[(beg end length) to
;;   (defalias 'auto-capitalize #[(&optional beg end length)
;;   Do the same in the auto-capitalize.el file.
;; The parameter has to be optional for desktop.el to save/load correctly the current desktop.
(ensure-package-installed 'auto-capitalize)

(when (display-graphic-p)
  (if (locate-library "reftex")
      (progn (require 'auto-capitalize)
             (custom-set-variables
              '(inhibit-startup-screen t)
              '(TeX-PDF-mode t))
             (add-hook 'LaTeX-mode-hook 'auto-capitalize-mode)
             (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
             (add-hook 'LaTeX-mode-hook 'flyspell-mode)
             (add-hook 'LaTeX-mode-hook (lambda() (set-variable 'truncate-lines nil)))

             (setq sentence-end-double-space nil)

                                        ; --------------------------------------------------------------------------------------
                                        ; Reverse and forward search between PDF and TEX
                                        ; --------------------------------------------------------------------------------------
             (add-hook 'LaTeX-mode-hook 'server-start)
                                        ;(setq TeX-source-correlate-method 'synctex)
                                        ;(custom-set-variables '(LaTeX-command "latex -synctex=1"))
             (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
             (setq TeX-view-program-selection
                   '((output-pdf "PDF Viewer")))
                                        ; IMPORTANT: to give auto-focus to Okular, I added:
                                        ; - && wmctrl -a %o - Okular
                                        ; - and you have to disable in Settings/Configure Okular/General/Display document title in title bar
             (setq TeX-view-program-list
                   '(("PDF Viewer" "okular --unique %o#src:%n%b && wmctrl -a %o - Okular #")))
             ))
  ) ;; display graphic

(provide 'odabai-auctex)
