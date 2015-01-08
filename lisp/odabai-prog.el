;; Dependencies : Load odabai-desktop before to define ask-to-save-desktop.

;;===============================================================================================
;; Vertical dotted lines serving as indention guide
;; call manually with M-x indent-guide-mode
;;===============================================================================================
(ensure-package-installed 'indent-guide)

;;===============================================================================================
;; Hideshow
;;===============================================================================================
;; (ensure-package-installed 'hideshow-org))
;; (require 'hideshow-org)
;; (global-set-key "\C-ch" 'hs-org/minor-mode)

;; Do not ask user for closing confirmation if we already ask for desktop session saving.
(unless (or (and (boundp 'ask-to-manage-sessions) ask-to-manage-sessions) (and (boundp 'ask-to-save-destop) ask-to-save-desktop))
  (add-hook 'prog-mode-hook (lambda() (local-set-key (kbd "C-x C-c") 'ask-before-closing))))

;; ---------------------------------------------------------------------------------------------
;; matching pair of braces
;; ---------------------------------------------------------------------------------------------
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

(provide 'odabai-prog)
