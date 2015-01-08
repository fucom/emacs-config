;; allows to manipulate occurances of a word simultaneously. It is bound to C-u C-c r.
(ensure-package-installed 'iedit)

;; we will bind it to C-u C-c r as defined in odabai-defun and binded in odabai-keybindings
(setq iedit-toggle-key-default nil)

(provide 'odabai-iedit)
