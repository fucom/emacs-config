;; Make ediff split side-by-side by default
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(provide 'odabai-ediff)
