;; dired mode
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
  ))

;; default commands to run when you press ! on a (selection of) file(s)
(setq dired-guess-shell-alist-user
      (list
       (list "\\.pdf$" "evince");; fixed rule
       (list "\\.pdf$" "okular")))

(provide 'odabai-dired)
