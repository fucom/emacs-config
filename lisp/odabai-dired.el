;; dired mode

;;; =================================================================================================
;;; Don't clutter with dired buffers
;;; http://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
;;; =================================================================================================
;;[
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
            ))

;; We want dired not not make always a new buffer if visiting a directory
;; but using only one dired buffer for all directories.
;; The same thing can be achieved by using a instead of e, f, enter
;; (defadvice dired-find-file (around dired-subst-directory activate)
;;   "Replace current buffer if file is a directory."
;;   (interactive)
;;   (message "I am activated")
;;   (let ((orig (current-buffer))
;;         (filename (dired-get-filename)))
;;     ad-do-it
;;     (when (and (file-directory-p filename)
;;                (not (eq (current-buffer) orig)))
;;       (kill-buffer orig))))
;;]

;; default commands to run when you press ! on a (selection of) file(s)
(setq dired-guess-shell-alist-user
      (list
       (list "\\.pdf$" "evince");; fixed rule
       (list "\\.pdf$" "okular")))

(provide 'odabai-dired)
