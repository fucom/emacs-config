(defcustom ask-to-save-desktop nil "Asks the user whether to restore and save current desktop.")
;; =================================================================================================
;; Desktop session management
;; =================================================================================================
(when (and ask-to-save-desktop (display-graphic-p))
  ;; We can't set it to 1 as it will add a hook into kill-emacs-hook and execute another
  ;; (desktop-read) on top of ours !
  ;; (desktop-save-mode 1)
  ;;(add-to-list 'desktop-globals-to-save 'file-name-history)
  ;; Make sure that no more than 3 search items are saved to gain in disk space
  ;; (add-hook 'kill-emacs-hook
  ;;           '(lambda ()
  ;;              (desktop-truncate search-ring 3)
  ;;              (desktop-truncate regexp-search-ring 3))

  ;; remove desktop after it's been read
  ;; HAMID: this is not needed as we delete any way the desktop file
  (add-hook 'desktop-after-read-hook
            '(lambda ()
               ;; desktop-remove clears desktop-dirname
               (setq desktop-dirname-tmp desktop-dirname)
               (desktop-remove)
               (setq desktop-dirname desktop-dirname-tmp)))

  ;; Function that tells whether a desktop file exists
  (defun saved-session ()
    (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

  ;; use session-restore to restore the desktop manually
  (defun session-restore ()
    "Restore a saved emacs session."
    (interactive)
    (if (saved-session)
        (desktop-read)
      (message "No desktop found.")))

  ;; use session-save to save the desktop manually
  (defun session-save ()
    "Save an emacs session."
    (interactive)
    (if (saved-session)
        (if (y-or-n-p "Overwrite existing desktop? ")
            (desktop-save-in-desktop-dir)
          (message "Session not saved."))
      (desktop-save-in-desktop-dir)))

  ;; =================================================================================================
  ;; Save current session and re-open it at emacs start-up
  ;; Use only one desktop
  ;; =================================================================================================

  (setq desktop-path '("/home/homeless/.emacs.d/")
        desktop-dirname "/home/homeless/.emacs.d/"
        desktop-base-file-name "desktop-emacs"
        desktop-base-lock-name  "desktop-emacs.lock"
        history-length 250
        desktop-load-locked-desktop nil)

  ;; ask user whether to restore desktop at start-up
  (add-hook 'after-init-hook
            '(lambda ()
               (if (saved-session)
                   (if (y-or-n-p "Restore desktop? ")
                       (session-restore)
                     (progn
                       ;; desktop-remove clears desktop-dirname
                       (setq desktop-dirname-tmp desktop-dirname)
                       (desktop-remove)
                       (setq desktop-dirname desktop-dirname-tmp))))))

  (add-hook 'kill-emacs-hook
            '(lambda ()
               (require 'desktop)
               (setq desktop-save-mode 1)
               (desktop-kill)))
  )
(provide 'odabai-desktop)
