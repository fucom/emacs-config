(defcustom ask-to-manage-sessions t "Asks the user whether to restore and save current sessions.")

(when (and ask-to-manage-sessions (display-graphic-p))
  ;; =================================================================================================
  ;; Multiple workspaces
  ;; Issues : - Cycling through perspectives (workspaces) is really possible as all perspectives are
  ;;            saved in hash tables.
  ;;          - Currently, I only manage one session which makes loading/saving faster as no user
  ;;            needs to be asked. This can be changed by not passing default filenames to
  ;;            perspective's save and load functions.
  ;; =================================================================================================
  (ensure-package-installed 'persp-mode)

  (setq persp-auto-save-opt 0)
  ;; Do not load sessions at start-up without asking me
  (setq persp-auto-resume-time 0)
  (setq persp-save-dir (concat dotfiles-dir "persp-confs/"))
  (setq persp-auto-save-fname "persp-auto-save")
  (unless (file-exists-p persp-save-dir)
    (make-directory persp-save-dir))

  (persp-mode 1)

  ;; check whether we have a previously saved session
  (defun odabai/persp-saved-session ()
    (file-exists-p (concat persp-save-dir persp-auto-save-fname)))

  ;; use session-restore to restore the session manually
  (defun odabai/persp-session-restore ()
    "Restore a saved emacs session."
    (interactive)
    (if (odabai/persp-saved-session)
        (persp-load-state-from-file)
      (message "No session found.")))

  (defun odabai/persp-session-save ()
    "Save an emacs session."
    (interactive)
    (if (odabai/persp-saved-session)
        (if (y-or-n-p "Overwrite existing session? ")
            (persp-save-state-to-file)
          (message "Session not saved."))
      (persp-save-state-to-file persp-auto-save-fname)))

  ;; function that asks the user whether he wants to restore session. It is used at start-up
  (defun odabai/persp-ask-user-restore-session ()
    (if (odabai/persp-saved-session)
        (if (y-or-n-p "Restore session? ")
            (progn
              (odabai/persp-session-restore)
              (delete-file (concat persp-save-dir persp-auto-save-fname))))))

  (defun odabai/persp-ask-to-save-session ()
    "Save an emacs session."
    (interactive)
    (if (y-or-n-p "Would you like to save the current session?")
        (odabai/persp-session-save)))

  (add-hook 'after-init-hook 'odabai/persp-ask-user-restore-session)
  (add-hook 'kill-emacs-hook 'odabai/persp-ask-to-save-session)
  )

(provide 'odabai-persp)
