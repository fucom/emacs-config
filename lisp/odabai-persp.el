(when (display-graphic-p)
  ;; =================================================================================================
  ;; Multiple workspaces
  ;; =================================================================================================
  ;;     (ensure-package-installed 'persp-mode)

  ;; (defun persp-cycle ()
  ;;   "Cycle through the available perspectives."
  ;;   (interactive)
  ;;   (let ((next-pos (1+ (persp-curr-position)))
  ;;         (list-size (length (persp-all-names))))
  ;;     (cond ((eq 1 list-size) (persp-switch nil))
  ;;           ((>= next-pos list-size) (persp-switch (nth 0 (persp-all-names))))
  ;;           (t (persp-next)))))

  ;; (persp-mode 1)
  ;; (setq persp-auto-save-opt 0)
  ;; (define-key persp-mode-map (kbd "C-c p n")  'persp-cycle)
  )

(provide 'odabai-persp)
