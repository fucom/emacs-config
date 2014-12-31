(when (display-graphic-p)
;; =================================================================================================
;; Knows which files belong to a project (e.g. git)
;; =================================================================================================
  ;;     (ensure-package-installed 'projectile)
  ;; ;; (add-hook 'ruby-mode-hook 'projectile-mode)
  ;; ;; quick indexing : (setq projectile-enable-caching t)
  ;; (if (not (locate-library "helm-projectile"))
  ;;     (ensure-package-installed 'helm-projectile))
  ;; USAGE:
  ;; -> M-x helm-projectile-find-file
  ;;        helm-projectile-grep
  ;;        helm-projectile-switch-to-buffer
  ;;        helm-projectile-recentf
  ;; -> Use .projectile file in .git folder to tell projectile which files to ignore
  )

(provide 'odabai-projectile)
