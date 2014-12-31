;; =================================================================================================
;; To view very large files
;; =================================================================================================
(ensure-package-installed 'vlf)
(add-hook 'text-mode-hook
          (lambda ()
            (when (> (buffer-size) (* 1024 1024))
              (setq buffer-read-only t)
              (buffer-disable-undo)
              (fundamental-mode)
              (set-variable 'truncate-lines t)
              (linum-mode -1))))

(provide 'odabai-vlf)
