;; ==================================================================================================
;; Git project management
;; ==================================================================================================
(ensure-package-installed 'magit)

;; Highlight uncommmitted changes and more
(ensure-package-installed 'diff-hl)

(when (display-graphic-p))

(provide 'odabai-magit)
