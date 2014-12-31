;;===============================================================================================
;; Autopair:
;; Put closing braces automatically
;;===============================================================================================
(ensure-package-installed 'autopair)
(add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))
(add-hook 'c++-mode-common-hook #'(lambda () (autopair-mode)))
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;               (push '(?< . ?>)
;;                     (getf autopair-extra-pairs :code))))
(add-hook 'prog-mode-hook #'(lambda () (autopair-mode)))
(add-hook 'LaTeX-mode-hook #'(lambda () (autopair-mode)))
(add-hook 'c++-mode-hook
          #'(lambda ()
              (push '(?< . ?>)
                    (getf autopair-dont-pair :code))))

(provide 'odabai-autopair)
