(when (display-graphic-p)
  ;; ======================================================================================================
  ;; Completion in M-x and find-file
  ;; ======================================================================================================
  ;; (if (not (locate-library "helm"))
  ;;     (ensure-package-installed 'helm))
  ;; (require 'helm-config)

  ;; (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
  ;; (global-set-key (kbd "M-x") 'helm-M-x)
  ;; (global-set-key (kbd "C-x b") 'helm-mini)
  ;; (setq helm-buffers-fuzzy-matching t
  ;;       helm-recentf-fuzzy-match    t)
  ;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
  ;; (add-hook 'eshell-mode-hook
  ;;           #'(lambda ()
  ;;               (define-key eshell-mode-map (kbd "<tab>")     #'helm-esh-pcomplete)
  ;;               (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))

  ;; (helm-mode 1)
  ;; ;; (setq helm-ff-ido-style-backspace 'always)
  ;; (setq helm-ff-skip-boring-files t)
  ;; (define-key helm-find-files-map (kbd "<backspace>") 'helm-find-files-up-one-level)
  ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  ;; (dolist (ext '("\\.mtc*" "\\.blg$" "\\.bbl$"  "\\.bash_history"))
  ;;   (add-to-list 'helm-boring-file-regexp-list ext))

  ;; ;; I don't like to see recent files in buffer list
  ;; (defcustom helm-mini-default-sources '(helm-source-buffers-list
  ;;                                        ;helm-source-recentf
  ;;                                        helm-source-buffer-not-found)
  ;;   "Default sources list used in `helm-mini'."
  ;;   :group 'helm-misc
  ;;   :type '(repeat (choice symbol)))

  ;; ;; (setq helm-idle-delay 0.1)
  ;; ;; (setq helm-input-idle-delay 0.1)
)

(provide 'odabai-helm)
