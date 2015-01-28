;; =================================================================================================
;; ido for nice and fast file searching/switching
;; do not confirm a new file or buffer
;; =================================================================================================
(setq confirm-nonexistent-file-or-buffer nil)
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-last-directory-history nil)
(setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
(setq ido-show-dot-for-dired t) ;; put . as the first item

;; Use smex to provide ido-like interface for M-x
(ensure-package-installed 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(ensure-package-installed 'ido-vertical-mode)
(ido-vertical-mode 1)

;; -------------------------------------------------------------------------------------------------
;; Do not list all files in switch buffer
;; This can be extended to show more starred buffers
;; -------------------------------------------------------------------------------------------------
(defun ido-ignore-but-this (name)
  "Ignore all non-user (a.k.a. *starred*) buffers except *eshell*, *scratch* and *Messages*.
   Always show current buffer in the listing."
  (and (string-match "^\*" name)
       (not (or (string= name "*scratch*") (string-match name "\*eshell\*") (string= name "*Messages*")))
       (not (string= name (buffer-name)))))

(setq ido-ignore-buffers '("^ " ido-ignore-but-this)) ;; ignore starred buffers

(provide 'odabai-ido)

;; =================================================================================================
;; ido-imenu : It's is like imenu but with ido support
;; =================================================================================================
(ensure-package-installed 'idomenu)

(set-default 'imenu-auto-rescan t)
