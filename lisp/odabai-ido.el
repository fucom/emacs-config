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
;; (ido-vertical-mode)

;; Do not list all buffers.
;; -------------------------------------------------------------------------------------------------
(defun ido-ignore-this (name)
  "List here the files to ignore."
  nil)
(setq ido-ignore-buffers '("^ " ido-ignore-this))

;; ido-imenu : It's is like imenu but with ido support
;; -------------------------------------------------------------------------------------------------
(ensure-package-installed 'idomenu)
(set-default 'imenu-auto-rescan t)

;; Try to use ido everywhere possible
;; -------------------------------------------------------------------------------------------------
(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

    Set it to nil using let in around-advice for functions where the
    original completing-read is required.  For example, if a function
    foo absolutely must use the original completing-read, define some
    advice like this:

    (defadvice foo (around original-completing-read-only activate)
      (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
    (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (and (boundp 'ido-cur-list)
               ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                                     allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))

;; Fuzzy matching without having to type the characters in order
;; -------------------------------------------------------------------------------------------------
(ensure-package-installed 'ido-better-flex)
(ido-better-flex/enable)

(provide 'odabai-ido)
