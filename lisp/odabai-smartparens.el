(ensure-package-installed 'smartparens)

(require 'smartparens-config)
(smartparens-global-mode t)

;; highlights matching pairs
;; (show-smartparens-global-mode t)

(defun my-open-block-c-mode (id action context)
  (when (eq action 'insert)
    (newline)
    (newline)
    (indent-according-to-mode)
    (previous-line)
    (indent-according-to-mode)))

(defun my-add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (length (plist-get (sp-get-pair id) :close)))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-local-pair 'c++-mode "{" nil :post-handlers '(:add my-open-block-c-mode))
(sp-local-pair 'emacs-lisp-mode "(" nil :post-handlers '(:add my-add-space-after-sexp-insertion))
;; (sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

(provide 'odabai-smartparens)
