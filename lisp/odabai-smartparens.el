(ensure-package-installed 'smartparens)

(require 'smartparens-config)
(smartparens-global-mode t)

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

;; I basically copied this code to show the power of smartparens
;; (sp-local-pair 'c++-mode "{" nil :post-handlers '(:add my-open-block-c-mode))
;; (sp-local-pair 'emacs-lisp-mode "(" nil :post-handlers '(:add my-add-space-after-sexp-insertion))
(sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

;; enable smartparens in minibuffer
(setq sp-ignore-modes-list (delete 'minibuffer-inactive-mode sp-ignore-modes-list))

(provide 'odabai-smartparens)
