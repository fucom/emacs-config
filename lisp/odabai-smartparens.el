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

;; void foo ()
;; {
;;
;; }
(defun odabai-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline-and-indent)
  (newline-and-indent)
  (previous-line)
  (previous-line)
  (search-forward "{")
  (backward-char)
  (newline-and-indent)
  (next-line)
  (indent-according-to-mode))

(defun odabai-add-space-after-paren-sexp (&rest _ignored)
  (insert "  ")
  (backward-char))

;; I basically copied this code to show the power of smartparens
;; (sp-local-pair 'c++-mode "{" nil :post-handlers '(:add my-open-block-c-mode))
;; (sp-local-pair 'emacs-lisp-mode "(" nil :post-handlers '(:add my-add-space-after-sexp-insertion))
(sp-local-pair 'c++-mode "{" nil :post-handlers '(:add odabai-create-newline-and-enter-sexp))
(sp-local-pair 'c++-mode "(" nil :post-handlers '(:add odabai-add-space-after-paren-sexp))

;; enable smartparens in minibuffer
(setq sp-ignore-modes-list (delete 'minibuffer-inactive-mode sp-ignore-modes-list))

(provide 'odabai-smartparens)
