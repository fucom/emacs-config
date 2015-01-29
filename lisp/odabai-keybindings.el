(global-set-key (kbd "C-z") 'goto-line)

;; scroll screen using up/down bottom
(global-set-key [next] 'scroll-n-lines-ahead)
(global-set-key [prior] 'scroll-n-lines-behind)

;; =================================================================================================
;; cool jumping in file
;; =================================================================================================
(ensure-package-installed 'ace-jump-mode)
;; this list specifies what the influence of the prefix is
(setq ace-jump-mode-submode-list
      '(ace-jump-char-mode
        ace-jump-line-mode
        ace-jump-word-mode))
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; ==========================================================================================
;; better window navigation
;; ==========================================================================================
(ensure-package-installed 'win-switch)
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

;; (add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-j") 'control-j-newline-indent)))

(global-set-key (kbd "C-k") 'delete-line)
(global-set-key (kbd "M-d") 'delete-word)

(global-set-key (kbd "C-c C-w") (quote copy-word))
(global-set-key (kbd "C-c C-k") (quote copy-line))

(global-set-key (kbd "C-x C-v") 'revert-buffer-func)

(global-set-key (kbd "<down>") 'move-region-or-next-line)
(global-set-key (kbd "<up>") 'move-region-or-previous-line)

(add-hook 'prog-mode-hook (lambda() (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)))
;; you can also use (C-c q) to break lines in comments
;; (add-hook 'prog-mode-hook 'turn-on-auto-fill)

(global-set-key (kbd "<f12>") 'create-or-kill-eshell)
(global-set-key (kbd "C-<f12>") 'erase-eshell-buffer)

;; killing region without kill ring
(global-set-key (kbd "C-M-q") 'delete-region)

;; to delete only the window : C-x 0
(global-set-key (kbd "C-x 4 t") 'toggle-window-split)
(global-set-key (kbd "C-x 4 k") 'kill-buffer-and-window)

;; spartparens
(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "M-<C-backspace>") 'sp-backward-kill-sexp)
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)


;; nice search: highlight-symbol

;; =================================================================================================
;; Give visual feed-back when searching for regexp
;; =================================================================================================
(ensure-package-installed 'visual-regexp-steroids)
(global-set-key (kbd "C-c r") 'vr/my-search-replace-simultaneously)
;; (define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s


(provide 'odabai-keybindings)
