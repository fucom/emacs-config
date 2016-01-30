;; TODO: make a keymap for C-k so that we can kill a line/end/start of file... in a easy way
;;       -> ;; https://github.com/kai2nenobu/guide-key

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
      '( ace-jump-line-mode
        ace-jump-char-mode
        ace-jump-word-mode))
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; =================================================================================================
;; ibuffer
;; =================================================================================================
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; launch imenu or idomenu if available
(global-set-key (kbd "C-c i") 'idomenu)

;; ==========================================================================================
;; better window navigation
;; ==========================================================================================
(ensure-package-installed 'win-switch)
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

(global-set-key (kbd "C-x 6 v") 'split-window-vertically-ff)
(global-set-key (kbd "C-x 6 h") 'split-window-horizontally-ff)

;; (add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-j") 'control-j-newline-indent)))

(global-set-key (kbd "C-k") 'delete-line)
(global-set-key (kbd "M-d") 'delete-word)

(global-set-key (kbd "C-c C-w") (quote copy-word))
(global-set-key (kbd "C-c C-k") (quote copy-line))

(global-set-key (kbd "C-x C-v") 'revert-buffer-func)

(global-set-key (kbd "<down>") 'move-region-or-next-line)
(global-set-key (kbd "<up>") 'move-region-or-previous-line)

;; we define comment-region for all files because we can define the commenting string afterwards.
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
;; you can also use (C-c q) to break lines in comments
;; (add-hook 'prog-mode-hook 'turn-on-auto-fill)

;; (global-set-key (kbd "<f12>") 'create-or-kill-eshell)
;; (global-set-key (kbd "C-<f12>") 'erase-eshell-buffer)

;; killing region without kill ring
(global-set-key (kbd "C-M-q") 'delete-region)

;; to delete only the window : C-x 0
(global-set-key (kbd "C-x 4 t") 'toggle-window-split)
(global-set-key (kbd "C-x 4 k") 'kill-buffer-and-window)

;; spartparens
(sp-use-smartparens-bindings)
;; you can run backward commands with negative prefix (C-M--)
(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "M-<C-backspace>") 'sp-backward-kill-sexp)
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
;; use C-- M-<delete> for sp-backward-unwrap-sexp
(define-key sp-keymap (kbd "<M-backspace>") nil)

;; nice search: highlight-symbol

;; =================================================================================================
;; Give visual feed-back when searching for regexp
;; https://github.com/benma/visual-regexp-steroids.el/blob/master/README.md
;; =================================================================================================
(ensure-package-installed 'visual-regexp-steroids)
(require 'visual-regexp-steroids)
(global-set-key (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; I prefer to use the emacs commands here. Seem to work better
;; (define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
;; (define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s

;; =================================================================================================
;; Nice rectangular visual feed-back mode. It is not that powerful as the emacs 24.4 version
;; especially for yanking/kill-ring-saving rectangle w/o the rectangle commands.
;; =================================================================================================
(ensure-package-installed 'rect-mark)
(when (or (and (= emacs-major-version 24) (<= emacs-minor-version 3))
          (< emacs-major-version 24))
  (define-key ctl-x-map (kbd "SPC") 'rm-set-mark)
  (define-key ctl-x-map "r\C-y" 'yank-rectangle)
  (define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
  (define-key ctl-x-map "r\C-w" 'rm-kill-region)
  (define-key ctl-x-map "r\M-w" 'rm-kill-ring-save))

(global-set-key (kbd "<f9> t") 'cycle-color-theme)

(define-key global-map (kbd "<f9> m") 'mu4e)

(define-key global-map (kbd "C-c b") 'backup-current-file)

(global-set-key (kbd "<f8>") (lambda () (interactive) (ansi-term "/bin/bash")))

;; cycle through mark ring using C-u C-<space> and repeat process by typing C-<space>
(setq set-mark-command-repeat-pop t)

(global-set-key (kbd "C-h s") 'apropos)

(global-set-key (kbd "C-x g") 'magit-status)

;; Use C-S-o to open a new line below and move point there.
;; With prefix argument, creates the line above.
(global-set-key (kbd "C-S-o") 'odabai/goto-created-newline)

;; Easier way of commenting than comment-dwim especially for one line
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; ====================================================================================================
;; multiple cursors
;; ====================================================================================================
;; (defun odabai-mc-dwim (&optional args)
;;   (interactive "P")
;;   (if args
;;       (mc/edit-lines)
;;     (if (use-region-p)
;;         (mc/mark-more-like-this-extended)
;;       (mc/mark-all-like-this))))
;; (copy-region-as-kill beg end)
;; (with-temp-buffer
;;   (yank)
;; (goto-char 0)
;; save-match-data
;; http://emacswiki.org/emacs/ThingAtPoint
;; count-lines start end => To know if region is over multiple lines
;; Nice way of knowing which option:
;; (style (cond
;;         ;; called from lisp
;;         ((and arg (symbolp arg))
;;          arg)
;;         ;; negative argument
;;         ((< (prefix-numeric-value arg) 0)
;;          'ignore)
;;         (arg 'pad)
;;         (t mc/edit-lines-empty-lines))))

;; (defun odabai-mc-dwim (&optional args)
;;   (interactive "P")
;;   ;; find out if user selected sexp or region over multiple lines
;;   (style (cond
;;           ((and arg (symbolp arg)) '
;;            )
;;   (int-to-string (count-lines (region-beginning) (region-end)))
;;   )

;; (ensure-package-installed 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)
(global-set-key (kbd "C-.") 'mc/mark-more-like-this-extended)

(provide 'odabai-keybindings)
