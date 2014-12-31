(setq eshell-highlight-prompt nil)
;; =================================================================================================
;; eshell aliases
;; =================================================================================================
(setq eshell-aliases-file (expand-file-name "~/.eshell/alias"))
(defvar eshell-aliases nil
  "These are my personal eshell aliases that are created if eshell-aliases-file does not exist.")
(setq eshell-aliases (list
                    "ll 'ls -l'"
                    "ff 'find-file $1'"))
;; create aliases
(when (not (file-exists-p eshell-aliases-file))
    (eshell "new")
    (rename-buffer (concat "*create aliases*"))
    
    (dolist (my-alias eshell-aliases)
      (insert (concat "alias " my-alias))
      (eshell-send-input))

    (kill-buffer))

;; shortcut for (e)shell
;; shell  : because I can easily delete buffer there with <C-c l>
;; eshell : better completion of files/commands
(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-l")
                           (lambda ()
                             (interactive)
                             (insert
                              (ido-completing-read "Eshell history: "
                                                   (delete-dups
                                                    (ring-elements eshell-history-ring))))))
            (local-set-key (kbd "C-c C-h") 'eshell-list-history)))

;; helpful command
;; erase-buffer or erase-eshell-buffer


;; Popup menu for the various possibilities
;; (defcustom complete-in-region-use-popup t
;;     "If non-NIL, complete-in-region will popup a menu with the possible completions."
;;     :type 'boolean
;;     :group 'completion)

;; (defun popup-complete-in-region (next-func start end collection &optional predicate)
;;     (if (not complete-in-region-use-popup)
;;         (funcall next-func start end collection predicate)
;;       (let* ((prefix (buffer-substring start end))
;;              (completion (try-completion prefix collection predicate))
;;              (choice (and (stringp completion)
;;                           (string= completion prefix)
;;                           (popup-menu* (all-completions prefix collection predicate))))
;;              (replacement (or choice completion))
;;              (tail (and (stringp replacement)
;;                         (not (string= prefix replacement))
;;                         (substring replacement (- end start)))))
;;         (cond ((eq completion t)
;;                (goto-char end)
;;                (message "Sole completion")
;;                nil)
;;               ((null completion)
;;                (message "No match")
;;                nil)
;;               (tail
;;                (goto-char end)
;;                (insert tail)
;;                t)
;;               (choice
;;                (message "Nothing to do")
;;                nil)
;;               (t
;;                (message "completion: something failed!")
;;                (funcall next-func start end collection predicate))))))
;; (add-hook 'completion-in-region-functions 'popup-complete-in-region)


(provide 'odabai-eshell)
