(setq eshell-highlight-prompt nil)
;; =================================================================================================
;; eshell aliases
;; =================================================================================================
(setq eshell-directory-name (concat dotfiles-dir "eshell/"))
(setq eshell-aliases-file (concat eshell-directory-name "alias"))
(defvar eshell-aliases nil
  "These are my personal eshell aliases that are created if eshell-aliases-file does not exist.")
;; If you update aliases remove eshell-aliases-file
(setq eshell-aliases (list
                    "ll 'ls -l $*'"
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

;; remember password for sudo
(setq password-cache t) ; enable password caching
(setq password-cache-expiry 10) ; for 10 minutes

(when (display-graphic-p)
  ;; =================================================================================================
  ;; Popup menu for the various possibilities
  ;; =================================================================================================
  (defcustom complete-in-region-use-popup t
    "If non-NIL, complete-in-region will popup a menu with the possible completions."
    :type 'boolean
    :group 'completion)

  ;; nicer highlight of matched searches
  (custom-set-faces
   '(popup-isearch-match ((t (:inherit isearch)))))

  (defun popup-complete-in-region (next-func start end collection &optional predicate)
    (if (not complete-in-region-use-popup)
        (funcall next-func start end collection predicate)
      (let* ((prefix (buffer-substring start end))
             (completion (try-completion prefix collection predicate)) ;; see if it is sole completion
             (choice (and (stringp completion)
                          (string= completion prefix) ;; if try-completion did not find a unique response (I think)
                          (popup-menu* (all-completions prefix collection predicate) :isearch t :scroll-bar t)))  ;; propose to choose among choices in collection
             (bnds (completion-boundaries pcomplete-stub candidates nil ""))
             (skip (car bnds)) ;; as the completion of ~/Desktop/t is only e.g. tmp, we do not delete ~/Desktop/
             (tail (or (and (stringp completion)
                            (not (string= prefix completion))
                            (substring completion (- end start))) ;; the remaining text needed to fill the word
                       (and (stringp choice)
                            (not (string= prefix choice))
                            (substring choice (- (- end start) skip)))))) ;; the remaining text needed to fill the word
        (cond ((eq completion t)
               (goto-char end)
               (message "Sole completion")
               nil)
              ((null completion)
               (message "No match")
               nil)
              (tail ;; true if tail is string
               (goto-char end)
               (remove-text-properties 0 (length tail) '(face nil) tail) ;; popup-isearch puts face properties in the result string tail
               (insert tail)
               t)
              (choice ;; choice is not a function but is true if it is a string
               (message "Nothing to do")
               nil)
              (t
               (message "completion: something failed!")
               (funcall next-func start end collection predicate)))
        )))
  ;; (add-hook 'completion-in-region-functions 'popup-complete-in-region)

  ;; written with the help of last section in http://www.emacswiki.org/emacs/EshellCompletion#toc4
  ;; Issue: cd Desktop/work/results/
  ;; Issue: when sole completion
  ;; Issue: why can't I type to refine the search during popup
  (defun eshell-popup-completion ()
    "Pop ups a menu with possible completions."
    (interactive)
    ;; stolen from pcomplete.el: allows to get all candidates. pcomplete-stub holds
    ;; the text we are looking for
    (let* ((pcomplete-stub)
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions)))
      ;; (popup-menu* (all-completions pcomplete-stub candidates))
      (popup-complete-in-region nil (pcomplete-begin) (point) candidates)))
  ;; (add-hook 'eshell-mode-hook '(lambda() (local-set-key (kbd "<tab>") 'eshell-popup-completion)))

  (defun ac-pcomplete ()
    ;; eshell uses `insert-and-inherit' to insert a \t if no completion
    ;; can be found, but this must not happen as auto-complete source
    (cl-flet ((insert-and-inherit (&rest args)))
      ;; this code is stolen from `pcomplete' in pcomplete.el
      (let* (tramp-mode ;; do not automatically complete remote stuff
             (pcomplete-stub)
             (pcomplete-show-list t) ;; inhibit patterns like * being deleted
             pcomplete-seen pcomplete-norm-func
             pcomplete-args pcomplete-last pcomplete-index
             (pcomplete-autolist pcomplete-autolist)
             (pcomplete-suffix-list pcomplete-suffix-list)
             (candidates (pcomplete-completions))
             (beg (pcomplete-begin))
             ;; note, buffer text and completion argument may be
             ;; different because the buffer text may bet transformed
             ;; before being completed (e.g. variables like $HOME may be
             ;; expanded)
             (buftext (buffer-substring beg (point)))
             (arg (nth pcomplete-index pcomplete-args)))
        ;; we auto-complete only if the stub is non-empty and matches
        ;; the end of the buffer text
        (when (and (not (zerop (length pcomplete-stub)))
                   (or (string= pcomplete-stub ; Emacs 23
                                (substring buftext
                                           (max 0
                                                (- (length buftext)
                                                   (length pcomplete-stub)))))
                       (string= pcomplete-stub ; Emacs 24
                                (substring arg
                                           (max 0
                                                (- (length arg)
                                                   (length pcomplete-stub)))))))
          ;; Collect all possible completions for the stub. Note that
          ;; `candidates` may be a function, that's why we use
          ;; `all-completions`.
          (let* ((cnds (all-completions pcomplete-stub candidates))
                 (bnds (completion-boundaries pcomplete-stub
                                              candidates
                                              nil
                                              ""))
                 (skip (- (length pcomplete-stub) (car bnds))))
            ;; We replace the stub at the beginning of each candidate by
            ;; the real buffer content.
            (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                    cnds))))))

  (defvar ac-source-pcomplete
    '((candidates . ac-pcomplete)))

  (defun eshell-do-popup-or-ac ()
    "Do completion using auto-complete. If auto-complete does not show completions, press <tab> to force showing possible completions."
    (interactive)
    (if (not (auto-complete))
        (eshell-popup-completion)))

  (add-hook 'eshell-mode-hook #'(lambda () (setq ac-sources '(ac-source-pcomplete))))
  (add-to-list 'ac-modes 'eshell-mode)
  (add-hook 'eshell-mode-hook '(lambda() (local-set-key (kbd "<tab>") 'eshell-do-popup-or-ac)))

  ;; =================================================================================================
  ;; Nice emacs shell prompt
  ;; =================================================================================================
  (defcustom eshell-max-prompt-length 40
    "If non-NIL, complete-in-region will popup a menu with the possible completions."
    :type 'integer
    :group 'eshell)

  (setq eshell-prompt-function (lambda nil
                                 (let* (;; replace /home/user/ with ~/
                                        (eshell-pwd (replace-regexp-in-string (regexp-quote (concat "/home/" user-login-name)) "~" (eshell/pwd)))
                                        (eshell-pwd-length (length eshell-pwd))
                                        ;; shorten string length
                                        (eshell-pwd (substring eshell-pwd (max 0 (- eshell-pwd-length eshell-max-prompt-length)))))
                                   (concat
                                    (propertize eshell-pwd 'face `(:underline t))
                                    (propertize " $" 'face `(:foreground "green"))
                                    " "))))
  (setq eshell-highlight-prompt nil)

  ) ;; display graphic

(provide 'odabai-eshell)
