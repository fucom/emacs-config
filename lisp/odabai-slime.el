;; =================================================================================================
;; SLIME
;; =================================================================================================
(ensure-package-installed 'slime)
(setq inferior-lisp-program (executable-find "/usr/bin/sbcl"))
(slime-setup)

;; ATTENTION: Slime overrides some keybindings associated to C-c which should be reserverd to user
;; keybindings (http://www.emacswiki.org/emacs/SlimeMode)
;; (add-hook 'slime-mode-hook
;;           (defun slime-sanitize-bindings ()
;;             "Removes SLIME's keybinding on C-c x"
;;             (cond ((boundp 'slime-mode-map)
;;                    ;; FIXME Momentarily until I fix the problem with "not connected"
;;                    (define-key slime-mode-map (kbd "C-c x") nil)
;;                    (define-key slime-mode-map (kbd "C-c C-c") nil)
;;                    (define-key slime-mode-map (kbd "C-c C-k") nil)
;;                    (define-key slime-mode-map (kbd "C-x C-e") nil)
;;                    (message "slime keybinding on C-c x has been sanitized")
;;                    (message "slime keybinding on C-c C-c has been sanitized")
;;                    (message "slime keybinding on C-c C-e has been sanitized")
;;                    (message "slime keybinding on C-c C-k has been sanitized"))
;;                   ('t (message "slime keybindings not sanitized")))))

;; From http://bc.tech.coop/blog/070515.html
(defun lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol currently under the curser"
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

(provide 'odabai-slime)
