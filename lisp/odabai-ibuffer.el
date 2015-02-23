(require 'ibuffer)
;; Defining filter groups so that we group filenames
;; [
(setq ibuffer-saved-filter-groups
      '(("home"
         ("Dired" (mode . dired-mode))
         ("Latex" (or (mode . latex-mode)
                      (mode . bibtex-mode)))
         ("Programming" (or (mode . c++-mode)
                            (mode . python-mode)
                            (mode . emacs-lisp-mode)
                            (mode . shell-mode)))
	 ("emacs-config" (or (filename . ".emacs.d")))
	 ("Org" (or (mode . org-mode)
	            (filename . "OrgMode")))
	 ("Magit" (name . "\*magit"))
	 ("Help" (or (name . "\*Help\*")
	             (name . "\*Apropos\*")
	             (name . "\*info\*")))
         ("Starred" (name . "^\*.*\*$")) )))

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
             (setq ibuffer-expert t)
             (setq ibuffer-show-empty-filter-groups nil)
             (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups "home")))
;;]

;; Use human readable Size column instead of original one
;;[
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))
;;]

;; Have some buffer groups collapsed by default
;;[
(setq mp/ibuffer-collapsed-groups (list "Help"))

(defadvice ibuffer (after collapse-helm)
  (dolist (group mp/ibuffer-collapsed-groups)
    (progn
      (goto-char 1)
      (when (search-forward (concat "[ " group " ]") (point-max) t)
        (progn
          (move-beginning-of-line nil)
          (ibuffer-toggle-filter-group)))))
  (goto-char 1)
  (search-forward "[ " (point-max) t))

(ad-activate 'ibuffer)
;;]

(provide 'odabai-ibuffer)
