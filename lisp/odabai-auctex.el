;; C-c C-e : starting new environment
;; C-c C-m : for macros like \footnote
;; M-/     : for completion
(ensure-package-installed 'auctex)
(ensure-package-installed 'reftex)
(ensure-package-installed 'latex-preview-pane)

;; Auto-capitalize: Capitalizes the beginning of a sentence.
;; ATTENTION (emacs 24.3.1):
;;   change the line in file .emacs.d/elpa/auto-capi./auto-capitalize.elc
;;   (defalias 'auto-capitalize #[(beg end length) to
;;   (defalias 'auto-capitalize #[(&optional beg end length)
;;   Do the same in the auto-capitalize.el file.
;; The parameter has to be optional for desktop.el to save/load correctly the current desktop.
(ensure-package-installed 'auto-capitalize)

(if (locate-library "reftex")
    (progn (require 'auto-capitalize)
           (custom-set-variables
            '(inhibit-startup-screen t)
            '(TeX-PDF-mode t))
           (add-hook 'LaTeX-mode-hook 'auto-capitalize-mode)
           (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
           (add-hook 'LaTeX-mode-hook 'flyspell-mode)
           (add-hook 'LaTeX-mode-hook (lambda() (set-variable 'truncate-lines nil)))

           (setq sentence-end-double-space nil)

           ;; --------------------------------------------------------------------------------------
           ;; Reverse and forward search between PDF and TEX
           ;; --------------------------------------------------------------------------------------
           (add-hook 'LaTeX-mode-hook 'server-start)
           ;; (setq TeX-source-correlate-method 'synctex)
           ;; (custom-set-variables '(LaTeX-command "latex -synctex=1"))
           (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
           (setq TeX-view-program-selection
                 '((output-pdf "PDF Viewer")))
           ;; IMPORTANT: to give auto-focus to Okular, I added:
           ;; - && wmctrl -a %o - Okular
           ;; - and you have to disable in Settings/Configure Okular/General/Display document title in title bar
           (setq TeX-view-program-list
                 '(("PDF Viewer" "okular --unique %o#src:%n%b && wmctrl -a %o - Okular #")))
           ))

;; ===============================================================================================
;; Setup org-mode and reftex as research paper management tool
;; https://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/
;; Workflow:
;; A) Adding a paper: Insert paper with title and link to paper file
;;    - Download paper and rename it with its citation reference.
;;    - Put this paper in your paper folder e.g. ~/work/papers/%s.pdf
;;    - Insert it in the file by calling reftex-citation then option choose f
;; B) To search for a paper: Gets you to org entry
;;    - Call org-mode-reftex-search
;;    - Type regexp to select the bib entry
;; C) Explanations:
;;    - reftex-set-cite-format defines how to insert a citation in the buffer
;;    - org-link-abbrev-alist allows to use a short name instead of the link
;;    - org-open-link-from-string opens just a link. As we pass here [[notes:%s]] where %s is
;;      replaced with citation id, the actual link gets expanded based on the abbreviation for
;;      [[notes:]] defined in org-link-abbrev-alist. If we had given [[papers:]], it would have
;;      immediately opened the paper.
;; [
;; ===============================================================================================
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
                                        ;enable auto-revert-mode to update reftex when bibtex file changes on disk
	 (global-auto-revert-mode t)
	 (reftex-parse-all)
                                        ;add a custom reftex cite format to insert links
	 (reftex-set-cite-format
	  '((?p . "[[papers:%l][%l-paper]]")
	    (?t . "%t")
	    (?f . "%t :%y:\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(defun org-mode-reftex-search (&optional what)
  "Jump to the notes for the paper pointed to at from reftex search"
  (interactive "P")
  (if what
      (org-open-link-from-string (format "[[papers:%s]]" (first (reftex-citation t))))
    (org-open-link-from-string (format "[[notes:%s]]" (first (reftex-citation t))))))

(setq org-link-abbrev-alist
      '(("bib"    . "~/Dropbox/thesis/egbib.bib::%s")
	("notes"  . "~/Dropbox/org/thesis.org::#%s")
	("papers" . "~/work/papers/%s.pdf")))
;;]

(provide 'odabai-auctex)
