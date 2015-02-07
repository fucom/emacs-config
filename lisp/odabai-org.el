;; =============================================================================================
;; org-mode
;; =============================================================================================
(ensure-package-installed 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-mode-hook (lambda() (local-set-key (kbd "C-c a") 'org-agenda)))
(add-hook 'org-mode-hook (lambda() (local-set-key (kbd "C-c l") 'org-store-link)))
;; where auto-fill should wrap
(add-hook 'org-mode-hook (lambda() (setq fill-column 80)))
;; do line wrap
(add-hook 'org-mode-hook (lambda() (set-variable 'truncate-lines nil)))
(setq org-log-done t)
;; do not show schedules TODOs once they are done
(setq org-agenda-skip-scheduled-if-done t)

(setq org-directory (expand-file-name "~/Dropbox/org/"))
;; agenda files
(setq org-agenda-files (list "~/Dropbox/org/general.org"
                             "~/Dropbox/org/thesis.org"))
(setq org-default-notes-file (concat org-directory "notes.org")
      org-contacts-files (list (concat org-directory "contacts.org"))
      org-journal-dir (concat org-directory "journal/"))

;; You can cycle through todo subsets using C-S left/right
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE")
        (sequence "CODE" "LAUNCH" "COLLECT" "FIX" "EVALUATE" "|" "DONE" "CANCELED") ;; it does not go to this cancel
        (sequence "|" "CANCELED")))

;; ---------------------------------------------------------------------------------------------
;; keybindings
;; ---------------------------------------------------------------------------------------------
(global-set-key "\C-cc" 'org-capture)

;; ---------------------------------------------------------------------------------------------
;; Function on how to react to appt (appointment tool of emacs)
;; ---------------------------------------------------------------------------------------------
;; A sound file could be:
;;         "/usr/share/sounds/ubuntu/stereo/phone-incoming-call.ogg"
(defun org-mode-notify-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played"
  (interactive)
  (when sound (shell-command
               (concat "ffplay -autoexit -nodisp -loglevel panic " sound " 2> /dev/null")))
  (if (eq window-system 'x)
      (shell-command (concat "notify-send "
                             (if icon (concat "-i " icon) "")
                             " '" title "' '" msg "'"))
    ;; text only version
    (message (concat title ": " msg))))

;; Configure properties of the appointment notification facility
(setq
 appt-message-warning-time 15 ;; warn 15 min in advance
 appt-display-mode-line t     ;; show in the modeline
 appt-display-format 'window) ;; use our func

;; -----------------------------------------------------------------------------------------------
;; Update appt system of emacs
;; -----------------------------------------------------------------------------------------------
;; update appt each time agenda opened
;; hook run when we compile agenda view (C-c a a)
;; (add-hook 'org-finalize-agenda-hook 'xyz)
;; Hint:
;;     add-hook arg1 arg2 &optional append local
;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook 'org-agenda-to-appt nil t)))

;; update appt each time we enter a new deadline/schedule
(defadvice org-time-stamp (after update-my-appt activate compile)
  "Update appt from within org when we create a new time stamp"
  (if (derived-mode-p 'org-mode)
      (org-agenda-to-appt)))

(defadvice org-deadline (after update-my-appt activate compile)
  "Update appt from within org when we create a new time stamp"
  (if (derived-mode-p 'org-mode)
      (org-agenda-to-appt)))

(defadvice org-schedule (after update-my-appt activate compile)
  "Update appt from within org when we create a new time stamp"
  (if (derived-mode-p 'org-mode)
      (org-agenda-to-appt)))

;; -----------------------------------------------------------------------------------------------
;; Set how to react on an appointment
;; -----------------------------------------------------------------------------------------------
;; our little façade-function for org-mode-notify-popup
(defun org-mode-appt-display (min-to-app new-time msg)
  (org-mode-notify-popup (format "Appointment in %s minute(s)" min-to-app) msg
                         "/usr/share/icons/gnome/32x32/status/appointment-soon.png"))

;; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil (lambda ()
                           (setq appt-time-msg-list nil)
                           (org-agenda-to-appt)))

(setq appt-disp-window-function (function org-mode-appt-display))
;; I do not need org apppointment right npw
;; (appt-activate 1)              ;; active appt (appointment notification)
;; (org-agenda-to-appt) ;; compile today's tasks



;; Execute the `odabai/org-include-img-from-pdf' function just before saving the file
(add-hook 'before-save-hook #'odabai/org-include-img-from-pdf)
;; Execute the `odabai/org-include-img-from-pdf' function before processing the
;; file for export
(add-hook 'org-export-before-processing-hook #'odabai/org-include-img-from-pdf)

(defun odabai/org-include-img-from-pdf (&rest ignore)
"Convert the pdf files to image files. Only looks at #HEADER: lines that have \":convertfrompdf t\".
This function does nothing if not in org-mode, so you can safely add it to `before-save-hook'.

An example usage goes like this:
     ...
#+HEADER: :convertfrompdf t
\[\[/home/odabai/Dropbox/thesis/optimization/figs/results_runtime_map/org-conv-imgs/base_voc07.png\]\]
     ...
The command will create the folder \"org-conv-imgs\" for you and place the image \"base_voc07.png\"
in it."
  (interactive)
  (defconst org-conv-img "org-conv-imgs/")
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+HEADER:.*\\s-:convertfrompdf\\s-+t"
              nil 'noerror)
        ;; narrow to the current element
        (org-narrow-to-element)
        (let* (filenoext imgext imgfile pdffile cmd filedir)
          ;; find a line with the specified image file `[[FILE.EXT]]'
          ;; EXT must be an image extension
          (search-forward-regexp "\\[\\[\\(.*\\)\\.\\(.*\\)\\]\\]" nil 'noerror)
          (setq filenoext (match-string-no-properties 1))
          (setq imgext    (match-string-no-properties 2))
          (setq filedir   (file-name-directory filenoext))

          ;; stripp off org-conv-img folder name extension
          (setq file-info (split-string filenoext org-conv-img))

          ;; make sure we have a valid image extension
          (unless (member imgext '("png" "jpg" "jpeg" "tiff") )
            (error (concat "Wrong image extension: " imgext)))

          ;; find full path
          (setq imgfile (expand-file-name (concat filenoext "." imgext)))
          (setq pdffile (expand-file-name (concat (car file-info) (cadr file-info) "." "pdf")))

          (setq cmd (concat "convert -density 96 -quality 85 " pdffile " " imgfile))
          ;; only update if we refreshed the pdf file or the img file does not exist
          (when (file-newer-than-file-p pdffile imgfile)
            (unless (file-exists-p filedir)
              (message (concat "Creating directory: " filedir))
              (make-directory filedir t))

            (message "%s" cmd)
            (shell-command cmd)))
        (widen)))))

(provide 'odabai-org)
