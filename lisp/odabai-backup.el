;; ====================================================================================================
;; Backups and autosaves
;; ====================================================================================================
(setq make-backup-files nil)  ; I will my own type of backups
(setq auto-save-default t)    ; I prefer to have auto-save in case my program/laptop crashes

(defvar backup-directory "/home/odabai/.emacs.d/backups/") ;; do not give relative paths
(defvar auto-save-directory (concat "/home/odabai/.emacs.d/autosaves" "/"))
(defvar max-backup-dir-size 100) ;; size when erasing the files in backup-directory

(if (not (file-exists-p backup-directory))
        (make-directory backup-directory t))
(if (not (file-exists-p auto-save-directory))
        (make-directory auto-save-directory t))

(when make-backup-files
  (setq backup-directory-alist `(("." . , backup-directory)))
  (setq backup-by-copying t               ; don't clobber symlinks
        version-control t                 ; version numbers for backup files
        delete-old-versions nil           ; ask for confirmation when to delete excess backup files
        delete-by-moving-to-trash nil     ; don't move the old versions to trash bin
        kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
        kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
        ))

(when auto-save-default
  (setq auto-save-file-name-transforms `((".*" ,auto-save-directory t))
        auto-save-default t               ; auto-save every buffer that visits a file
        auto-save-timeout 100             ; number of seconds idle time before auto-save (default:
        auto-save-interval 300))          ; number of keystrokes between auto-saves (default: 300)


(defun do-backup ()
  "Adds a date to the end of the filename and saves it to backup-directory"
  (interactive)
  (if (buffer-file-name)
      (let ((backup-name (replace-regexp-in-string "/" "!" buffer-file-name)))
        (setf backup-name (concat backup-name "~" (format-time-string "%Y%m%d_%H%M")))
        (setf backup-name (concat backup-directory "/" backup-name))
        ;; copy file and if exists ask user to overwrite eventually
        (copy-file buffer-file-name backup-name 1)
        (message (concat "Saved backup file to " (file-name-directory backup-name))))
    (error "Not visiting a file!")))

;; Hint: string-match count
;;       when using a reg. expr. we have parenthetical subexpressions that means parts of the
;;       reg. expr. written in parenthesis  "\\(^[0-9.,]+\\)\\(.\\)*" (\\ is used to allow putting
;;       a ')' afterwards). Here we only have one parenthetical subexpr.: (^[0...]). When we match
;;       a string with reg. expr. it matches a whole string. Subparts matched are those matched
;;       with the parenthesis expr. These subparts can be immmediately extracted by using match-string
;;        count, where here count is 1. If count is 0, we get the whole matched expression.
(defun size-of-directory (dir)
  "Returns the size of directory in Mb"
  (interactive)
  (with-temp-buffer
    (call-process "/usr/bin/du" nil t nil "-BM" dir)
    (re-search-backward "^[0-9]+")
    (string-to-int (match-string 0))))


;; verify that we have enough space for backups
(when (and (> (size-of-directory backup-directory) max-backup-dir-size)
          (y-or-n-p "Backup directory is full. Empty it? "))
  (delete-directory backup-directory t nil)
  (make-directory backup-directory t)
  (message "Backup directory emptied."))

;; =================================================================================================
;; Saving histories between sessions
;; =================================================================================================
(setq savehist-additional-variables                   ;; also save...
      '(search-ring regexp-search-ring)               ;; ... my search entries
      savehist-file (concat dotfiles-dir "savehist")) ;; keep my home clean
(savehist-mode t)                                     ;; do customization before activate

(provide 'odabai-backup)
