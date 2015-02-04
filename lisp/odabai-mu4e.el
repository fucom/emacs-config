;; Installation instructions obtained from: https://gist.github.com/areina/3879626
;; - install offlineimap
;; - copy .offlineimaprc to ~/
;; - copy .offlineimap.py
;; - create ~/.authinfo with your password content:
;;   machine imap.gmail.com login areina0@gmail.com port 993 password blabla123bla456
;;   machine smtp.gmail.com login areina0@gmail.com port 587 password blabla123bla456
;; - and encrypt it with:
;;   M-x epa-encrypt-file
;;   This will generate ~/.authinfo.gpg
;; - Run offlineimap and take a beer
;; - Download latest version of mu
;; - mu index --maildir=~/Maildir
;; - copy odabai-mu4e.el to your your own lisp folder
;; WARNING You need the latest mu version I used 0.9.9.6 for several reasons:
;; - to correctly index long email adresses
;; - to correctly display letters in the mu4e menu during emacs session

;; mu4e is installed by default with mu
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)
;; default
(setq mu4e-maildir (expand-file-name "~/Maildir"))

(setq mu4e-drafts-folder "/my_gmail_drafts")
(setq mu4e-sent-folder   "/my_gmail_drafts")
(setq mu4e-trash-folder  "/my_gmail_trash")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
;; I don't use a signature...
(setq
 user-mail-address "ofhamid@gmail.com"
 user-full-name  "Hamid Odabai"
 ;; message-signature
 ;;  (concat
 ;;    "Foo X. Bar\n"
 ;;    "http://www.example.com\n")
 )

;; make sure the gnutls command line utils are installed package 'gnutls-bin' in Debian/Ubuntu
(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

;; =================================================================================================
;; Auto complete email adresses
;; http://emacs.stackexchange.com/questions/4209/using-ido-or-helm-to-auto-complete-email-addresses-in-mu4e
;; =================================================================================================
(defun select-and-insert-contact (&optional start)
  (interactive)
  (let ((mail-abbrev-mode-regexp mu4e~compose-address-fields-regexp)
        (eoh ;; end-of-headers
         (save-excursion
           (goto-char (point-min))
           (search-forward-regexp mail-header-separator nil t))))
    (when (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
      (let* ((end (point))
             (start
              (or start
                  (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point))))
             (contact
              (ido-completing-read "Contact: "
                                   mu4e~contacts-for-completion
                                   nil
                                   nil
                                   (buffer-substring-no-properties start end))))
        (unless (equal contact "")
          (kill-region start end)
          (insert contact))))))
;; Replace mu4e completion method with ours
(defalias 'mu4e~compose-complete-contact 'select-and-insert-contact)

(provide 'odabai-mu4e)
