;; as I do not know how to do correct if..else, I wrote it the ugly way
(if (display-graphic-p)
    (progn
      (defface buffer-file-name-face
        '(
          (((class color) (min-colors 8))
           :foreground "#F2804F"))
        "Basic face for highlight buffer name in modline."
        ))
  (progn
    (defface buffer-file-name-face
      '(
        (((class color) (min-colors 8))
         :background "color-111" :foreground "color-196"))
      "Basic face for highlight buffer name in modline."
      )))

(defface dir-name-face
  '(
    (((class color) (min-colors 8))
     :foreground "#859900" :inverse-video nil))
  "Basic face for highlight directory name in modline.")

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(setq prog-mode-line-format
      (list
       "["
       '(:eval (propertize (shorten-directory default-directory 40)
                           'face 'dir-name-face))
       ;; the buffer name; the file name as a tool tip
       '(:eval (propertize "%b" 'face 'buffer-file-name-face
                           'help-echo (buffer-file-name)))
       "]"

       ;; ;; line and column
       ;; " (" ;; '%02' to set to 2 chars at least; prevents flickering
       ;; (propertize "%l" 'face 'font-lock-type-face) ","
       ;; (propertize "%c" 'face 'font-lock-type-face)
       ;; ") "

       ;; if which-func-mode is in effect, display which
       ;; function we are currently in.
       '(which-func-mode (" " which-func-format))

       ;; relative position, size of file
       "["
       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
       "/"
       (propertize "%I" 'face 'font-lock-constant-face) ;; size
       "]"

       ;; the current major mode for the buffer.
       "["
       '(:eval (propertize "%m" 'face 'font-lock-string-face
                           'help-echo buffer-file-coding-system))
       "]"

       "[" ;; insert vs overwrite mode, input-method in a tooltip
       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                           'face 'font-lock-preprocessor-face
                           'help-echo (concat "Buffer is in "
                                              (if overwrite-mode "overwrite" "insert") " mode")))

       ;; was this buffer modified since the last save?
       '(:eval (when (buffer-modified-p)
                 (concat ","  (propertize "Mod"
                                          'face 'font-lock-warning-face
                                          'help-echo "Buffer has been modified"))))

       ;; is this buffer read-only?
       '(:eval (when buffer-read-only
                 (concat ","  (propertize "RO"
                                          'face 'font-lock-type-face
                                          'help-echo "Buffer is read-only"))))
       "] "

       ;; alert if file coding system is dos or mac
       '(:eval (let ((buffer-coding-system-string (symbol-name buffer-file-coding-system)))
                 (when (or (string-match "dos" buffer-coding-system-string) (string-match "mac" buffer-coding-system-string))
                   (concat "(" (propertize buffer-coding-system-string 'face 'font-lock-type-face) ")"))))

       ;; add the time, with the date and the emacs uptime in the tooltip
       ;; '(:eval (propertize (format-time-string "%H:%M")
       ;;                     'help-echo
       ;;                     (concat (format-time-string "%c; ")
       ;;                             (emacs-uptime "Uptime:%hh"))))

       ;; i don't want to see minor-modes; but if you want, uncomment this:
       ;; minor-mode-alist  ;; list of minor modes
       "% " ;; fill with '-'
       ))

;; let solarized color theme set modeline colors
;; (if (display-graphic-p)
;;     (set-face-foreground 'mode-line "black")
;;   (set-face-foreground 'mode-line "white"))
;; (if (display-graphic-p)
;;     (set-face-background 'mode-line "white")
;;   (set-face-background 'mode-line "brightblack"))

(add-hook 'prog-mode-hook
          (lambda ()
            (setq mode-line-format prog-mode-line-format)))

(provide 'odabai-modeline)
