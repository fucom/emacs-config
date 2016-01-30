;; Detect online status
(require 'cl)
(defun esk-online-p ()
  (interactive)
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                              (member 'up (first (last (network-interface-info
                                                        (car iface)))))))
            (network-interface-list))
    t))

;; =================================================================================================
;; scroll one line up/down
;; =================================================================================================
(defun scroll-n-lines-ahead (n)
  "Scroll ahead N lines (1 by default)"
  (interactive "P")
  (scroll-up (prefix-numeric-value n)))

(defun scroll-n-lines-behind (n)
  "Scroll ahead N lines (1 by default)"
  (interactive "P")
  (scroll-down (prefix-numeric-value n)))

;; =================================================================================================
;; Open file in vertical/horizontal split
;; I bind them to C-x 6 v/h
;; TODO It would be nice to have these features:
;; TODO Do not split if no file was selected
;; TODO Let user specify a function through shortcut so it is not only limited to find-file
;; http://www.emacswiki.org/emacs/HorizontalSplitting: split-window-prefer-horizonally
;; =================================================================================================
(defun split-window-vertically-ff ()
  "If there's only one window (excluding any possibly active minibuffer),
   then split the current window horizontally and find file."
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer (call-interactively 'find-file)))

(defun split-window-horizontally-ff ()
  "If there's only one window (excluding any possibly active minibuffer),
   then split the current window horizontally and find file."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (call-interactively 'find-file)))

;; =========================================================================================================
;; Easier line/word deletion
;; =========================================================================================================

;; ----------------------------------------------------------------------------------------------------------
;; delete whole line
;; Usage : C-k         : kill-line
;;         C-u C-k     : delete the line from the beginning
;;         C-u arg C-k : kill-line arg
;; ----------------------------------------------------------------------------------------------------------
(defun delete-line (arg)
  (interactive "P")
  (if (consp arg)
      (kill-whole-line)
    (kill-line arg)))

;; ----------------------------------------------------------------------------------------------------------
;; My version for deleting a word forward and the complete word.
;; Usage: M-d       : kill-word
;;        C-u M-d   : kill the whole word from beginning
;;        C-u arg M-d : kill-word arg
;; Hint:
;; with the "P" option to interactive, if no prefix integer value is given,
;; the raw value of the arg when typing C-u M-x ... is a list.
;; If a prefix argument is given e.g. C-u 4 M-x ... then the argument has a
;; value. If the value is 0, we kill the word, otherwise we call kill-word
;; with this parameter.
;; ATTENTION: M-d does not work here asasfas-def-ghi starting from the 'a' letter.
;; ----------------------------------------------------------------------------------------------------------
(defun delete-word (arg)
  (interactive "P")
  (cond ((consp arg) ;; e.g C-u
         (kill-word 1))
        (arg ;; e.g. C-u 3
         (kill-word arg))
        ( ;; no prefix key
         (let ((start (point)))
           (forward-word)
           (backward-word)
           (if (< start (point))
               (goto-char start))
           (kill-word 1)))))

;; ===============================================================================================
;; Faster copying
;; ===============================================================================================
;; -----------------------------------------------------------------------------------------------
;; Base functions
;; -----------------------------------------------------------------------------------------------
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )

;; I do not use paste-to-mark as one needs to specify where to paste before copying text
(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe
         (lambda()
           (if (string= "shell-mode" major-mode)
               (progn (comint-next-prompt 25535) (yank))
             (progn (goto-char (mark)) (yank) )))))
    (if arg
        (if (= arg 1)
            nil
          (funcall pasteMe))
      (funcall pasteMe))
    ))
;; -----------------------------------------------------------------------------------------------
;; Copy Word
;; -----------------------------------------------------------------------------------------------
(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "p")
  (copy-thing 'backward-word 'forward-word arg)
  (message "%d word%s copied" arg (if (= 1 arg) "" "s"))
  ;; (paste-to-mark arg)
  )
;; -----------------------------------------------------------------------------------------------
;; Copy Line
;; Hint:
;; beginning-of-line       : moves to the beginning of the current or +arg line
;; end-of-line             : moves to the end of the curreutn or +arg line
;; line-beginning-position : return the POSITION of the beginning of the current or +arg line
;; -----------------------------------------------------------------------------------------------
(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "p")
  (copy-thing 'beginning-of-line 'beginning-of-line (+ 1 arg))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s"))
  ;; (paste-to-mark arg)
  )


(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t)
        (message "Refreshing : %s" buffer))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))

(defun revert-buffer-func ()
  "Refreshes all open buffers or the current buffer"
  (interactive)
  (if (y-or-n-p (format "Refresh all buffers?"))
      (revert-all-buffers)
    (progn
      (revert-buffer t t t)
      (message "Refreshed current buffer"))))

;; =======================================================================================================
;; Fast alignment of equal signs
;; Hint on align-regexp:
;; - regexp: match the place you are interested in aligning; to do it, one of its parenthesis groups
;; will be extended with spaces, or shortened by deleting characters
;; - parenthesis group: choose which one
;; - spacing: if the group is shorter than this, spaces will be added to it; if it's longer,
;; characters will be deleted from it, starting at the end (unless it's longer for the purposes of
;; alignment, of course)
;; - repeat: well, this is obvious, I think
;; - \s- stands for whitespace char in emacs lisp
;; I do not use (interactive "r") as proposed in orignal solution as it uses points instead of markers.
;; =======================================================================================================
(defun align-equal-sign ()
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\) =" 1 0 1))
(add-hook 'prog-mode-hook (lambda() (local-set-key (kbd "C-c =") 'align-equal-sign)))

;; =================================================================================================
;; Undoing unvolanteering scrolls
;; =================================================================================================
(put 'scroll-up-command    'unscrollable t)
(put 'scroll-down-command  'unscrollable t)
(put 'scroll-left          'unscrollable t)
(put 'scroll-right         'unscrollable t)

(defvar unscroll-point (make-marker)
  "Cursor position for next call to 'undo-scrolling'.")

(defvar unscroll-window-start (make-marker)
  "Window start for next call to 'undo-scrolling'.")

(defvar unscroll-hscroll nil
  "Hscroll for next call to 'unscroll'.")

(defun unscroll-remember-maybe ()
  (if  (not (get last-command 'unscrollable))
      (progn
        (set-marker unscroll-point (point))
        (set-marker unscroll-window-start (window-start))
        (setq unscroll-hscroll (window-hscroll)))))

(defun undo-scrolling ()
  "Revert to 'unscroll-point' and 'unscroll-window-start'."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

(defadvice scroll-up-command (before remember-for-scroll
                              activate compile)
  (unscroll-remember-maybe))

(defadvice scroll-down-command (before remember-for-scroll
                              activate compile)
  (unscroll-remember-maybe))

(defadvice scrolll-left (before remember-for-scroll
                              activate compile)
  (unscroll-remember-maybe))

(defadvice scroll-right (before remember-for-scroll
                              activate compile)
  (unscroll-remember-maybe))

;; =================================================================================================
;; ;; Move selected region using keyboard keys
;; =================================================================================================
(defun move-region (arg)
  "Move region down (arg=2) or up (arg=0). Attention: You have to select from top left to bottom right."
  (setq distance (- (region-end) (region-beginning)))

  (setf beg-region (make-marker))
  (set-marker beg-region (region-beginning))
  (setq end-region (region-end))

  (goto-char (if (eq arg 2) (region-end) (region-beginning)))
  (setq beg-next-line (line-beginning-position arg)
        end-next-line (line-end-position arg))

  (transpose-regions beg-region end-region beg-next-line end-next-line)

  ;; re-select the region if command is repeated
  (goto-char beg-region)
  (set-mark-command nil)
  (goto-char (+ beg-region distance))
  (setq deactivate-mark nil))

(defun move-region-or-next-line ()
  "Move selected region."
  (interactive)
  (cond ((not (use-region-p)) (next-line))
        (t (move-region 2))))

(defun move-region-or-previous-line ()
  "Move selected region."
  (interactive)
  (cond ((not (use-region-p)) (previous-line))
        (t (move-region 0))))

;; ===============================================================================================
;; more efficient commenting: comment region or current line if no region specified
;; ===============================================================================================
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (use-region-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; replace C-j with go to new line and indent. This allows to go to new line
;; without going to end of line and do C-j.
(defun control-j-newline-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun create-or-kill-eshell (&optional arg)
  "Go to eshell or kill it if in eshell mode."
  (interactive "P")
  (if (and  (null arg) (derived-mode-p 'eshell-mode))
      (kill-buffer)
    (eshell arg)))

(defun erase-eshell-buffer ()
  "Erase eshell buffer and re-create the input."
  (interactive)
  (erase-buffer)
  (eshell-send-input))

;; Use prefix argument to C-c r (C-u C-c r) to start iedit that means to search/replace for a
;; pattern and change it simultaneously in all other occurances.
(defun search-replace-simultaneously (&optional arg)
  "Search for a string if no prefix argument is provided. If a
  prefix argument is used, search for word at point and replace
  it simultaneously in the whole buffer."
  (interactive "P")
  (if (consp arg)
      (iedit-mode)
    (call-interactively 'replace-string)))

;; if you need to open a root file for modification
(defun find-alternate-file-with-sudo ()
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

;; http://www.emacswiki.org/emacs/AutoIndentation
(defadvice kill-line (before remove-whitespaces activate)
  "If at end of line, join with following; otherwise kill line.
   Deletes whitespace at join."
  (and (derived-mode-p 'prog-mode)
       (eolp)
       (not (bolp))
       (progn
         (forward-char 1)
         (just-one-space 1)
         (backward-char 2))))

;; indent when I yank code in programming languages
;; http://www.emacswiki.org/emacs/AutoIndentation
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (derived-mode-p 'prog-mode)
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(defun dos2unix ()
  "Convert current buffer to unix format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix 't))

(defun unix2dos ()
  "Convert current buffer to dos format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos 't))

;; http://stackoverflow.com/questions/17325713/looking-for-a-replace-in-string-function-in-elisp
(defun replace-in-string (what with in)
  "Replaces a all occurances of \"what\" with \"with\" in string \"in\"."
  (replace-regexp-in-string (regexp-quote what) with in))

;; list minor modes
(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))

;; finding overlays TODO Only works when called with keybindings
;; (defun find-overlays-specifying (prop)
;;   (let ((overlays (overlays-at (point)))
;;         found)
;;     (while overlays
;;       (let ((overlay (car overlays)))
;;         (if (overlay-get overlay prop)
;;             (setq found (cons overlay found))))
;;       (setq overlays (cdr overlays)))
;;     found))

;; =================================================================================================
;; Emacs 24.4 pop ups a new emacs instance (frame) when going to an error during the compilation
;; process. The following defun overrides the default display-buffer-use-some-window function to
;; allow open the file in the same frame.
;; =================================================================================================
(if (and (>= emacs-major-version 24) (> emacs-minor-version 3))
    (defun display-buffer-use-some-window (buffer alist)
      (let* ((not-this-window (cdr (assq 'inhibit-same-window alist)))
             (frame (or (window--frame-usable-p (selected-frame))
                        (window--frame-usable-p (last-nonminibuffer-frame))))
             (window
              ;; Reuse an existing window.
              (or (get-lru-window frame nil not-this-window)
                  (let ((window (get-buffer-window buffer 'visible)))
                    (unless (and not-this-window
                                 (eq window (selected-window)))
                      window))
                  (get-largest-window 'visible nil not-this-window)
                  (let ((window (get-buffer-window buffer 0)))
                    (unless (and not-this-window
                                 (eq window (selected-window)))
                      window))
                  (get-largest-window 0 not-this-window))))
        (when (window-live-p window)
          (prog1
              (window--display-buffer buffer window 'reuse alist)
            (window--even-window-heights window)
            (unless (cdr (assq 'inhibit-switch-frame alist))
              (window--maybe-raise-frame (window-frame window))))))))


;; =================================================================================================
;; Wrap isearch at end of file
;; http://stackoverflow.com/questions/285660/automatically-wrapping-i-search
;; WARNING It sleeps for 0.3 seconds when search failed.
;; TODO: Needs improvement especially with the face stuff to indicate that we wrapped.
;; =================================================================================================
(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (copy-face 'isearch 'isearch-original)
    (set-face-attribute 'isearch nil :inherit 'isearch :foreground "yellow" :weight 'bold)
    (isearch-repeat (if isearch-forward 'forward))
    (sleep-for 0.3)
    (copy-face 'isearch-original 'isearch)
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    ;; this call needs to be made to update as we enabled an advice
    (ad-activate 'isearch-repeat)))

;; =================================================================================================
;; Return a circular list
;; =================================================================================================
(defun odabai--make-circular (items)
  "Returns the list in items as a circular list that is the last element is pointing to its
beginning."
  (setq items-circular (copy-list items))
  (setf (cdr (last items-circular)) items-circular)
  items-circular)


;; =================================================================================================
;; Ack
;; http://www.emacswiki.org/emacs/Ack
;; Note: Works poorly
;; =================================================================================================
;;[
(defvar ack-history nil
  "History for the `ack' command.")

(defun ack (command-args)
  (interactive
   (let ((ack-command "ack "))
     (list (read-shell-command "Run ack (like this): "
                               ack-command
                               'ack-history))))
  (let ((compilation-disable-input t))
    (compilation-start (concat command-args " < " null-device)
                       'grep-mode)))
;;]

;; Open line above/below and place me there
;; Can be improved using numeric prefix C-u 9 odabai/goto... to create 9 lines.
(defun odabai/goto-created-newline (&optional arg)
  "Create a new line above and place point to that position.
Using a prefix argument create the newline below."
  (interactive "P")
  (cond (arg
         (move-end-of-line 1)
         (newline-and-indent))
        (t
         (move-beginning-of-line 1)
         (split-line)
         (indent-for-tab-command)) ))

(provide 'odabai-defuns)
