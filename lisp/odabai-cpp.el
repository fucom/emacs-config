;; let's assume cuda/opencl is cpp
(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . c++-mode))
;; Just come clean and admit .h files are most often C++
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-c o") 'ff-find-other-file)))
(setq-default c-basic-offset 4)

;; change indention style
;;Mode C
(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "stroustrup")
             (load-library "cc-cmds")
             (c-toggle-hungry-state t)
             (setq c-indent-level 2)
             (setq c-basic-offset 2)
             ))
;;Mode C++
(add-hook 'c++-mode-hook
          '(lambda ()
             (c-set-style "stroustrup")
             (load-library "cc-cmds")
             (c-toggle-hungry-state t)
             (setq c-indent-level 2)
             (setq c-basic-offset 2)
             ))

;; To make the _ not a word separator (i.e. make it a word constituent) for c++ mode, you would do this
;; (modify-syntax-entry ?_ "w" c++-mode-syntax-table)


;; =================================================================================================
;; TAGS
;; =================================================================================================
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f | egrep \"\.(cpp|h|hpp)$\" | xargs etags -a" dir-name)))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

; Add cmake listfile names to the mode list.
(setq auto-mode-alist
        (append
            '(("CMakeLists\\.txt\\'" . cmake-mode))
               '(("\\.cmake\\'" . cmake-mode))
                  auto-mode-alist))
(autoload 'cmake-mode "~/.emacs.d/cmake_auxiliary/cmake-mode.el" t) ;; ??

;; =======================================================================================================
;; Remove completion buffer when done
;; =======================================================================================================
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
            (kill-buffer buffer)))))

;; =======================================================================================
;; Configure compilation mode in emacs
;; =======================================================================================
(defun switch-to-compilation-window ()
  (interactive)
  (switch-to-buffer "*compilation*"))

;;  shortcut for compile command by first setting our compile command
(add-hook 'c++-mode-hook
          (lambda()
            (set (make-local-variable 'compile-command)
                 (concat "g++ --std=c++0x " (buffer-file-name)))))
;; do not keep warnings
(setq compilation-skip-threshold 2)
(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-c v") 'compile)))
(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-c c") 'recompile)))
(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-c q") 'kill-compilation)))
(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-c w") 'switch-to-compilation-window)))
(add-hook 'compilation-mode-hook (lambda() (local-set-key (kbd "C-c v") 'compile)))
(add-hook 'compilation-mode-hook (lambda() (local-set-key (kbd "C-c c") 'recompile)))
(add-hook 'compilation-mode-hook (lambda() (local-set-key (kbd "C-c q") 'kill-compilation)))

;; ---------------------------------------------------------------------------------------------
;; Show compiler messages in big window
;; ---------------------------------------------------------------------------------------------
(defun no-split-window ()
  (interactive)
  nil)
(add-hook 'c-mode-common-hook (lambda () (setq split-window-preferred-function 'no-split-window)))

;; ---------------------------------------------------------------------------------------------
;; Show error messages if any or close window if none
;; ---------------------------------------------------------------------------------------------
(defun kill-compile-buffer-if-successful (buffer string)
  " kill a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer (buffer-name buffer)
          (and (goto-char 0) (re-search-forward ": \\(fatal \\)?error" nil t))))) ; adapt to visual c++ error messages
      (run-with-timer 1 nil (lambda () (switch-to-buffer (other-buffer (current-buffer) 1))))))

(add-hook 'compilation-finish-functions 'kill-compile-buffer-if-successful)
;; Follow compilation output to first error
(setq compilation-scroll-output 'first-error)
;; ... or to the end
;;(setq compilation-scroll-output t)

;; =================================================================================================
;; Doxygen
;; =================================================================================================
(when (require 'doxymacs nil 'noerror)
  (add-hook 'c++-mode-common-hook 'doxymacs-mode)
  (add-hook 'c-mode-common-hook 'doxymacs-mode)
  )

(provide 'odabai-cpp)
