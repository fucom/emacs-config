;; Who said that appearance doesn't matter

;; =================================================================================================
;; Basic appearance
;; =================================================================================================
;; font and font size
;; (set-face-attribute 'default nil :height 120)
;; you can get the name of fonts by running the following in the minibuffer:
;; set-default-font
(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; no tabs
(setq-default indent-tabs-mode nil)

;; disable toolbar/menubar/scrollbar
(dolist (mode '(tool-bar-mode menu-bar-mode scroll-bar-mode))
  (if (fboundp mode) (funcall mode -1)))

;; no splash screen
(setq inhibit-splash-screen t)

;; display time in modeline
;; (display-time)

;;  press y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; We have a large screen
(setq-default fill-column 100)

;; cursor properties
(set-face-background 'cursor "red")
(blink-cursor-mode -1)

;; Nice emascs modeline
(require 'odabai-modeline)
(which-function-mode 1)

;;  Customize text mode
(add-hook 'text-mode-hook (lambda() (set-variable 'truncate-lines t)))
(add-to-list 'auto-mode-alist '("README" . text-mode))
(add-to-list 'auto-mode-alist '("\.txt$" . text-mode))

(require 'linum)
;; (global-linum-mode 1)
(setq linum-format "%d ")
;; to see the current line number hightlighted
(ensure-package-installed 'hlinum)
(hlinum-activate)
;; enable/disable line numbers for specific modes
(add-hook 'prog-mode-hook (lambda () (linum-mode 1)))
(add-hook 'text-mode-hook (lambda () (linum-mode 1)))
(add-hook 'org-mode-hook (lambda () (linum-mode -1)))

;; =================================================================================================
;; Color customization for solarized
;; =================================================================================================

(ensure-package-installed 'color-theme)
(require 'odabai-color-theme-solarized)

;; -------------------------------------------------------------------------------------------------
;; Synchronise color theme
;; -------------------------------------------------------------------------------------------------
(setq current-theme-color nil)
(defun synchronize-theme ()
  ;; get time
  (setq hour 
        (string-to-number 
         (substring (current-time-string) 11 14)))
  ;; find out best current theme
  (if (member hour (number-sequence 11 12))
      (setq new-color '(color-theme-solarized-light))
    (setq new-color '(color-theme-solarized-dark)))
  ;; update background
  (if (eq new-color current-theme-color)
      nil
    (progn
      (setq current-theme-color new-color)
      (eval current-theme-color)
                                        ;(set-face-background 'show-paren-match-face (darken-my-color 'default :background )))))
      )))

;; update every hour the theme
;; (run-with-timer 0 3600 'synchronize-theme)
;; for strange reasons, I have to call this again otherwise it won't work on terminator
;; (synchronize-theme)

;; =================================================================================================
;; Highlights several events: such as regions that were undo-ed.
;; =================================================================================================
(ensure-package-installed 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; unfortunately I constantly work in dark environments right now
(color-theme-solarized-dark)


;; =================================================================================================
;; Colorise .txt files
;; =================================================================================================
;; (require 'ansi-color)
;; (defun ansi-color-apply-on-region-int (beg end)
;;   "interactive version of func"
;;   (interactive "r")
;;   (ansi-color-apply-on-region beg end))
;; (define-derived-mode fundamental-ansi-mode fundamental-mode "fundamental ansi"
;;   "Fundamental mode that understands ansi colors."
;;   (require 'ansi-color)
;;   (ansi-color-apply-on-region (point-min) (point-max)))
;; (setq auto-mode-alist
;;       (cons '("\\.txt\\'" . fundamental-ansi-mode) auto-mode-alist))

;; opens file only in read-only
(require 'tty-format)
(add-hook 'find-file-hooks 'tty-format-guess)

(provide 'odabai-theme)


