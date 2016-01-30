(ensure-package-installed 'auto-complete)
(ensure-package-installed 'auto-complete-c-headers)

(require 'auto-complete)
;; do default config for auto-complete
(require 'auto-complete-config)
;; we want to navigate in the ac menu with C-n/p
(setq ac-use-menu-map t)

(dolist (mode '(lisp-interaction-mode
                lisp-mode
                emacs-lisp-mode
                inferior-emacs-lisp-mode
                slime-repl-mode
                c-mode-common-hook))
  (add-to-list 'ac-modes mode))
;; turn ac for the modes in ac-modes on
(global-auto-complete-mode t)

;; copied from auto-complete-config.el. This enables auto-config to know actually the emacs
;; function and variables.
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)

(setq ac-auto-show-menu t)              ; show display menu w/o asking for it
(setq ac-delay          0)              ; show display menu immediately

;; =================================================================================================
;; C/C++ configuration
;; I will configure ac for c/cpp when the time is right.
;; =================================================================================================
;; http://barisyuksel.com/cppmode/.emacs_irony
;; let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun odabai/ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; the below line is obtained by gcc "-xc++ -E -v -"
  ;;(add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/usr/llvm-gcc-4.2/lib/gcc/i686-apple-darwin11/4.2.1/include")
  )
;; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'odabai/ac-c-header-init)
(add-hook 'c-mode-hook 'odabai/ac-c-header-init)

;; fix an auto-complete-mode and linum-mode annoyance
(ac-linum-workaround)

(provide 'odabai-ac)
