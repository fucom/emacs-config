;; Where I keep all my lisp and configurations for emacs
(setq dotfiles-dir (expand-file-name "/home/odabai/.emacs.d/"))

(add-to-list 'load-path (concat dotfiles-dir "lisp"))

;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)

(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)

;; font
(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; no tabs
(setq-default indent-tabs-mode nil)

;; configuration for various modes
(require 'odabai-elpa)
(require 'odabai-defuns)
(require 'odabai-keybindings)

(ensure-package-installed 'ace-jump-mode)
(require 'tty-format)
(add-hook 'find-file-hooks 'tty-format-guess)

;; mark down mode (.md)
(ensure-package-installed 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd\\'" . markdown-mode))

;; use ido
(require 'odabai-ido)
;; enable viewing .m files
(require 'odabai-matlab)

;; (require 'moe-theme)
;; (require 'zenburn-theme)
;; (load-theme 'zenburn t) ;; t is important to avoid asking for confirmation
(ensure-package-installed 'color-theme-odabai-solarized)
(load-theme 'odabai-solarized-dark t)
