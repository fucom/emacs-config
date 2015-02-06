;; Where I keep all my lisp and configurations for emacs
(setq dotfiles-dir (expand-file-name "/home/odabai/.emacs.d/"))

(add-to-list 'load-path (concat dotfiles-dir "lisp"))

;; configuration for various modes
(require 'odabai-elpa)
(require 'odabai-defuns)
(require 'odabai-keybindings)

(ensure-package-installed 'ace-jump-mode)
(require 'tty-format)
(add-hook 'find-file-hooks 'tty-format-guess)

(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-last-directory-history nil)
(setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
(setq ido-show-dot-for-dired t) ;; put . as the first item

