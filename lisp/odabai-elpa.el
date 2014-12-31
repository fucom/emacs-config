;; =================================================================================================
;; ELPA
;; =================================================================================================
(require 'package)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))
       package))
  packages))

(setq package-user-dir (concat dotfiles-dir "elpa"))

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")
                          ("melpa" . "http://melpa.milkbox.net/packages/")))

;; does not really work as it does not put required packages of each package in the load-path
(defun require-package (&rest packages)
  "Load required packages to load-path and require that package. If package is missing, it will be installed."
  ;; ensure all packages are installed
  (apply #'ensure-package-installed packages)

  (mapcar (lambda (package-dir)
            (if (file-directory-p package-dir)
                (add-to-list 'load-path package-dir)))
          (directory-files package-user-dir t (concat "^\\("
                                                      (mapconcat 'symbol-name packages ".*\\|")
                                                      "\\)")))
  (dolist (package packages)
    (require package)))

;; do not load slime
(push '(slime nil) package-load-list)
;; Activate installed packages.
(package-initialize nil)
;; update packages
(when (esk-online-p)
  (unless package-archive-contents (package-refresh-contents)))

(provide 'odabai-elpa)
