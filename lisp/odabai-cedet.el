(ensure-package-installed 'cedet)

;; turn on semantic mode
(semantic-mode 1)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)
;;(semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)

;; let's define a function which adds semantic as a suggestion backend to auto complete
;; and hook this function to c-mode-common-hook
(defun odabai/add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook 'odabai/add-semantic-to-autocomplete)

;; ;; Semantic
;; ;; (global-semantic-decoration-mode)
;; ;; (global-semantic-highlight-func-mode)

(provide 'odabai-cedet)
