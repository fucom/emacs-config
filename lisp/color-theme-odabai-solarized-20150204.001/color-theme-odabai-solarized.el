;; https://github.com/purcell/color-theme-sanityinc-solarized/blob/master/color-theme-sanityinc-solarized.el
;;; TODO - backward compatible to previous Emacs versions
;;;      - sRGB
;;;      - colors global
;;; Code

(defcustom solarized-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'solarized)
(defcustom solarized-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'solarized)
(defcustom solarized-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'solarized)
(defcustom solarized-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'solarized)
(defcustom solarized-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'solarized)

;;; Color Palette
(defmacro color-theme-odabai-solarized--with-colors (mode &rest body)
  `(let* ((base03 "#002b36")
         (base02 "#073642")
         (base01 "#586e75")
         (base00 "#657b83")
         (base0  "#839496")
         (base1  "#93a1a1")
         (base2  "#eee8d5")
         (base3  "#fdf6e3")
         (yellow "#b58900")
         (orange "#cb4b16")
         (red    "#dc322f")
         (magenta"#d33682")
         (violet "#6c71c4")
         (blue   "#268bd2")
         (cyan   "#2aa198")
         (green  "#859900")

         (yellow-d  "#7B6000")
         (yellow-l  "#DEB542")
         (orange-d  "#8B2C02")
         (orange-l  "#F2804F")
         (red-d     "#990A1B")
         (red-l     "#FF6E64")
         (magenta-d "#93115C")
         (magenta-l "#F771AC")
         (violet-d  "#3F4D91")
         (violet-l  "#9EA0E5")
         (blue-d    "#00629D")
         (blue-l    "#69B7F0")
         (cyan-d    "#00736F")
         (cyan-l    "#69CABF")
         (green-d   "#546E00")
         (green-l   "#B4C342")

         (yellow-hc (if (eq ,mode 'light) yellow-d yellow-l))
         (yellow-lc (if (eq ,mode 'light) yellow-l yellow-d))
         (orange-hc (if (eq ,mode 'light) orange-d orange-l))
         (orange-lc (if (eq ,mode 'light) orange-l orange-d))
         (red-hc (if (eq ,mode 'light) red-d red-l))
         (red-lc (if (eq ,mode 'light) red-l red-d))
         (magenta-hc (if (eq ,mode 'light) magenta-d magenta-l))
         (magenta-lc (if (eq ,mode 'light) magenta-l magenta-d))
         (violet-hc (if (eq ,mode 'light) violet-d violet-l))
         (violet-lc (if (eq ,mode 'light) violet-l violet-d))
         (blue-hc (if (eq ,mode 'light) blue-d blue-l))
         (blue-lc (if (eq ,mode 'light) blue-l blue-d))
         (cyan-hc (if (eq ,mode 'light) cyan-d cyan-l))
         (cyan-lc (if (eq ,mode 'light) cyan-l cyan-d))
         (green-hc (if (eq ,mode 'light) green-d green-l))
         (green-lc (if (eq ,mode 'light) green-l green-d)))

     (when (eq 'light ,mode)
       (rotatef base03 base3)
       (rotatef base02 base2)
       (rotatef base01 base1)
       (rotatef base00 base0))
     ,@body))

(defmacro color-theme-odabai-solarized--face-specs ()
  "Return a backquote which defines a list of face specs."
  (quote
   (mapcar
    (lambda (entry)
      (list (car entry) `((t ,@(cdr entry)))))
    `(
      (default (:foreground ,base0 :background ,base03))
      (bold (:weight bold))
      (bold-italic (:slant italic :weight bold))
      (underline (:underline t))
      (italic (:slant italic))

      ;; basic
      (cursor (:background ,red))
      (escape-glyph-face (:foreground ,red))
      (fringe (:foreground ,base01 :background ,base02))
      (header-line (:foreground ,base0 :background ,base2))
      (highlight (:background ,base02))
      (lazy-highlight (:foreground ,cyan :background ,base03 :inverse-video t)) ;; other search results
      (query-replcae (:foreground ,orange :background ,base03 :inverse-video t))
      (menu (:foreground ,base0 :background ,base02))
      (minibuffer-prompt (:foreground ,blue))
      (mode-line (:foreground ,base1 :background ,base02 :box (:line-width 1 :color ,base1)))
      (mode-line-buffer-id (:foreground ,base1))
      (mode-line-inactive (:foreground ,base0  :background ,base02 :box (:line-width 1 :color ,base02)))

      ;; (region ((t (:background ,base02 :foreground ,base0 :inverse-video nil))

      ;; asasa C-u C-x = (what-cursor-position)
      ;; to find out if overlay is active: (equal (get-char-property (point) 'face) 'show-paren-match)
      ;; FIXME: How to remove inverse-video when smartparens is active.
      (region (:background
                   ,(if nil
                        base02
                      base02) :foreground ,base0 :inverse-video nil))

      (secondary-selection (:background ,base02))
      (trailing-whitespace (:foreground "yellow" :inverse-video t))
      (vertical-border (:foreground ,base0))
      ;; compilation
      (compilation-info (:forground ,green :bold t))
      (compilation-warning (:foreground ,orange :bold t))
      ;; customize
      (custom-button (:background ,base02 :box (:line-width 2 :style released-button)))
      (custom-button-mouse (:inherit custom-button :foreground ,base1))
      (custom-button-pressed (:inherit custom-button-mouse :box (:line-width 2 :style pressed-button)))
      (custom-comment-tag (:background ,base02))
      (custom-comment-tag (:background ,base02))
      (custom-documentation (:inherit default))
      (custom-group-tag (:foreground ,orange :bold t))
      (custom-link (:foreground ,violet))
      (custom-state (:foreground ,green))
      (custom-variable-tag (:foreground ,orange :bold t))

      ;; -----------------------------------------------------------------------------------------------
      ;; font-lock
      ;; -----------------------------------------------------------------------------------------------
      (font-lock-builtin-face (:foreground ,green))
      (font-lock-comment-face (:foreground ,base01 :italic t))
      (font-lock-constant-face (:foreground ,cyan))
      (font-lock-function-name-face (:foreground ,blue))
      (font-lock-keyword-face (:foreground ,green))
      (font-lock-string-face (:foreground ,cyan))
      (font-lock-type-face (:foreground ,yellow))
      (font-lock-variable-name-face (:foreground ,blue))
      (font-lock-warning-face (:foreground ,red :bold t))

      ;; ----------------------------------------------------------------------------------------
      ;; isearch
      ;; ----------------------------------------------------------------------------------------
      (isearch (:foreground ,yellow :background ,base03 :inverse-video t))
      (isearch-lazy-highlight-face (:foreground ,cyan :background ,base03 :inverse-video t)) ;; strangely the lazy-highlight face is applied
      (isearch-fail (:background ,base03 :inherit font-lock-warning-face :inverse-video t))

      ;; -----------------------------------------------------------------------------------------------
      ;; diff
      ;; -----------------------------------------------------------------------------------------------
      (diff-added (:foreground ,green :inverse-video t))
      (diff-changed (:foreground ,yellow :inverse-video t))
      (diff-removed (:foreground ,red :inverse-video t))

      ;; -----------------------------------------------------------------------------------------------
      ;; emacs-wiki
      ;; -----------------------------------------------------------------------------------------------
      (emacs-wiki-bad-link-face (:foreground ,red :underline t))
      (emacs-wiki-link-face (:foreground ,blue :underline t))
      (emacs-wiki-verbatim-face (:foreground ,base00 :underline t))

      ;; -----------------------------------------------------------------------------------------------
      ;; info
      ;; -----------------------------------------------------------------------------------------------
      (info-xref (:foreground ,blue :underline t))
      (info-xref-visited (:inherit info-xref :foreground ,magenta))

      ;; -----------------------------------------------------------------------------------------------
      ;; org
      ;; -----------------------------------------------------------------------------------------------
      (org-agenda-structure (:foreground ,base1 :background ,base02
                                             :weight bold :slant normal :inverse-video nil :height ,solarized-height-plus-1
                                             :underline nil
                                             :box (:line-width 2 :color ,base03)))
      (org-agenda-calendar-event (:foreground ,base1))
      (org-agenda-calendar-sexp (:foreground ,base0 :slant italic))
      (org-agenda-date (:foreground ,base01 :background ,base03 :weight normal :box (:line-width 2 :color ,base03)
                                        :inverse-video nil :overline nil :slant normal :height 1.0))
      (org-agenda-date-weekend (:inherit org-agenda-date :inverse-video nil :background unspecified :foreground ,base01 :weight unspecified
                                             :underline t :overline nil :box unspecified))
      (org-agenda-date-today (:inherit org-agenda-date :inverse-video t :weight bold :underline unspecified :overline nil :box unspecified
                                           :foreground ,blue :background ,base03))
      (org-agenda-done (:foreground ,base01 :slant italic))
      (org-archived (:foreground ,base01 :weight normal))
      (org-block (:foreground ,base01))
      (org-block-begin-line (:foreground ,base01 :slant italic))
      (org-checkbox (:background ,base03 :foreground ,base0 :box (:line-width 1 :style released-button)))
      (org-code (:foreground ,base01))
      (org-date (:foreground ,blue :underline t))
      (org-done (:weight bold :foreground ,green))
      (org-ellipsis (:foreground ,base01))
      (org-formula (:foreground ,yellow))
      (org-headline-done (:foreground ,green))
      (org-hide (:foreground ,base03))
      (org-level-1 (:inherit ,'default :foreground ,orange))
      (org-level-2 (:inherit ,'default :foreground ,green))
      (org-level-3 (:inherit ,'default :foreground ,blue))
      (org-level-4 (:inherit ,'default :foreground ,yellow))
      (org-level-5 (:inherit ,'default :foreground ,cyan))
      (org-level-6 (:inherit ,'default :foreground ,green))
      (org-level-7 (:inherit ,'default :foreground ,red))
      (org-level-8 (:inherit ,'default :foreground ,blue))
      (org-link (:foreground ,yellow :underline t))
      (org-sexp-date (:foreground ,violet))
      (org-scheduled (:foreground ,green))
      (org-scheduled-previously (:foreground ,cyan))
      (org-scheduled-today (:foreground ,blue :weight normal))
      (org-special-keyword (:foreground ,base01 :weight bold))
      (org-table (:foreground ,green))
      (org-tag (:weight bold))
      (org-time-grid (:foreground ,base01))
      (org-todo (:foreground ,cyan :weight bold))
      (org-upcoming-deadline (:foreground ,yellow :weight normal :underline nil))
      (org-warning (:foreground ,orange :weight normal :underline nil))
      ;; org-habit
      ;; (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
      (org-habit-clear-face (:background ,blue-lc :foreground ,blue-hc))
      (org-habit-clear-future-face (:background ,blue-lc))
      (org-habit-ready-face (:background ,green-lc :foreground ,green))
      (org-habit-ready-future-face (:background ,green-lc))
      (org-habit-alert-face (:background ,yellow :foreground ,yellow-lc))
      (org-habit-alert-future-face (:background ,yellow-lc))
      (org-habit-overdue-face (:background ,red :foreground ,red-lc))
      (org-habit-overdue-future-face (:background ,red-lc))
                                        ; latest additions
      (org-agenda-dimmed-todo-face (:foreground ,base01))
      (org-agenda-restriction-lock (:background ,yellow))
      (org-clock-overlay (:background ,yellow))
      (org-column (:background ,base02 :strike-through nil
                                   :underline nil :slant normal :weight normal :inherit default))
      (org-column-title (:background ,base02 :underline t :weight bold))
      (org-date-selected (:foreground ,red :inverse-video t))
      (org-document-info (:foreground ,base0))
      (org-document-title (:foreground ,base1 :weight bold :height ,solarized-height-plus-4))
      (org-drawer (:foreground ,cyan))
      (org-footnote (:foreground ,magenta :underline t))
      (org-latex-and-export-specials (:foreground ,orange))
      (org-mode-line-clock-overrun (:inherit mode-line :background ,red))

      ;; -----------------------------------------------------------------------------------------------
      ;; show-paren
      ;; -----------------------------------------------------------------------------------------------
      (show-paren-match-face (:background nil :foreground nil :inverse-video t))
      (show-paren-mismatch-face (:background ,red :foreground ,base02))

      ;; -----------------------------------------------------------------------------------------------
      ;; helm
      ;; These probably needs tweaking.
      ;; -----------------------------------------------------------------------------------------------
      (helm-apt-deinstalled (:foreground ,base01))
      (helm-apt-installed (:foreground ,green))
      (helm-bookmark-directory (:inherit helm-ff-directory))
      (helm-bookmark-file (:foreground ,base0))
      (helm-bookmark-gnus (:foreground ,cyan))
      (helm-bookmark-info (:foreground ,green))
      (helm-bookmark-man (:foreground ,violet))
      (helm-bookmark-w3m (:foreground ,yellow))
      (helm-bookmarks-su (:foreground ,orange))
      (helm-buffer-not-saved (:foreground ,orange))
      (helm-buffer-saved-out (:foreground ,red :background ,base03 :inverse-video t))
      (helm-buffer-size (:foreground ,base01))
      (helm-candidate-number (:background ,base02 :foreground ,base1 :bold t))
      (helm-ff-directory (:background ,base03 :foreground ,blue))
      (helm-ff-executable (:foreground ,green))
      (helm-ff-file (:background ,base03 :foreground ,base0))
      (helm-ff-invalid-symlink (:background ,base03 :foreground ,orange :slant italic))
      (helm-ff-prefix (:background ,yellow :foreground ,base03))
      (helm-ff-symlink (:foreground ,cyan))
      (helm-grep-file (:foreground ,cyan :underline t))
      (helm-grep-finish (:foreground ,green))
      (helm-grep-lineno (:foreground ,orange))
      (helm-grep-match (:inherit match))
      (helm-grep-running (:foreground ,red))
      (helm-header (:inherit header-line))
      (helm-lisp-completion-info (:foreground ,base0))
      (helm-lisp-show-completion (:foreground ,yellow :background ,base02 :bold t))
      (helm-M-x-key (:foreground ,orange :underline t))
      (helm-moccur-buffer (:foreground ,cyan :underline t))
      ;; (helm-match (:inherit match))
      (helm-selection (:background ,base02 :underline t))
      (helm-selection-line (:background ,base02 :foreground ,base1
                                            :underline nil))
      (helm-separator (:foreground ,red))
      (helm-source-header (:background ,blue-lc :foreground ,base03 :underline nil))
      (helm-time-zone-current (:foreground ,green))
      (helm-time-zone-home (:foreground ,red))
      (helm-visible-mark (:background ,base03 :foreground ,magenta :bold t))

      ;; -----------------------------------------------------------------------------------------------
      ;; ido
      ;; -----------------------------------------------------------------------------------------------
      (ido-first-match (:foreground ,yellow :weight normal))
      (ido-only-match (:foreground ,base03 :background ,yellow :weight normal))
      (ido-subdir (:foreground ,blue))
      (ido-incomplete-regexp (:foreground ,red :weight bold ))
      (ido-indicator (:background ,red :foreground ,base03 :width condensed))
      (ido-virtual (:foreground ,cyan))

      ;; ----------------------------------------------------------------------------------------
      ;; line number color
      ;; ----------------------------------------------------------------------------------------
      (linum (:foreground ,base0))

      ;; ----------------------------------------------------------------------------------------
      ;; magit
      ;; ----------------------------------------------------------------------------------------
      (magit-section-title (:foreground ,yellow :weight bold))
      (magit-branch (:foreground ,orange :weight bold))
      (magit-item-highlight (:background ,base02 :weight unspecified))
      (magit-log-author (:foreground ,cyan))
      (magit-log-graph (:foreground ,base01))
      (magit-log-head-label-bisect-bad (:background ,red-hc :foreground ,red-lc :box 1))
      (magit-log-head-label-bisect-good (:background ,green-hc :foreground ,green-lc :box 1))
      (magit-log-head-label-default (:background ,base02 :box 1))
      (magit-log-head-label-local (:background ,blue-lc :foreground ,blue-hc :box 1))
      (magit-log-head-label-patches (:background ,red-lc :foreground ,red-hc :box 1))
      (magit-log-head-label-remote (:background ,green-lc :foreground ,green-hc :box 1))
      (magit-log-head-label-tags (:background ,yellow-lc :foreground ,yellow-hc :box 1))
      ;; magit hotfix
      (magit-header (:inherit default))
      ;; TODO
      (magit-log-sha1 (:foreground ,yellow))
      (magit-cherry-equivalent (:foreground ,magenta))
      (magit-cherry-unmatched (:foreground ,cyan))
      ;; (magit-log-head-label-bisect-skip (:background "light goldenrod" :foreground "dark goldenrod" :box 1))
      ;; (magit-log-head-label-head (:background "Grey70" :foreground "Black" :box 1))
      ;; (magit-log-head-label-wip (:background "Grey95" :foreground "LightSkyBlue3" :box 1))
      ;; (magit-log-reflog-label-checkout (:background "Grey85" :foreground "LightSkyBlue4" :box 1))
      ;; (magit-log-reflog-label-cherry-pick (:background "light green" :foreground "dark olive green" :box 1))
      ;; (magit-log-reflog-label-commit (:background "LemonChiffon1" :foreground "goldenrod4" :box 1))
      ;; (magit-log-reflog-label-other (:background "Grey50" :box 1))
      ;; (magit-log-reflog-label-rebase (:background "Grey85" :foreground "OliveDrab4" :box 1))
      ;; (magit-log-reflog-label-remote (:background "Grey50" :box 1))
      ;; (magit-log-reflog-label-reset (:background "IndianRed1" :foreground "IndianRed4" :box 1))
      (magit-process-ng (:inherit magit-header :foreground ,red))
      (magit-process-ok (:inherit magit-header :foreground ,green))
      (magit-signature-bad (:foreground ,red))
      (magit-signature-good (:foreground ,green))
      (magit-signature-none (:inherit magit-log-message))
      (magit-signature-untrusted (:foreground ,cyan))
      (magit-whitespace-warning-face (:inherit trailing-whitespace))

      ;; iedit
      (iedit-occurrence (:foreground ,orange :background ,base03 :inverse-video t))

      ;; which-func that shows which function we are in
      (which-func (:foreground ,blue))

      ;; ansi-term
      (term (:foreground nil :background nil :inherit default))
      (term-color-black (:foreground ,base0 :background ,base03))
      (term-color-red (:foreground ,red :background ,red))
      (term-color-green (:foreground ,green :background ,green))
      (term-color-yellow (:foreground ,yellow :background ,yellow))
      (term-color-blue (:foreground ,blue :background ,blue))
      (term-color-magenta (:foreground ,magenta :background ,magenta))
      (term-color-cyan (:foreground ,cyan :background ,cyan))
      (term-color-white (:foreground ,base03 :BACKGROUND ,base03))

      ;; visual regexp
      (vr/match-0 (:inherit isearch))
      ))))

(defmacro color-theme-odabai-solarized--define-theme (mode)
  "Define either the dark or the light theme.
Argument MODE: 'light or 'dark"
  (let ((name (intern (format "odabai-solarized-%s" (symbol-name mode)))) ; TODO do not know what intern stands for
        (doc (format "A version of Ethan Schoonover's 'Solarized' theme (%s version)" mode )))
    ;; we need to use prog to backquote expression as it uses the special marker ','
    `(progn
       (deftheme ,name ,doc)
       ;; create the correct colors
       (color-theme-odabai-solarized--with-colors
        ',mode
        (apply 'custom-theme-set-faces ',name (color-theme-odabai-solarized--face-specs))
        (custom-theme-set-variables
         ',name
         `(ansi-color-names-vector (vector "Black" ,red ,green ,yellow ,blue ,magenta ,cyan ,base03)))
        )
       (provide-theme ',name))))

(defun color-theme-odabai-solarized (mode)
  "Apply either the dark or the light theme."
  (if (fboundp 'load-theme)
    (let ((name (cond
                   ((eq 'light mode) 'odabai-solarized-light)
                   ((eq 'dark mode) 'odabai-solarized-dark)
                   (t (error "invalid mode: %s" mode)))))

      (if (boundp 'custom-enabled-themes)
          (custom-set-variables `(custom-enabled-themes '(,name)))
        (if (> emacs-major-version 23)
            (load-theme name t)
          (load-theme name))))
    (progn
      (error "Old Emacs version. Not yet handled."))))

;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun color-theme-odabai-solarized-dark ()
  "Apply the dark solarized theme."
  (interactive)
  (color-theme-odabai-solarized 'dark))

;;;###autoload
(defun color-theme-odabai-solarized-light ()
  "Apply the light solarized theme."
  (interactive)
  (color-theme-odabai-solarized 'light))

(provide 'color-theme-odabai-solarized)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
