;;; Author: Ethan Schoonover, Solarized; Greg Pfeil, Emacs adaptation
;;; URL: http://ethanschoonover.com/solarized
;;; HAMID: I also used https://github.com/bbatsov/solarized-emacs/blob/master/solarized.el
;;; This file is not (YET) part of GNU Emacs.

;;; # Usage

;;; 1. Install the color-theme package
;;;   (http://www.emacswiki.org/cgi-bin/wiki/ColorTheme)
;;; 2. Load this file
;;; 3. M-x color-theme-solarized-[dark|light]

(eval-when-compile
  (require 'color-theme))

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


(defun color-theme-solarized (mode)
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized."
  (interactive "Slight or dark? ")
  (let* ((base03  "#002b36")
        (base02  "#073642")
        (base01  "#586e75")
        (base00  "#657b83")
        (base0   "#839496")
        (base1   "#93a1a1")
        (base2   "#eee8d5")
        (base3   "#fdf6e3")
        (yellow  "#b58900")
        (orange  "#cb4b16")
        (red     "#dc322f")
        (magenta "#d33682")
        (violet  "#6c71c4")
        (blue    "#268bd2")
        (cyan    "#2aa198")
        (green   "#859900")

        (yellow "#b58900")
        (orange "#cb4b16")
        (red "#dc322f")
        (magenta "#d33682")
        (violet "#6c71c4")
        (blue "#268bd2")
        (cyan "#2aa198")
        (green "#859900")
        
        (yellow "#b58900")
        (orange "#cb4b16")
        (red "#dc322f")
        (magenta "#d33682")
        (violet "#6c71c4")
        (blue "#268bd2")
        (cyan "#2aa198")
        (green "#859900")

        ;; Darker and lighter accented colors
        ;; Only use these in exceptional circumstances!
        (yellow-d "#7B6000")
        (yellow-l "#DEB542")
        (orange-d "#8B2C02")
        (orange-l "#F2804F")
        (red-d "#990A1B")
        (red-l "#FF6E64")
        (magenta-d "#93115C")
        (magenta-l "#F771AC")
        (violet-d "#3F4D91")
        (violet-l "#9EA0E5")
        (blue-d "#00629D")
        (blue-l "#69B7F0")
        (cyan-d "#00736F")
        (cyan-l "#69CABF")
        (green-d "#546E00")
        (green-l "#B4C342")

        (yellow-hc (if (eq mode 'light) yellow-d yellow-l))
        (yellow-lc (if (eq mode 'light) yellow-l yellow-d))
        (orange-hc (if (eq mode 'light) orange-d orange-l))
        (orange-lc (if (eq mode 'light) orange-l orange-d))
        (red-hc (if (eq mode 'light) red-d red-l))
        (red-lc (if (eq mode 'light) red-l red-d))
        (magenta-hc (if (eq mode 'light) magenta-d magenta-l))
        (magenta-lc (if (eq mode 'light) magenta-l magenta-d))
        (violet-hc (if (eq mode 'light) violet-d violet-l))
        (violet-lc (if (eq mode 'light) violet-l violet-d))
        (blue-hc (if (eq mode 'light) blue-d blue-l))
        (blue-lc (if (eq mode 'light) blue-l blue-d))
        (cyan-hc (if (eq mode 'light) cyan-d cyan-l))
        (cyan-lc (if (eq mode 'light) cyan-l cyan-d))
        (green-hc (if (eq mode 'light) green-d green-l))
        (green-lc (if (eq mode 'light) green-l green-d)))

    (when (eq 'light mode)
      (rotatef base03 base3)
      (rotatef base02 base2)
      (rotatef base01 base1)
      (rotatef base00 base0))
    
    (color-theme-install
     `(color-theme-solarized
       ((foreground-color . ,base0)
        (background-color . ,base03)
        (background-mode . ,mode)
        (cursor-color . ,base0))
       ;; basic
       (default ((t (:foreground ,base0))))
       (cursor ((t (:foreground ,base0 :background ,base03 :inverse-video t))))
       (escape-glyph-face ((t (:foreground ,red))))
       (fringe ((t (:foreground ,base01 :background ,base02))))
       (header-line ((t (:foreground ,base0 :background ,base2))))
       (highlight ((t (:background ,base02))))
       (isearch ((t (:foreground ,orange :background ,base03 :inverse-video t))))
       (lazy-highlight ((t (:foreground ,yellow :background ,base03 :inverse-video t))))
       (query-replcae ((t (:foreground ,orange :background ,base03 :inverse-video t))))
       (menu ((t (:foreground ,base0 :background ,base02))))
       (minibuffer-prompt ((t (:foreground ,blue))))
       (mode-line
        ((t (:foreground ,base1 :background ,base02
                         :box (:line-width 1 :color ,base1)))))
       (mode-line-buffer-id ((t (:foreground ,base1))))
       (mode-line-inactive
        ((t (:foreground ,base0  :background ,base02
                         :box (:line-width 1 :color ,base02)))))
       (region ((t (:background ,base02))))
       (secondary-selection ((t (:background ,base02))))
       (trailing-whitespace ((t (:foreground ,red :inverse-video t))))
       (vertical-border ((t (:foreground ,base0))))
       ;; compilation
       (compilation-info ((t (:forground ,green :bold t))))
       (compilation-warning ((t (:foreground ,orange :bold t))))
       ;; customize
       (custom-button
        ((t (:background ,base02 :box (:line-width 2 :style released-button)))))
       (custom-button-mouse ((t (:inherit custom-button :foreground ,base1))))
       (custom-button-pressed
        ((t (:inherit custom-button-mouse
                      :box (:line-width 2 :style pressed-button)))))
       (custom-comment-tag ((t (:background ,base02))))
       (custom-comment-tag ((t (:background ,base02))))
       (custom-documentation ((t (:inherit default))))
       (custom-group-tag ((t (:foreground ,orange :bold t))))
       (custom-link ((t (:foreground ,violet))))
       (custom-state ((t (:foreground ,green))))
       (custom-variable-tag ((t (:foreground ,orange :bold t))))

       ;; -----------------------------------------------------------------------------------------------
       ;; diff
       ;; -----------------------------------------------------------------------------------------------
       (diff-added ((t (:foreground ,green :inverse-video t))))
       (diff-changed ((t (:foreground ,yellow :inverse-video t))))
       (diff-removed ((t (:foreground ,red :inverse-video t))))

       ;; -----------------------------------------------------------------------------------------------
       ;; emacs-wiki
       ;; -----------------------------------------------------------------------------------------------
       (emacs-wiki-bad-link-face ((t (:foreground ,red :underline t))))
       (emacs-wiki-link-face ((t (:foreground ,blue :underline t))))
       (emacs-wiki-verbatim-face ((t (:foreground ,base00 :underline t))))

       ;; -----------------------------------------------------------------------------------------------
       ;; font-lock
       ;; -----------------------------------------------------------------------------------------------
       (font-lock-builtin-face ((t (:foreground ,green))))
       (font-lock-comment-face ((t (:foreground ,base01 :italic t))))
       (font-lock-constant-face ((t (:foreground ,cyan))))
       (font-lock-function-name-face ((t (:foreground ,blue))))
       (font-lock-keyword-face ((t (:foreground ,green))))
       (font-lock-string-face ((t (:foreground ,cyan))))
       (font-lock-type-face ((t (:foreground ,yellow))))
       (font-lock-variable-name-face ((t (:foreground ,blue))))
       (font-lock-warning-face ((t (:foreground ,red :bold t))))

       ;; -----------------------------------------------------------------------------------------------
       ;; info
       ;; -----------------------------------------------------------------------------------------------
       (info-xref ((t (:foreground ,blue :underline t))))
       (info-xref-visited ((t (:inherit info-xref :foreground ,magenta))))

       ;; -----------------------------------------------------------------------------------------------
       ;; org
       ;; -----------------------------------------------------------------------------------------------
       (org-agenda-structure ((t (:foreground ,base1 :background ,base02
                                              :weight bold :slant normal :inverse-video nil :height ,solarized-height-plus-1
                                              :underline nil
                                              :box (:line-width 2 :color ,base03)))))
       (org-agenda-calendar-event ((t (:foreground ,base1))))
       (org-agenda-calendar-sexp ((t (:foreground ,base0 :slant italic))))
       (org-agenda-date ((t (:foreground ,base01 :background ,base03 :weight normal :box (:line-width 2 :color ,base03)
                                         :inverse-video nil :overline nil :slant normal :height 1.0))))
       (org-agenda-date-weekend ((t (:inherit org-agenda-date :inverse-video nil :background unspecified :foreground ,base01 :weight unspecified
                                              :underline t :overline nil :box unspecified))))
       (org-agenda-date-today ((t (:inherit org-agenda-date :inverse-video t :weight bold :underline unspecified :overline nil :box unspecified
                                            :foreground ,blue :background ,base03))))
       (org-agenda-done ((t (:foreground ,base01 :slant italic))))
       (org-archived ((t (:foreground ,base01 :weight normal))))
       (org-block ((t (:foreground ,base01))))
       (org-block-begin-line ((t (:foreground ,base01 :slant italic))))
       (org-checkbox ((t (:background ,base03 :foreground ,base0 :box (:line-width 1 :style released-button)))))
       (org-code ((t (:foreground ,base01))))
       (org-date ((t (:foreground ,blue :underline t))))
       (org-done ((t (:weight bold :foreground ,green))))
       (org-ellipsis ((t (:foreground ,base01))))
       (org-formula ((t (:foreground ,yellow))))
       (org-headline-done ((t (:foreground ,green))))
       (org-hide ((t (:foreground ,base03))))
       (org-level-1 ((t (:inherit ,'default :foreground ,orange))))
       (org-level-2 ((t (:inherit ,'default :foreground ,green))))
       (org-level-3 ((t (:inherit ,'default :foreground ,blue))))
       (org-level-4 ((t (:inherit ,'default :foreground ,yellow))))
       (org-level-5 ((t (:inherit ,'default :foreground ,cyan))))
       (org-level-6 ((t (:inherit ,'default :foreground ,green))))
       (org-level-7 ((t (:inherit ,'default :foreground ,red))))
       (org-level-8 ((t (:inherit ,'default :foreground ,blue))))
       (org-link ((t (:foreground ,yellow :underline t))))
       (org-sexp-date ((t (:foreground ,violet))))
       (org-scheduled ((t (:foreground ,green))))
       (org-scheduled-previously ((t (:foreground ,cyan))))
       (org-scheduled-today ((t (:foreground ,blue :weight normal))))
       (org-special-keyword ((t (:foreground ,base01 :weight bold))))
       (org-table ((t (:foreground ,green))))
       (org-tag ((t (:weight bold))))
       (org-time-grid ((t (:foreground ,base01))))
       (org-todo ((t (:foreground ,cyan :weight bold))))
       (org-upcoming-deadline ((t (:foreground ,yellow :weight normal :underline nil))))
       (org-warning ((t (:foreground ,orange :weight normal :underline nil))))
       ;; org-habit
       ;; (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
       (org-habit-clear-face ((t (:background ,blue-lc :foreground ,blue-hc))))
       (org-habit-clear-future-face ((t (:background ,blue-lc))))
       (org-habit-ready-face ((t (:background ,green-lc :foreground ,green))))
       (org-habit-ready-future-face ((t (:background ,green-lc))))
       (org-habit-alert-face ((t (:background ,yellow :foreground ,yellow-lc))))
       (org-habit-alert-future-face ((t (:background ,yellow-lc))))
       (org-habit-overdue-face ((t (:background ,red :foreground ,red-lc))))
       (org-habit-overdue-future-face ((t (:background ,red-lc))))
                                        ; latest additions
       (org-agenda-dimmed-todo-face ((t (:foreground ,base01))))
       (org-agenda-restriction-lock ((t (:background ,yellow))))
       (org-clock-overlay ((t (:background ,yellow))))
       (org-column ((t (:background ,base02 :strike-through nil
                                    :underline nil :slant normal :weight normal :inherit default))))
       (org-column-title ((t (:background ,base02 :underline t :weight bold))))
       (org-date-selected ((t (:foreground ,red :inverse-video t))))
       (org-document-info ((t (:foreground ,base0))))
       (org-document-title ((t (:foreground ,base1 :weight bold :height ,solarized-height-plus-4))))
       (org-drawer ((t (:foreground ,cyan))))
       (org-footnote ((t (:foreground ,magenta :underline t))))
       (org-latex-and-export-specials ((t (:foreground ,orange))))
       (org-mode-line-clock-overrun ((t (:inherit mode-line :background ,red))))

       ;; -----------------------------------------------------------------------------------------------
       ;; show-paren
       ;; -----------------------------------------------------------------------------------------------
       (show-paren-match-face ((t (:background ,base03 :foreground ,magenta))))
       (show-paren-mismatch-face ((t (:background ,red :foreground ,base02))))

       ;; -----------------------------------------------------------------------------------------------
       ;; helm
       ;; These probably needs tweaking.
       ;; -----------------------------------------------------------------------------------------------
       (helm-apt-deinstalled ((t (:foreground ,base01))))
       (helm-apt-installed ((t (:foreground ,green))))
       (helm-bookmark-directory ((t (:inherit helm-ff-directory))))
       (helm-bookmark-file ((t (:foreground ,base0))))
       (helm-bookmark-gnus ((t (:foreground ,cyan))))
       (helm-bookmark-info ((t (:foreground ,green))))
       (helm-bookmark-man ((t (:foreground ,violet))))
       (helm-bookmark-w3m ((t (:foreground ,yellow))))
       (helm-bookmarks-su ((t (:foreground ,orange))))
       (helm-buffer-not-saved ((t (:foreground ,orange))))
       (helm-buffer-saved-out ((t (:foreground ,red :background ,base03 :inverse-video t))))
       (helm-buffer-size ((t (:foreground ,base01))))
       (helm-candidate-number ((t (:background ,base02 :foreground ,base1 :bold t))))
       (helm-ff-directory ((t (:background ,base03 :foreground ,blue))))
       (helm-ff-executable ((t (:foreground ,green))))
       (helm-ff-file ((t (:background ,base03 :foreground ,base0))))
       (helm-ff-invalid-symlink ((t (:background ,base03 :foreground ,orange :slant italic))))
       (helm-ff-prefix ((t (:background ,yellow :foreground ,base03))))
       (helm-ff-symlink ((t (:foreground ,cyan))))
       (helm-grep-file ((t (:foreground ,cyan :underline t))))
       (helm-grep-finish ((t (:foreground ,green))))
       (helm-grep-lineno ((t (:foreground ,orange))))
       (helm-grep-match ((t (:inherit match))))
       (helm-grep-running ((t (:foreground ,red))))
       (helm-header ((t (:inherit header-line))))
       (helm-lisp-completion-info ((t (:foreground ,base0))))
       (helm-lisp-show-completion ((t (:foreground ,yellow :background ,base02 :bold t))))
       (helm-M-x-key ((t (:foreground ,orange :underline t))))
       (helm-moccur-buffer ((t (:foreground ,cyan :underline t))))
       ;; (helm-match ((t (:inherit match))))
       (helm-selection ((t (:background ,base02 :underline t))))
       (helm-selection-line ((t (:background ,base02 :foreground ,base1
                                                  :underline nil))))
       (helm-separator ((t (:foreground ,red))))
       (helm-source-header ((t (:background ,blue-lc :foreground ,base03 :underline nil))))
       (helm-time-zone-current ((t (:foreground ,green))))
       (helm-time-zone-home ((t (:foreground ,red))))
       (helm-visible-mark ((t (:background ,base03 :foreground ,magenta :bold t))))

       ;; -----------------------------------------------------------------------------------------------
       ;; ido
       ;; -----------------------------------------------------------------------------------------------
       (ido-first-match ((t (:foreground ,yellow :weight normal))))
       (ido-only-match ((t (:foreground ,base03 :background ,yellow :weight normal))))
       (ido-subdir ((t (:foreground ,blue))))
       (ido-incomplete-regexp ((t (:foreground ,red :weight bold ))))
       (ido-indicator ((t (:background ,red :foreground ,base03 :width condensed))))
       (ido-virtual ((t (:foreground ,cyan))))

       ;; ----------------------------------------------------------------------------------------
       ;; line number color
       ;; ----------------------------------------------------------------------------------------
       (linum ((t (:foreground ,base0))))

       ;; ----------------------------------------------------------------------------------------
       ;; magit
       ;; ----------------------------------------------------------------------------------------
       (magit-section-title ((t (:foreground ,yellow :weight bold))))
       (magit-branch ((t (:foreground ,orange :weight bold))))
       (magit-item-highlight ((t (:background ,base02 :weight unspecified))))
       (magit-log-author ((t (:foreground ,cyan))))
       (magit-log-graph ((t (:foreground ,base01))))
       (magit-log-head-label-bisect-bad ((t (:background ,red-hc :foreground ,red-lc :box 1))))
       (magit-log-head-label-bisect-good ((t (:background ,green-hc :foreground ,green-lc :box 1))))
       (magit-log-head-label-default ((t (:background ,base02 :box 1))))
       (magit-log-head-label-local ((t (:background ,blue-lc :foreground ,blue-hc :box 1))))
       (magit-log-head-label-patches ((t (:background ,red-lc :foreground ,red-hc :box 1))))
       (magit-log-head-label-remote ((t (:background ,green-lc :foreground ,green-hc :box 1))))
       (magit-log-head-label-tags ((t (:background ,yellow-lc :foreground ,yellow-hc :box 1))))
       ;; magit hotfix
       (magit-header ((t (:inherit default))))
       ;; TODO
       (magit-log-sha1 ((t (:foreground ,yellow))))
       (magit-cherry-equivalent ((t (:foreground ,magenta))))
       (magit-cherry-unmatched ((t (:foreground ,cyan))))
       ;; (magit-log-head-label-bisect-skip ((t (:background "light goldenrod" :foreground "dark goldenrod" :box 1))))
       ;; (magit-log-head-label-head ((t (:background "Grey70" :foreground "Black" :box 1))))
       ;; (magit-log-head-label-wip ((t (:background "Grey95" :foreground "LightSkyBlue3" :box 1))))
       ;; (magit-log-reflog-label-checkout ((t (:background "Grey85" :foreground "LightSkyBlue4" :box 1))))
       ;; (magit-log-reflog-label-cherry-pick ((t (:background "light green" :foreground "dark olive green" :box 1))))
       ;; (magit-log-reflog-label-commit ((t (:background "LemonChiffon1" :foreground "goldenrod4" :box 1))))
       ;; (magit-log-reflog-label-other ((t (:background "Grey50" :box 1))))
       ;; (magit-log-reflog-label-rebase ((t (:background "Grey85" :foreground "OliveDrab4" :box 1))))
       ;; (magit-log-reflog-label-remote ((t (:background "Grey50" :box 1))))
       ;; (magit-log-reflog-label-reset ((t (:background "IndianRed1" :foreground "IndianRed4" :box 1))))
       (magit-process-ng ((t (:inherit magit-header :foreground ,red))))
       (magit-process-ok ((t (:inherit magit-header :foreground ,green))))
       (magit-signature-bad ((t (:foreground ,red))))
       (magit-signature-good ((t (:foreground ,green))))
       (magit-signature-none ((t (:inherit magit-log-message))))
       (magit-signature-untrusted ((t (:foreground ,cyan))))
       (magit-whitespace-warning-face ((t (:inherit trailing-whitespace))))

       ;; iedit
       (iedit-occurrence ((t (:foreground ,orange :background ,base03 :inverse-video t))))

       ;; which-func that shows which function we are in
       (which-func ((t (:foreground ,blue))))
       
))))



(defun color-theme-solarized-dark ()
  (interactive)
  (color-theme-solarized 'dark))

(defun color-theme-solarized-light ()
  (interactive)
  (color-theme-solarized 'light))

(add-to-list 'color-themes
             '(color-theme-solarized-light
               "Solarized Light"
               "Ethan Schoonover & Greg Pfeil <greg@technomadic.org>"))
(add-to-list 'color-themes
             '(color-theme-solarized-dark
               "Solarized Dark"
               "Ethan Schoonover & Greg Pfeil <greg@technomadic.org>"))

(provide 'odabai-color-theme-solarized)
