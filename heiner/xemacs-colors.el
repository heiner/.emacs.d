(require 'color-theme)

(defun color-theme-heiner-xemacs-like ()
  "colors like I used to have in xemacs."
  (interactive)
  (color-theme-install
    '(color-theme-heiner-xemacs-like
      ((foreground-color . "black")
       (background-color . "ivory2")
       (background-mode . light))
      (bold ((t (:bold t))))
      (bold-italic ((t (:italic t :bold t))))
      (default ((t (nil))))

      (hl-line ((t (:background "#FF0000"))))
      (mode-line ((t (:foreground "#000000" :background "#E0DFD8"))))
      (mode-line-buffer-id ((t (:background "#E0DFD8" :foreground "firebrick4"))))
      (region ((t (:foreground nil :background "gray80"))))
      (show-paren-match-face ((t (:bold t :background "darkseagreen2"))))
      (show-paren-mismatch-face ((t (:foreground "#2e3436"
				      :background "#ef2929"))))
      (minibuffer-prompt ((t (:foreground "#000000"))))
      (fringe ((t (:background "ivory2"))))
      ;;(border ((t (:background "#eeeeee"))))

      ;;(font-lock-function-name-face ((t (:foreground "#0000FB"))))
      (font-lock-constant-face ((t (:foreground "#4e9a06"))))
      (font-lock-type-face ((t (:foreground "#064e9A"))))
      (font-lock-string-face ((t (:foreground "green4"))))
      (font-lock-keyword-face ((t (:foreground "red4"))))
      (font-lock-comment-face ((t (:foreground "OrangeRed3"))))
      (font-lock-variable-name-face ((t (:foreground "gray20"))))

      ;;   ;;; Standard font lock faces
      ;; (default ((t (nil))))
      ;; (font-lock-comment-delimiter-face ((t (:foreground "#61635e")))) ; dark aluminum
      ;; (font-lock-doc-face ((t (:foreground "#77507b")))) ; plum
      ;; (font-lock-doc-string-face ((t (:foreground "#77507b")))) ; plum
      ;; (font-lock-builtin-face ((t (:foreground "#855c1b")))) ; med-dark chocolate
      ;; (font-lock-preprocessor-face ((t (:foreground "#888a85")))) ; aluminum
      ;; (font-lock-warning-face ((t (:bold t :foreground "#cc0000")))) ; scarlet red

      ;; ;; Search
      ;; (isearch ((t (:foreground "#080808" :background "#edd400"))))
      ;; (isearch-lazy-highlight-face ((t (:foreground "#080808" :background "#2e3436"))))

     ;; ;; Emacs Interface
     ;; (mode-line-inactive ((t (:background "#1f1f1f" :foreground "#888a85"))))
     ;; (minibuffer-prompt ((t (:foreground "#729fcf")))) ; light sky blue

     ;; ;; Line highlighting
     ;; (highlight ((t (:background "#1f1f1f" :foreground nil))))
     ;; (highlight-current-line-face ((t (:background "#1f1f1f" :foreground nil))))

     ;; ;; Calendar
     ;; (holiday-face ((t (:foreground "#cc0000")))) ; dark scarlet red

     ;; ;; Info
     ;; (info-xref ((t (:foreground "#729fcf")))) ; light sky blue
     ;; (info-xref-visited ((t (:foreground "#ad7fa8")))) ; light plum

     ;; ;;; AUCTeX
     ;; (font-latex-sectioning-5-face ((t (:foreground "#c4a000" :bold t)))) ; dark butter
     ;; (font-latex-bold-face ((t (:foreground "#4e9a06" :bold t)))) ; dark chameleon
     ;; (font-latex-italic-face ((t (:foreground "#4e9a06" :italic t)))) ; dark chameleon
     ;; (font-latex-math-face ((t (:foreground "#855c1b")))) ; med-dark chocolate
     ;; (font-latex-string-face ((t (:foreground "#77507b")))) ; plum
     ;; (font-latex-warning-face ((t (:foreground "#cc0000")))) ; dark scarlet red
     ;; (font-latex-slide-title-face ((t (:foreground "#c4a000")))) ; dark butter

     ;; ;;; post-mode
     ;; (post-emoticon-face ((t (:background "#edd400" :foreground "#000000")))) ; medium butter
     ;; (post-header-value-face ((t (:foreground "#4e9a06")))) ; dark chameleon
     ;; (post-header-keyword-face ((t (:foreground "#4e9a06" :bold t)))) ; dark chameleon
     ;; (post-signature-text-face ((t (:foreground "#cc0000")))) ; dark scarlet red
     ;; (post-quoted-text-face ((t (:foreground "#855c1b" :slant normal)))) ; med-dark chocolate
     ;; (post-double-quoted-text-face ((t (:foreground "#77507b" :slant normal)))) ; plum
     ;; (post-multiply-quoted-text-face ((t (:foreground "#61635e" :slant normal)))) ; dark aluminum
     ;; (post-email-address-text-face ((t (:foreground "#729fcf" :bold t)))) ; light sky blue
     ;; (post-url-face ((t (:foreground "#729fcf" :bold t)))) ; light sky blue
       )))

(color-theme-heiner-xemacs-like)
