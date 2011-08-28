;;;; Heiner's emacs init file, ~/.emacs.d/init.el
;;;; Written for GNU Emacs 23.1.50.1 (i486-pc-linux-gnu, GTK+ Version 2.18.0)
;;;; of 2009-09-27 on palmer, modified by Debian

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(defun heiner-xemacs-like-cursor ()
  "Have half a cursor at line endings, like xemacs has"
  (if (looking-at "$")
      (setq cursor-type '(bar . 4))
    (setq cursor-type 'box)))

(add-hook 'post-command-hook 'heiner-xemacs-like-cursor)

(setq show-paren-delay 0)
(tool-bar-mode -1)

;; http://www.emacswiki.org/emacs/CopyAndPaste
(setq mouse-drag-copy-region nil)
(setq x-select-enable-primary nil)
(setq x-select-enable-clipboard t)
(setq select-active-regions t)
(global-set-key [mouse-2] 'mouse-yank-primary)

;; From kde-emacs-core.el
;; http://websvn.kde.org/trunk/KDE/kdesdk/scripts/kde-emacs/
;; Originally by Arnt Gulbrandsen ("agulbra")
(defun agulbra-tab (arg)
  "Do the right thing about tabs."
  (interactive "*P")
  (cond
   ((and (not (looking-at "[A-Za-z0-9ÄÖÜäöü]"))
         (save-excursion
	   (backward-char)
           (looking-at "[A-Za-z0-9ÄÖÜäöü:>_\\-\\&\\.(){}\\*\\+/]"))
	 ;; if in ruby mode, don't try to expand "end"
	 (or (not (equal major-mode 'ruby-mode))
	  (not (looking-back "end"))))
    (condition-case nil
	(dabbrev-expand arg)
      (error (indent-for-tab-command))))
   (t
    (indent-for-tab-command))))

(global-set-key [tab] 'agulbra-tab)
;; The above does not work in the minibuffer, hence:
(define-key minibuffer-local-map [tab] 'minibuffer-complete)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'ruby-mode)
(define-key ruby-mode-map "\C-m" 'newline-and-indent)

(menu-bar-mode -1)
(global-set-key [f9] 'menu-bar-mode)    ; toggles menu bar

(delete-selection-mode 1)

(setq-default indent-tabs-mode nil)

(setq column-number-mode t)

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/heiner")
(load "xemacs-colors")

;; from http://wttools.sourceforge.net/emacs-stuff/emacs.html
(require 'pager)
(global-set-key "\C-v"	   'pager-page-down)
(global-set-key [next] 	   'pager-page-down)
(global-set-key "\ev"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)

(setq frame-title-format "emacs: %b")

;; Have 50 rows and 84 columns now (works for my font/monitor)
;; This does not work?!
(setq initial-frame-alist
      `((width . 84) (height . 60)))

(load "latex-devel")
(load "lilypond-devel")

(defun make-in-background ()
  "Executes a \"make\" in the current directory"
  (interactive)
  (compile "make -k")
  (delete-other-windows))

(define-key global-map [(f4)] 'make-in-background)

;(load "nxhtml/autostart.el")
;(require 'mediawiki)

(if (file-exists-p "heiner/passwords.el")
    (load "passwords"))

(define-key global-map [(meta backspace)] 'backward-kill-word)

;; "Wie es sich für einen PC gehört."
(pc-selection-mode)

;;; copy Amit's mode-line
;;; http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html

;; Mode line setup
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; emacsclient [default -- keep?]
   mode-line-client
   "  "
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "      ")))
   "    "
   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ; narrow [default -- keep?]
   " %n "
   ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "    "
   ; nyan-mode uses nyan cat as an alternative to %p
   (:eval (when nyan-mode (list (nyan-create))))
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground "gray60" :background "gray20"
    :inverse-video nil
    :box '(:line-width 6 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray80" :background "gray40"
    :inverse-video nil
    :box '(:line-width 6 :color "gray40" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo" :height 100)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 110)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")
