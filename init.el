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
