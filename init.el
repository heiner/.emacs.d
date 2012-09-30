;;;; Heiner's emacs init file, ~/.emacs.d/init.el
;;;; Written for
;;;; * GNU Emacs 23.3.1 (i686-pc-linux-gnu, GTK+ Version 2.24.10)
;;;; * GNU Emacs 23.2.1 (x86_64-pc-linux-gnu, GTK+ Version 2.20.1)
;;;; * GNU Emacs 23.1.50.1 (i486-pc-linux-gnu, GTK+ Version 2.18.0)

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'ruby-mode)
(define-key ruby-mode-map "\C-m" 'newline-and-indent)

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/heiner")
(load "xemacs-colors")

;; The famous KDE Emacs bindings
(add-to-list 'load-path "~/.emacs.d/site-lisp/kde-emacs")
(require 'kde-emacs)

(setq kde-full-name "Heinrich Kuettler")
(setq kde-email "heinrich.kuettler@gmx.de")

;;(c-subword-mode)

;; Disable menu bar
(menu-bar-mode -1)
(global-set-key [f9] 'menu-bar-mode)    ; toggles menu bar

(delete-selection-mode 1)

(setq-default indent-tabs-mode nil)

(setq column-number-mode t)

;; from http://wttools.sourceforge.net/emacs-stuff/emacs.html
(require 'pager)
(global-set-key "\C-v"	   'pager-page-down)
(global-set-key [next] 	   'pager-page-down)
(global-set-key "\ev"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)

(pc-selection-mode)

;; Visible bookmarks. See emacswiki.org/emacs/VisibleBookmarks
(require 'bm)

(defun bm-forward nil
 "Goto next bookmark."
 (interactive)
 (if (= (bm-count) 0)
     (if bm-cycle-all-buffers
         (bm-first-in-next-buffer)
       (forward-paragraph))
   (bm-next)))

(defun bm-backward nil
 "Goto previous bookmark."
 (interactive)
 (if (= (bm-count) 0)
     (if bm-cycle-all-buffers
         (bm-last-in-previous-buffer)
       (backward-paragraph))
   (bm-previous)))

(global-set-key (kbd "<left-fringe> <mouse-1>") #'(lambda(event)
                                                    (interactive "e")
                                                    (save-excursion
                                                      (mouse-set-point event)
                                                      (bm-toggle))))

;;(global-set-key [(f3)] 'bm-toggle)
(global-set-key [(control return)] 'bm-toggle)
(global-set-key [(control down)] 'bm-forward)  ; was forward-paragraph
(global-set-key [(control up)] 'bm-backward)   ; was backward-paragraph

;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
;; Above, we set tab in minibuffer-local-map; not good for ido, so:
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)))

(setq frame-title-format "emacs: %b")

(require 'heiner-devel)

(define-key global-map [(f4)] (compile-function "make -k"))

(load "latex-devel")
(load "lilypond-devel")

;;(load "nxhtml/autostart.el")
;;(require 'mediawiki)

(if (file-exists-p "heiner/passwords.el")
    (load "passwords"))

(define-key global-map [(meta backspace)] 'backward-kill-word)

;; "Wie es sich für einen PC gehört."
;(pending-delete-mode)
(pc-selection-mode)

;;(setq visible-bell 0)
(setq ring-bell-function 'ignore) ; no alarm at all

;; (setq ring-bell-function
;;       (lambda ()
;; 	(unless (memq this-command
;; 		      '(isearch-abort
;;                         abort-recursive-edit
;;                         exit-minibuffer
;;                         keyboard-quit))
;; 	  (message-box (symbol-name this-command)))))

(prefer-coding-system 'utf-8)

;;(global-subword-mode t)
