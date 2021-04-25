;;;; Heiner's emacs init file, ~/.emacs.d/init.el
;;;; Written for
;;;; * GNU Emacs 23.3.1 (i686-pc-linux-gnu, GTK+ Version 2.24.10)
;;;; * GNU Emacs 23.2.1 (x86_64-pc-linux-gnu, GTK+ Version 2.20.1)
;;;; * GNU Emacs 23.1.50.1 (i486-pc-linux-gnu, GTK+ Version 2.18.0)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
;(load "xemacs-colors")

;; The famous KDE Emacs bindings
;(add-to-list 'load-path "~/.emacs.d/site-lisp/kde-emacs")
;(require 'kde-emacs)

;(setq kde-full-name "Heinrich Kuettler")
;(setq kde-email "heinrich.kuettler@gmail.com")

;;(c-subword-mode)


;; (menu-bar-mode -1)  ;; Disable menu bar
(global-set-key [f9] 'menu-bar-mode)    ; toggles menu bar

(delete-selection-mode 1)

(setq-default indent-tabs-mode nil)

(setq column-number-mode t)

;; unbind `C-x C-c'
(global-unset-key [(control x) (control c)])
(global-unset-key [(control z)])

;; Don't minimize on M-m.
(global-unset-key [?\s-m])


;; from http://wttools.sourceforge.net/emacs-stuff/emacs.html
(require 'pager)
(global-set-key "\C-v"	   'pager-page-down)
(global-set-key [next] 	   'pager-page-down)
(global-set-key "\ev"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)

;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)

;(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-bazel-mode")
;(require 'bazel-mode)

(add-hook 'ido-setup-hook
          (lambda ()
            ;; We set tab in minibuffer-local-map; not good for ido, so:
            (define-key ido-completion-map [tab] 'ido-complete)
            (define-key ido-completion-map [backtab] 'ido-prev-match)))

(setq frame-title-format "emacs: %b")

(require 'heiner-devel)

(define-key global-map [(f4)] (compile-function "make -k"))

(load "latex-devel")
(load "lilypond-devel")

;;(load "nxhtml/autostart.el")
;(require 'mediawiki)

(if (file-exists-p "heiner/passwords.el")
    (load "passwords"))

(define-key global-map [(meta backspace)] 'backward-kill-word)

;; From http://stackoverflow.com/questions/3124844/what-are-your-favorite-global-key-bindings-in-emacs
(defmacro global-set-key* (keys &rest body)
  `(global-set-key ,keys (lambda () (interactive) ,@body)))

(defun ensure-mark-active ()
  (unless mark-active
    (push-mark (point) nil t)))

;; (pending-delete-mode) (delete-selection-mode t) (pc-selection-mode 0)
;; Pseudo pc selection, but without mark deactivation
(global-set-key* [(shift up)] (ensure-mark-active) (previous-line))
(global-set-key* [(shift down)] (ensure-mark-active) (next-line))
(global-set-key* [(shift right)] (ensure-mark-active) (forward-char))
(global-set-key* [(shift left)] (ensure-mark-active) (backward-char))
(global-set-key* [(shift next)] (ensure-mark-active) (pager-page-down))
(global-set-key* [(shift prior)] (ensure-mark-active) (pager-page-up))
(global-set-key* [(control shift right)] (ensure-mark-active) (forward-word))
(global-set-key* [(control shift left)] (ensure-mark-active) (backward-word))
(global-set-key* [(shift home)]
                 (ensure-mark-active) (move-beginning-of-line nil))
(global-set-key* [(shift end)]
                 (ensure-mark-active) (move-end-of-line nil))
(global-set-key* [(control shift home)]
                 (ensure-mark-active) (beginning-of-buffer))
(global-set-key* [(control shift end)]
                 (ensure-mark-active) (end-of-buffer))

;;(setq visible-bell 0)
(setq ring-bell-function 'ignore) ; no alarm at all

(prefer-coding-system 'utf-8)

;;(global-subword-mode t)

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

(global-set-key (kbd "<left-fringe> <mouse-1>")
                (lambda (event)
                  (interactive "e")
                  (save-excursion
                    (mouse-set-point event)
                    (bm-toggle))))

(global-set-key [(control return)] 'bm-toggle)
(global-set-key [(s return)] 'bm-toggle)
(global-set-key [(s down)] 'bm-forward)
(global-set-key [(s up)] 'bm-backward)

(global-set-key* (kbd "M-3") (insert-and-inherit ?#))

(global-set-key [(f3)] 'isearch-repeat-forward)

(when (eq system-type 'darwin)
  ;; This is beginning/end-of-buffer by default.
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>") 'end-of-line))


(setq tramp-default-method "ssh")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
