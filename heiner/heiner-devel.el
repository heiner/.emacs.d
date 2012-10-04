
(provide 'heiner-devel)

(defun heiner-enclose-by (open close)
  "Insert open und close around the highlighted region"
  (if (region-active-p)
      (let ((content (buffer-substring-no-properties
                      (region-beginning) (region-end)))
            (origin (point)))
        (delete-region (region-beginning) (region-end))
        (insert (concat open content close))
        (goto-char (+ origin (length open))))
    (insert (concat open close))
    (backward-char (length close))))

(add-hook 'c-mode-common-hook
          (lambda () (c-subword-mode 1)))

(defun compile-function (command)
  `(lambda () (interactive)
     (compile ,command)))

(add-hook 'java-mode-hook
          (lambda ()
            (define-key java-mode-map [(f4)]
              (compile-function "ant debug -find"))
            (define-key java-mode-map [(f5)]
              (compile-function "ant debug install -find"))))

(add-hook 'ruby-mode-hook
          (lambda () (abbrev-mode 1)))

;; Adapted from emacswiki.org/emacs/CompileCommand
(defun delete-compilation-window-if-successful (buffer msg)
  "Delete the compilation window if the compilation was successful and
the compilation window did not have a frame of its own."
  (if (and (string-match "^finished" msg)
           (equal "*compilation*" (buffer-name buffer))
           (not (one-window-p)))
      (delete-windows-on buffer)))

(add-to-list 'compilation-finish-functions
	     'delete-compilation-window-if-successful)

;; Intended to be set as `special-display-function'.
(defun prefer-other-frame (buffer &optional buffer-data)
   (let* ((next (next-window (selected-window) 'never-minibuf t))
         (window
          (cond
           ((get-buffer-window buffer 0))
           ((not (eq next (selected-window)))
            next)
           (t
            (split-window)))))
     (raise-frame (window-frame window))
     (set-window-buffer window buffer)
     window))

;; http://emacswiki.org/emacs/ParEdit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))

(add-to-list 'load-path "~/.emacs.d/site-lisp/clojure-mode")
(require 'clojure-mode)

(setq inferior-lisp-program "lein repl")

(eval-after-load 'paredit
  '(progn (define-key paredit-mode-map (kbd ")")
            'paredit-close-parenthesis)
          (define-key paredit-mode-map (kbd "M-)")
            'paredit-close-parenthesis-and-newline)))

(defun funcall-did-not-change-value (f g)
  "Call g, then f, then g again. Return true if the return values
of the two calls of g are equal, nil otherwise"
  (let ((y (funcall g)))
    (funcall f)
    (equal y (funcall g))))

(defun indent-or-fixup-whitespace nil
  (cond
   ((or (looking-at "[^[:space:]]")
        (string-match "^[[:space:]]*$"
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (point))))
    (indent-for-tab-command))
   ((funcall-did-not-change-value
     'indent-for-tab-command
     (lambda ()
       (list (point)
             (buffer-substring-no-properties
              (line-beginning-position)
              (line-end-position)))))
    (fixup-whitespace)
    (if (looking-at "[[:space:]]")
        (forward-char)))))

;; Taken from kde-emacs-core.el and adapted
;; http://websvn.kde.org/trunk/KDE/kdesdk/scripts/kde-emacs/
;; Originally by Arnt Gulbrandsen ("agulbra")
(defun new-agulbra-tab (arg)
  "Do the right thing about tabs."
  (interactive "*P")
  (cond
   ((and (not (looking-at "[[:alnum:]]"))
         (save-excursion
	   (backward-char)
           (looking-at "[[:alnum:]:>_\\-\\&\\.{}\\*\\+/]"))
	 ;; if in ruby mode, don't try to expand "end"
	 (or (not (equal major-mode 'ruby-mode))
	  (not (looking-back "end"))))
    (condition-case nil
	(dabbrev-expand arg)
      (error "Error in dabbrev-expand" (indent-or-fixup-whitespace))))
   (t
    (indent-or-fixup-whitespace))))

(defun uli-agulbra-tab (arg)
  "Do the right thing about tabs."
  (interactive "*P")
  (cond
   ((and (not (looking-at "[[:alnum:]]"))
         (save-excursion
	   (backward-char)
           (looking-at "[[:alnum:]:>_\\-\\&\\.{}\\*\\+/]"))
	 ;; if in ruby mode, don't try to expand "end"
	 (or (not (equal major-mode 'ruby-mode))
	  (not (looking-back "end"))))
    (dabbrev-expand arg))
   ((and (looking-at "[[:space:]]")
         (not (string-match "^[[:space:]]*$"
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (point)))))
    (fixup-whitespace)
    (if (looking-at "[[:space:]]")
        (forward-char)))
   (t
    (indent-for-tab-command))))

(global-set-key [tab] 'uli-agulbra-tab)
;;(global-set-key [tab] 'new-agulbra-tab)
;; The above does not work in the minibuffer, hence:
(define-key minibuffer-local-map [tab] 'minibuffer-complete)
                                        ; But be careful about ido, see init.el!
