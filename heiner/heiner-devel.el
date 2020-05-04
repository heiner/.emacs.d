
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

(defmacro compile-function (command)
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

;;; http://emacswiki.org/emacs/ParEdit
;(autoload 'paredit-mode "paredit"
;  "Minor mode for pseudo-structurally editing Lisp code." t)
;(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
;(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
;(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
;(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
;;(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))

;(add-to-list 'load-path "~/.emacs.d/site-lisp/clojure-mode")
;(require 'clojure-mode)

(defun clang-format-on-save ()
  (add-hook 'before-save-hook #'clang-format-buffer nil 'local))
(add-hook 'c++-mode-hook 'clang-format-on-save)
(add-hook 'c-mode-hook 'clang-format-on-save)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(require 'blacken)
(add-hook 'python-mode-hook 'blacken-mode)

(setq inferior-lisp-program "lein repl")

(defun heiner-indent-shift-left (start end &optional count)
  "Copied from python-indent-shift-left in python.el"
  (interactive
   (if mark-active
       (list (save-excursion
               (goto-char (region-beginning)) (line-beginning-position))
             (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count python-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (user-error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(global-set-key [S-tab] 'heiner-indent-shift-left)

;(eval-after-load 'paredit
;  '(progn (define-key paredit-mode-map (kbd ")")
;            'paredit-close-parenthesis)
;          (define-key paredit-mode-map (kbd "M-)")
;            'paredit-close-parenthesis-and-newline)))

;; Insprired by the tab from kde-emacs-core.el, see
;; http://websvn.kde.org/trunk/KDE/kdesdk/scripts/kde-emacs/
;; Version there originally by Arnt Gulbrandsen ("agulbra")
(defun agulbra-tab (arg &optional skip-abbrev)
  "Do the right thing about tabs."
  (interactive "*P")
  (cond
   ((region-active-p)
    (indent-rigidly
     (save-excursion
       (goto-char (region-beginning))
       (line-beginning-position))
     (region-end)
     4)
    (setq deactivate-mark nil))
   ((and (not skip-abbrev)
         (not (looking-at "[[:alnum:]]"))
         (save-excursion
	   (backward-char)
           (looking-at "[[:alnum:]:>_\\-\\&\\.{}\\*\\+/]"))
	 ;; if in ruby mode, don't try to expand "end"
	 (or (not (equal major-mode 'ruby-mode))
             (not (looking-back "end"))))
    (condition-case nil
	(dabbrev-expand arg)
      ;; When no abbrev found, indent or fixup whitespace
      (error (agulbra-tab arg t))))
   ((looking-at "[[:space:]]*$")
    (delete-horizontal-space)
    (indent-for-tab-command))
   ((and (or (looking-at "[[:space:]]")
             (save-excursion
               (backward-char)
               (looking-at "[[:space:]]")))
         (not (string-match "^[[:space:]]*$"
                            (buffer-substring-no-properties
                             (line-beginning-position)
                             (point)))))
    (let ((current-point (point)))
      (indent-for-tab-command)
      (when (= current-point (point))
        (let ((line-end (line-end-position)))
          (fixup-whitespace)
          (if (and (not (= line-end (line-end-position)))
                   (looking-at "[[:space:]]"))
              (forward-char)
            (goto-char current-point))))))
   (t
    (indent-for-tab-command))))

(global-set-key [tab] 'agulbra-tab)
;; The above does not work in the minibuffer, hence:
(define-key minibuffer-local-map [tab] 'minibuffer-complete)
                                        ; But be careful about ido, see init.el!
