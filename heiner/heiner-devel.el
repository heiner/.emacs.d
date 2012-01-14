
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

;; Adapted from emacswiki.org/emacs/CompileCommand
(defun delete-compilation-window-if-successful (buffer msg)
  "Delete the compilation window if the compilation was successful and
the compilation window did not have a frame of its own."
  (if (and (string-match "^Compilation finished" msg) (not (one-window-p)))
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
