
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
(add-hook 'java-mode-hook
          (lambda () (define-key java-mode-map [(f4)]
                       '(lambda () (interactive)
                          (save-buffer)
                          (compile "ant debug -find")))))

;; Adapted from emacswiki.org/emacs/CompileCommand
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (delete-windows-on buffer))
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (select-frame-set-input-focus current-frame))

(add-to-list 'compilation-finish-functions
	     'notify-compilation-result)

;;; For compilation frame; from
;;; http://snarfed.org/emacs_special-display-function_prefer-other-visible-frame
(defvar non-special-display-buffer-names
  '("*Completions*" " *minibuf-isearch*")
  "Buffers that should *not* be handled by `prefer-other-visible-frame'.
   (I'd rather just exclude them in `special-display-regexps', but elisp regex
   doesn't support negative lookahead, ie (?!...), so i can't.)")

(defvar special-display-switch-to-regexp
  "^.TAGS: "
  "Regexp for buffer names that should be selected (switched to) when
   handled by `prefer-other-visible-frame'.")

(defun prefer-other-visible-frame (buffer &optional buffer-data)
  "If other frames are visible, display the buffer in one of them.
Otherwise, display the buffer in this frame in another window. If
there's only one window, split to create another. Also hide the
buffer in all windows other than the window it gets displayed in.

Intended to be set as `special-display-function'.

Ignores buffers in `non-special-display-buffer-names'.

I can *almost* do this with frame parameters alone, e.g. visibility, except for
forcing it to use a different frame if possible. (same-frame . t) lets you force
it to use the *same* frame, but (same-frame . nil) doesn't force a different
one.

http://www.gnu.org/software/emacs/manual/html_node/emacs/Special-Buffer-Frames.html


http://www.gnu.org/software/emacs/manual/html_node/emacs/Frame-Parameters.html

I wish i could just use `switch-to-buffer-other-window' here, but it calls
`display-buffer', which ends up calling this again. Sigh."
   ;; i'd like to use `other-frame', but it raises the frame :/
  (let ((window
    (cond
     ((member (buffer-name buffer) non-special-display-buffer-names) nil)
     ;; is the buffer already displayed in another visible window?
     ((get-buffer-window buffer 'visible))
     ;; find another window
     (t (let* ((existing (next-window (selected-window) 'never-minibuf 'visible))
               (new (if (and existing (not (eq existing (selected-window))))
                      ; found another existing one
                      existing
                      ; couldn't find an existing one; splitting the current one
                      (split-window))))
          (set-window-buffer new buffer)
          new)))))
    (when (string-match special-display-switch-to-regexp (buffer-name buffer))
      (select-frame (window-frame window))
      (select-window window))
    window))
