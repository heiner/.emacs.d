
(require 'heiner-devel)

(defconst lilypond-command "lilypond"
  "Command to run LaTeX")

(add-hook
 'LilyPond-mode-hook
 (lambda ()
   (abbrev-mode t)
   (font-lock-mode t)

   (define-key LilyPond-mode-map [(f4)]
     ;; this works great, except in the case or compilation errors ...
     '(lambda () (interactive)
        (save-buffer)
        (compile (concat lilypond-command " " buffer-file-name))
        (delete-other-windows)))

   (define-key LilyPond-mode-map [(f8)]
     '(lambda () (interactive)
        (heiner-enclose-by "{" "}")))
   ))
