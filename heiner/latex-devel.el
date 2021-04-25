
;;(require 'tex-site)
;(require 'filladapt)
;;(pending-delete-mode t)
;;(autoload 'latex-mode "auc-tex" "Mode for LaTeX" t)

(defconst latex-command "/Library/TeX/texbin/pdflatex"
  "Command to run LaTeX")

;; Ideas to do:
;;   - completing-read for environments

(defun hook-fun ()
   (abbrev-mode t)
   (font-lock-mode t)
   (turn-on-auto-fill)
   ;(filladapt-mode t)

   (local-set-key "\"" 'self-insert-command)

   (local-set-key [(meta ?.)]
     '(lambda () (interactive) (insert "\\dotsc")))

   (local-set-key [(meta m)(m)]
     '(lambda () (interactive)
        (heiner-enclose-by "$" "$")))

   (local-set-key [(meta m)(f)]
     '(lambda () (interactive)
        (heiner-enclose-by "\\frac{" "}{}")))

   (local-set-key [(f5)]
     '(lambda () (interactive)
        (heiner-latex-insert-environment
         (read-from-minibuffer "Environment: "))))

   (local-set-key [(f6)]
     '(lambda () (interactive)
        (heiner-latex-insert-command
         (read-from-minibuffer "Command: "))))

   (local-set-key [(f7)]
     'heiner-latex-insert-math-display)

   (local-set-key [(meta m)(d)]
     'heiner-latex-insert-math-display)

   (local-set-key [(meta m)(1)]
     '(lambda () (interactive)
        (insert "^{-1}")))

   (local-set-key [(meta m)(<)]
     '(lambda () (interactive)
        (heiner-enclose-by "\\<" "\\>")))

   (local-set-key [(meta m)(~)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "tilde")))

   (local-set-key [(meta m)(|)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "abs")))

   (local-set-key (kbd "M-m ¦")
     '(lambda () (interactive)
        (heiner-latex-insert-command "norm")))

   (local-set-key [(meta m)({)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "bigbraces")))

   (local-set-key [(meta m)(s)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "sqrt")))

   (local-set-key [(meta m)(r)]
     '(lambda () (interactive)
        (heiner-enclose-by "\\sqrt[]{" "}")))

   (local-set-key [(meta m)(?8)]
     '(lambda () (interactive)
        (insert "\\infty")))

   (local-set-key [(meta m)(?0)]
     '(lambda () (interactive)
        (insert "\\varnothing")))

   (local-set-key [(meta m)(?\\)]
     '(lambda () (interactive)
        (insert "\\setminus")))

   (local-set-key [(meta m)(?\()]
     '(lambda () (interactive)
        (heiner-latex-insert-command "Bigparens")))

   (local-set-key [(meta m)(p)]
     '(lambda () (interactive)
        (insert "\\partial")))

   (local-set-key [(meta m)(n)]
     '(lambda () (interactive)
        (insert "\\nabla")))

   (local-set-key [(meta m)(x)]
     '(lambda () (interactive)
        (insert "\\times")))

   (local-set-key [(meta m)(o)]
     '(lambda () (interactive)
        (insert "\\otimes")))

   (local-set-key [(meta m)(^)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "hat")))

   (local-set-key [(meta m)(_)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "overline")))

   (local-set-key [(meta m)(?1)]
     '(lambda () (interactive)
        (insert "^{-1}")))

   (local-set-key [(f8)]
     '(lambda () (interactive)
        (heiner-enclose-by "{" "}")))

   (local-set-key [(shift f8)]
     '(lambda () (interactive)
        (heiner-enclose-by "[" "]")))

   (local-set-key [(f4)]
     ;; this works great, except in the case or compilation errors ...
     '(lambda () (interactive)
        (set-buffer-modified-p t)
        (save-buffer)
        (if (file-exists-p "Makefile")
          (compile "make -k")
          (compile (concat latex-command " " buffer-file-name)))))

   ;; compare amsldoc.pdf, 4.3 (PDF page 16/17)
   (local-set-key [(meta m)(?.)(?,)]
     '(lambda () "dots with commas"
        (interactive)
        (insert "\\dotsc")))
   (local-set-key [(meta m)(?.)(?b)]
     '(lambda () "dots with binary operators/relations"
        (interactive)
        (insert "\\dotsb")))
   (local-set-key [(meta m)(?.)(?+)]
     '(lambda () "dots with binary operators/relations"
        (interactive)
        (insert "\\dotsb")))
   (local-set-key [(meta m)(?.)(m)]
     '(lambda () "multiplication dots"
        (interactive)
        (insert "\\dotsm")))
   (local-set-key [(meta m)(?.)(i)]
     '(lambda () "dots with integrals"
        (interactive)
        (insert "\\dotsi")))
   (local-set-key [(meta m)(?.)(o)]
     '(lambda () "other dots"
        (interactive)
        (insert "\\dotso")))

   (local-set-key [(meta m)(?.)(?.)]
     '(lambda () "central dot"
        (interactive)
        (insert "\\cdot")))

   (local-set-key [(control return)] nil)

   (local-set-key [(meta m)(g)(a)]
     '(lambda () (interactive) (insert "\\alpha")))
   (local-set-key [(meta m)(g)(b)]
     '(lambda () (interactive) (insert "\\beta")))
   (local-set-key [(meta m)(g)(c)]
     '(lambda () (interactive) (insert "\\chi")))
   (local-set-key [(meta m)(g)(d)]
     '(lambda () (interactive) (insert "\\delta")))
   (local-set-key [(meta m)(g)(e)]
     '(lambda () (interactive) (insert "\\varepsilon")))
   (local-set-key [(meta m)(g)(f)]
     '(lambda () (interactive) (insert "\\varphi")))
   (local-set-key [(meta m)(g)(g)]
     '(lambda () (interactive) (insert "\\gamma")))
   (local-set-key [(meta m)(g)(h)]
     '(lambda () (interactive) (insert "\\eta")))
   (local-set-key [(meta m)(g)(i)]
     '(lambda () (interactive) (insert "\\iota")))
   (local-set-key [(meta m)(g)(j)]
     '(lambda () (interactive) (insert "\\phi")))
   (local-set-key [(meta m)(g)(k)]
     '(lambda () (interactive) (insert "\\kappa")))
   (local-set-key [(meta m)(g)(l)]
     '(lambda () (interactive) (insert "\\lambda")))
   (local-set-key [(meta m)(g)(m)]
     '(lambda () (interactive) (insert "\\mu")))
   (local-set-key [(meta m)(g)(n)]
     '(lambda () (interactive) (insert "\\nu")))
   (local-set-key [(meta m)(g)(o)]
     '(lambda () (interactive) (insert "\\omega")))
   (local-set-key [(meta m)(g)(p)]
     '(lambda () (interactive) (insert "\\pi")))
   (local-set-key [(meta m)(g)(q)]
     '(lambda () (interactive) (insert "\\vartheta")))
   (local-set-key [(meta m)(g)(r)]
     '(lambda () (interactive) (insert "\\rho")))
   (local-set-key [(meta m)(g)(s)]
     '(lambda () (interactive) (insert "\\sigma")))
   (local-set-key [(meta m)(g)(t)]
     '(lambda () (interactive) (insert "\\tau")))
   (local-set-key [(meta m)(g)(u)]
     '(lambda () (interactive) (insert "\\upsilon")))
   (local-set-key [(meta m)(g)(v)]
     '(lambda () (interactive) (insert "\\theta")))
   (local-set-key [(meta m)(g)(w)]
     '(lambda () (interactive) (insert "\\omega")))
   (local-set-key [(meta m)(g)(x)]
     '(lambda () (interactive) (insert "\\xi")))
   (local-set-key [(meta m)(g)(y)]
     '(lambda () (interactive) (insert "\\psi")))
   (local-set-key [(meta m)(g)(z)]
     '(lambda () (interactive) (insert "\\zeta")))

   (local-set-key [(meta m)(g)(D)]
     '(lambda () (interactive) (insert "\\Delta")))
   (local-set-key [(meta m)(g)(E)]
     '(lambda () (interactive) (insert "\\epsilon")))
   (local-set-key [(meta m)(g)(F)]
     '(lambda () (interactive) (insert "\\Phi")))
   (local-set-key [(meta m)(g)(G)]
     '(lambda () (interactive) (insert "\\Gamma")))
   (local-set-key [(meta m)(g)(I)]
     '(lambda () (interactive) (insert "\\iota")))
   (local-set-key [(meta m)(g)(J)]
     '(lambda () (interactive) (insert "\\epsilon")))
   (local-set-key [(meta m)(g)(L)]
     '(lambda () (interactive) (insert "\\Lambda")))
   (local-set-key [(meta m)(g)(O)]
     '(lambda () (interactive) (insert "\\Omega")))
   (local-set-key [(meta m)(g)(P)]
     '(lambda () (interactive) (insert "\\Pi")))
   (local-set-key [(meta m)(g)(Q)]
     '(lambda () (interactive) (insert "\\vartheta")))
   (local-set-key [(meta m)(g)(R)]
     '(lambda () (interactive) (insert "\\varrho")))
   (local-set-key [(meta m)(g)(S)]
     '(lambda () (interactive) (insert "\\Sigma")))
   (local-set-key [(meta m)(g)(T)]
     '(lambda () (interactive) (insert "\\varsigma")))
   (local-set-key [(meta m)(g)(U)]
     '(lambda () (interactive) (insert "\\Upsilon")))
   (local-set-key [(meta m)(g)(V)]
     '(lambda () (interactive) (insert "\\Theta")))
   (local-set-key [(meta m)(g)(W)]
     '(lambda () (interactive) (insert "\\Omega")))
   (local-set-key [(meta m)(g)(X)]
     '(lambda () (interactive) (insert "\\Xi")))
   (local-set-key [(meta m)(g)(Y)]
     '(lambda () (interactive) (insert "\\Psi")))

   (define-abbrev latex-mode-abbrev-table "enum" ""
     (lambda () (interactive)
       (heiner-latex-insert-environment "enumerate")
       (end-of-previous-line)
       (next-line 1)
       (insert "\\item")))

   (define-abbrev latex-mode-abbrev-table "itemi" ""
     (lambda () (interactive)
       (heiner-latex-insert-environment "itemize")
       (insert "\\item")))

   (define-abbrev latex-mode-abbrev-table "eqr" ""
     (lambda () (interactive)
       (insert "\\eqref{eq:}")
       (backward-char))))

(add-hook
 'latex-mode-hook
 'hook-fun)

(add-hook
 'markdown-mode-hook
 'hook-fun)

(defun end-of-previous-line ()
  (previous-line 1)
  (end-of-line))

(defun heiner-latex-indented-enclose-by (open close)
  "Insert open and close around the region or an empty line, with indention"
  (if (region-active-p)
      (let ((content (buffer-substring-no-properties
                      (region-beginning) (region-end)))
            (origin (point))
            (was-beginning (= (point) (region-beginning))))
        (delete-region (region-beginning) (region-end))

        (let ((ibegin (point))
              (content (if (string-equal
                            (substring content -1 nil) "\n")
                           content
                         (concat content "\n"))))
          (insert (concat open "\n" content close))
          (unless (looking-at "\n")
            (insert "\n"))
          (indent-region ibegin (point)))
        (goto-char origin)
        (if was-beginning
            (progn
              (next-line)
              (skip-chars-forward " "))
          (end-of-line)))
    (indent-for-tab-command)
    (insert (concat open "\n\n" close))
    (indent-for-tab-command)
    (previous-line 1)
    (indent-for-tab-command)))

(defun heiner-latex-insert-environment (env)
  "Insert environment env the right way"
  (heiner-latex-indented-enclose-by (concat "\\begin{" env "}")
                                    (concat "\\end{" env "}")))

(defun heiner-latex-insert-math-display ()
  "Insert \[ \] the right way"
  (interactive)
  (heiner-latex-indented-enclose-by "\\[" "\\]"))

(defun heiner-latex-insert-command (cmd)
  "Insert command cmd the right way"
  (heiner-enclose-by (concat "\\" cmd "{") "}"))

;; (defun heiner-latex-insert-environment-with-label (env)
;;   "Insert environment env with a label, the right way"
;;   (heiner-latex-insert-environment env)
;;   (end-of-previous-line)
;;   (insert (format "\\label{%s:}" env))
;;   (backward-char))

;; (add-hook
;;  'latex-mode-hook
;;  (lambda ()
;;    (local-set-key [(meta m)(t)(t)]
;;      '(lambda () (interactive)
;;         (heiner-latex-insert-environment-with-label "theorem")))
;;    (local-set-key [(meta m)(t)(l)]
;;      '(lambda () (interactive)
;;         (heiner-latex-insert-environment-with-label "lemma")))
;;    (local-set-key [(meta m)(t)(c)]
;;      '(lambda () (interactive)
;;         (heiner-latex-insert-environment-with-label "corollary")))
;;    (local-set-key [(meta m)(t)(r)]
;;      '(lambda () (interactive)
;;         (heiner-latex-insert-environment "remark")))))
