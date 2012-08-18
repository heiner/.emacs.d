
;;(require 'tex-site)
(require 'filladapt)
;;(pending-delete-mode t)
;;(autoload 'latex-mode "auc-tex" "Mode for LaTeX" t)

(defconst latex-command "pdflatex"
  "Command to run LaTeX")

;; Ideas to do:
;;   - completing-read for environments

(add-hook
 'latex-mode-hook
 (lambda ()
   (abbrev-mode t)
   (font-lock-mode t)
   (turn-on-auto-fill)
   (filladapt-mode t)

   (define-key latex-mode-map "\"" 'self-insert-command)

   (define-key latex-mode-map [(meta ?.)]
     '(lambda () (interactive) (insert "\\dotsc")))

   (define-key latex-mode-map [(meta m)(m)]
     '(lambda () (interactive)
        (heiner-enclose-by "$" "$")))

   (define-key latex-mode-map [(meta m)(f)]
     '(lambda () (interactive)
        (heiner-enclose-by "\\frac{" "}{}")))

   (define-key latex-mode-map [(f5)]
     '(lambda () (interactive)
        (heiner-latex-insert-environment
         (read-from-minibuffer "Environment: "))))

   (define-key latex-mode-map [(f6)]
     '(lambda () (interactive)
        (heiner-latex-insert-command
         (read-from-minibuffer "Command: "))))

   (define-key latex-mode-map [(f7)]
     'heiner-latex-insert-math-display)

   (define-key latex-mode-map [(meta m)(d)]
     'heiner-latex-insert-math-display)

   (define-key latex-mode-map [(meta m)(1)]
     '(lambda () (interactive)
        (insert "^{-1}")))

   (define-key latex-mode-map [(meta m)(<)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "angles")))

   (define-key latex-mode-map [(meta m)(~)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "tilde")))

   (define-key latex-mode-map [(meta m)(|)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "abs")))

   (define-key latex-mode-map (kbd "M-m ¦")
     '(lambda () (interactive)
        (heiner-latex-insert-command "norm")))

   (define-key latex-mode-map [(meta m)({)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "bigbraces")))

   (define-key latex-mode-map [(meta m)(s)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "sqrt")))

   (define-key latex-mode-map [(meta m)(r)]
     '(lambda () (interactive)
        (heiner-enclose-by "\\sqrt[]{" "}")))

   (define-key latex-mode-map [(meta m)(?8)]
     '(lambda () (interactive)
        (insert "\\infty")))

   (define-key latex-mode-map [(meta m)(?\\)]
     '(lambda () (interactive)
        (insert "\\setminus")))

   (define-key latex-mode-map [(meta m)(?\()]
     '(lambda () (interactive)
        (heiner-latex-insert-command "Bigparens")))

   (define-key latex-mode-map [(meta m)(p)]
     '(lambda () (interactive)
        (insert "\\partial")))

   (define-key latex-mode-map [(meta m)(n)]
     '(lambda () (interactive)
        (insert "\\nabla")))

   (define-key latex-mode-map [(meta m)(x)]
     '(lambda () (interactive)
        (insert "\\times")))

   (define-key latex-mode-map [(meta m)(o)]
     '(lambda () (interactive)
        (insert "\\otimes")))

   (define-key latex-mode-map [(meta m)(^)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "hat")))

   (define-key latex-mode-map [(meta m)(_)]
     '(lambda () (interactive)
        (heiner-latex-insert-command "overline")))

   (define-key latex-mode-map [(meta m)(?1)]
     '(lambda () (interactive)
        (insert "^{-1}")))

   (define-key latex-mode-map [(f8)]
     '(lambda () (interactive)
        (heiner-enclose-by "{" "}")))

   (define-key latex-mode-map [(f4)]
     ;; this works great, except in the case or compilation errors ...
     '(lambda () (interactive)
        (save-buffer)
        (compile (concat latex-command " " buffer-file-name))))

   ;; compare amsldoc.pdf, 4.3 (PDF page 16/17)
   (define-key latex-mode-map [(meta m)(?.)(?,)]
     '(lambda () "dots with commas"
        (interactive)
        (insert "\\dotsc")))
   (define-key latex-mode-map [(meta m)(?.)(?b)]
     '(lambda () "dots with binary operators/relations"
        (interactive)
        (insert "\\dotsb")))
   (define-key latex-mode-map [(meta m)(?.)(?+)]
     '(lambda () "dots with binary operators/relations"
        (interactive)
        (insert "\\dotsb")))
   (define-key latex-mode-map [(meta m)(?.)(m)]
     '(lambda () "multiplication dots"
        (interactive)
        (insert "\\dotsm")))
   (define-key latex-mode-map [(meta m)(?.)(i)]
     '(lambda () "dots with integrals"
        (interactive)
        (insert "\\dotsi")))
   (define-key latex-mode-map [(meta m)(?.)(o)]
     '(lambda () "other dots"
        (interactive)
        (insert "\\dotso")))

   (define-key latex-mode-map [(meta m)(?.)(?.)]
     '(lambda () "central dot"
        (interactive)
        (insert "\\cdot")))

   (define-key latex-mode-map [(control return)] nil)

   (define-key latex-mode-map [(meta m)(g)(a)]
     '(lambda () (interactive) (insert "\\alpha")))
   (define-key latex-mode-map [(meta m)(g)(b)]
     '(lambda () (interactive) (insert "\\beta")))
   (define-key latex-mode-map [(meta m)(g)(c)]
     '(lambda () (interactive) (insert "\\chi")))
   (define-key latex-mode-map [(meta m)(g)(d)]
     '(lambda () (interactive) (insert "\\delta")))
   (define-key latex-mode-map [(meta m)(g)(e)]
     '(lambda () (interactive) (insert "\\varepsilon")))
   (define-key latex-mode-map [(meta m)(g)(f)]
     '(lambda () (interactive) (insert "\\varphi")))
   (define-key latex-mode-map [(meta m)(g)(g)]
     '(lambda () (interactive) (insert "\\gamma")))
   (define-key latex-mode-map [(meta m)(g)(h)]
     '(lambda () (interactive) (insert "\\eta")))
   (define-key latex-mode-map [(meta m)(g)(i)]
     '(lambda () (interactive) (insert "\\iota")))
   (define-key latex-mode-map [(meta m)(g)(j)]
     '(lambda () (interactive) (insert "\\phi")))
   (define-key latex-mode-map [(meta m)(g)(k)]
     '(lambda () (interactive) (insert "\\kappa")))
   (define-key latex-mode-map [(meta m)(g)(l)]
     '(lambda () (interactive) (insert "\\lambda")))
   (define-key latex-mode-map [(meta m)(g)(m)]
     '(lambda () (interactive) (insert "\\mu")))
   (define-key latex-mode-map [(meta m)(g)(n)]
     '(lambda () (interactive) (insert "\\nu")))
   (define-key latex-mode-map [(meta m)(g)(o)]
     '(lambda () (interactive) (insert "\\omega")))
   (define-key latex-mode-map [(meta m)(g)(p)]
     '(lambda () (interactive) (insert "\\pi")))
   (define-key latex-mode-map [(meta m)(g)(q)]
     '(lambda () (interactive) (insert "\\vartheta")))
   (define-key latex-mode-map [(meta m)(g)(r)]
     '(lambda () (interactive) (insert "\\rho")))
   (define-key latex-mode-map [(meta m)(g)(s)]
     '(lambda () (interactive) (insert "\\sigma")))
   (define-key latex-mode-map [(meta m)(g)(t)]
     '(lambda () (interactive) (insert "\\tau")))
   (define-key latex-mode-map [(meta m)(g)(u)]
     '(lambda () (interactive) (insert "\\upsilon")))
   (define-key latex-mode-map [(meta m)(g)(v)]
     '(lambda () (interactive) (insert "\\theta")))
   (define-key latex-mode-map [(meta m)(g)(w)]
     '(lambda () (interactive) (insert "\\omega")))
   (define-key latex-mode-map [(meta m)(g)(x)]
     '(lambda () (interactive) (insert "\\xi")))
   (define-key latex-mode-map [(meta m)(g)(y)]
     '(lambda () (interactive) (insert "\\psi")))
   (define-key latex-mode-map [(meta m)(g)(z)]
     '(lambda () (interactive) (insert "\\zeta")))

   (define-key latex-mode-map [(meta m)(g)(D)]
     '(lambda () (interactive) (insert "\\Delta")))
   (define-key latex-mode-map [(meta m)(g)(E)]
     '(lambda () (interactive) (insert "\\epsilon")))
   (define-key latex-mode-map [(meta m)(g)(F)]
     '(lambda () (interactive) (insert "\\Phi")))
   (define-key latex-mode-map [(meta m)(g)(G)]
     '(lambda () (interactive) (insert "\\Gamma")))
   (define-key latex-mode-map [(meta m)(g)(I)]
     '(lambda () (interactive) (insert "\\iota")))
   (define-key latex-mode-map [(meta m)(g)(J)]
     '(lambda () (interactive) (insert "\\epsilon")))
   (define-key latex-mode-map [(meta m)(g)(L)]
     '(lambda () (interactive) (insert "\\Lambda")))
   (define-key latex-mode-map [(meta m)(g)(O)]
     '(lambda () (interactive) (insert "\\Omega")))
   (define-key latex-mode-map [(meta m)(g)(P)]
     '(lambda () (interactive) (insert "\\Pi")))
   (define-key latex-mode-map [(meta m)(g)(Q)]
     '(lambda () (interactive) (insert "\\vartheta")))
   (define-key latex-mode-map [(meta m)(g)(R)]
     '(lambda () (interactive) (insert "\\varrho")))
   (define-key latex-mode-map [(meta m)(g)(S)]
     '(lambda () (interactive) (insert "\\Sigma")))
   (define-key latex-mode-map [(meta m)(g)(T)]
     '(lambda () (interactive) (insert "\\varsigma")))
   (define-key latex-mode-map [(meta m)(g)(U)]
     '(lambda () (interactive) (insert "\\Upsilon")))
   (define-key latex-mode-map [(meta m)(g)(V)]
     '(lambda () (interactive) (insert "\\Theta")))
   (define-key latex-mode-map [(meta m)(g)(W)]
     '(lambda () (interactive) (insert "\\Omega")))
   (define-key latex-mode-map [(meta m)(g)(X)]
     '(lambda () (interactive) (insert "\\Xi")))
   (define-key latex-mode-map [(meta m)(g)(Y)]
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
       (backward-char)))))

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
              (content (if (char-equal
                            (heiner-char-at-index content -1) ?\n)
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
;;    (define-key latex-mode-map [(meta m)(t)(t)]
;;      '(lambda () (interactive)
;;         (heiner-latex-insert-environment-with-label "theorem")))
;;    (define-key latex-mode-map [(meta m)(t)(l)]
;;      '(lambda () (interactive)
;;         (heiner-latex-insert-environment-with-label "lemma")))
;;    (define-key latex-mode-map [(meta m)(t)(c)]
;;      '(lambda () (interactive)
;;         (heiner-latex-insert-environment-with-label "corollary")))
;;    (define-key latex-mode-map [(meta m)(t)(r)]
;;      '(lambda () (interactive)
;;         (heiner-latex-insert-environment "remark")))))
