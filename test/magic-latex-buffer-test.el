;;; magic-latex-buffer-test.el --- Tests for Magic LaTeX -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Functional and visual-contract tests for Magic LaTeX's scanners and
;; overlays.  Run with `make test'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magic-latex-buffer)

(defconst ml-test/root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defconst ml-test/fixture
  (expand-file-name "test/fixtures/large-document.tex" ml-test/root))

(defun ml-test/clear-overlays ()
  "Delete every overlay created by Magic LaTeX."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (pcase (overlay-get overlay 'category)
      ('ml/ov-block
       (when-let ((partner (overlay-get overlay 'partner)))
         (delete-overlay partner))
       (delete-overlay overlay))
      ('ml/ov-align
       (mapc #'delete-overlay (overlay-get overlay 'partners))
       (delete-overlay overlay))
      ((or 'ml/ov-pretty 'ml/ov-align-alignment)
       (delete-overlay overlay)))))

(defun ml-test/render (content)
  "Render CONTENT and return all Magic LaTeX command overlays."
  (with-temp-buffer
    (insert content)
    (setq buffer-file-name "magic-latex-test.tex")
    (latex-mode)
    (font-lock-mode 1)
    (magic-latex-buffer 1)
    (font-lock-ensure)
    (ml-test/clear-overlays)
    (let ((ml/jit-point (point-max))
          (magic-latex-enable-block-align nil)
          (magic-latex-enable-inline-image nil))
      (goto-char (point-min))
      (ml/jit-block-highlighter (point-min) (point-max))
      (goto-char (point-min))
      (ml/jit-prettifier (point-min) (point-max))
      (mapcar
       (lambda (overlay)
         (list (overlay-start overlay)
               (overlay-end overlay)
               (buffer-substring-no-properties
                (overlay-start overlay) (overlay-end overlay))
               (overlay-get overlay 'category)
               (format "%S" (overlay-get overlay 'display))))
       (cl-remove-if-not
        (lambda (overlay)
          (memq (overlay-get overlay 'category)
                '(ml/ov-pretty ml/ov-block)))
        (overlays-in (point-min) (point-max)))))))

(defun ml-test/reference-search-regexp
    (regex &optional bound backward point-safe)
  "Original recursive implementation of `ml/search-regexp'."
  (ml/safe-excursion
   (let ((case-fold-search nil))
     (if backward
         (search-backward-regexp regex bound)
       (search-forward-regexp regex bound)))
   (or (save-match-data
         (save-excursion
           (and (goto-char (match-beginning 0))
                (not (and point-safe
                          (< (point) ml/jit-point)
                          (< ml/jit-point (match-end 0))))
                (looking-back "\\([^\\\\]\\|^\\)\\(\\\\\\\\\\)*" (point-min))
                (not (ml/skip-comments-and-verbs backward)))))
       (ml-test/reference-search-regexp
        regex bound backward point-safe))))

(defun ml-test/search-outcome (search regex backward point-safe)
  "Capture the observable result of calling SEARCH."
  (condition-case error-data
      (let ((value (funcall search regex nil backward point-safe)))
        (list 'success value (point) (match-data t) (match-string 0)))
    (error
     (list 'error (car error-data) (error-message-string error-data) (point)))))

(defun ml-test/compare-searches
    (content regex start jit-point &optional backward point-safe ignored-range)
  "Compare reference and optimized searches over CONTENT from START."
  (with-temp-buffer
    (insert content)
    (when ignored-range
      (put-text-property (car ignored-range) (cdr ignored-range)
                         'face 'font-lock-comment-face))
    (let ((ml/jit-point jit-point))
      (goto-char start)
      (let ((reference
             (ml-test/search-outcome
              #'ml-test/reference-search-regexp regex backward point-safe)))
        (goto-char start)
        (should
         (equal reference
                (ml-test/search-outcome
                 #'ml/search-regexp regex backward point-safe)))))))

(defun ml-test/reference-prettify-symbols (beg end)
  "Run the original one-regexp-per-symbol implementation from BEG to END."
  (dolist (symbol ml/symbols)
    (save-excursion
      (goto-char beg)
      (let ((regex (car symbol)))
        (while (ignore-errors (ml/search-regexp regex end nil t))
          (let* ((old-overlay
                  (ml/overlay-at
                   (match-beginning 0) 'category 'ml/ov-pretty))
                 (priority-base
                  (and old-overlay
                       (or (overlay-get old-overlay 'priority) 1)))
                 (old-display
                  (and old-overlay (overlay-get old-overlay 'display))))
            (unless (stringp old-display)
              (ml/make-pretty-overlay
               (match-beginning 0) (match-end 0)
               'priority (when old-overlay (1+ priority-base))
               'display
               (propertize
                (eval (cdr symbol)) 'display old-display)))))))))

(defun ml-test/reference-jit-block-highlighter (_ end)
  "Run the original one-pass-per-command block highlighter to END."
  (when magic-latex-enable-block-highlight
    (condition-case nil
        (progn (ml/skip-blocks 1 nil t) (point))
      (error (goto-char 1)))
    (ml/remove-block-overlays (point) end)
    (dolist (command ml/block-commands)
      (save-excursion
        (while (funcall (car command) end)
          (ml/make-block-overlay (match-beginning 0) (match-end 0)
                                 (match-beginning 1) (match-end 1)
                                 'face (eval (cdr command))))))))

(defun ml-test/symbol-snapshot (prettifier content)
  "Run PRETTIFIER over CONTENT and return canonical symbol overlays."
  (with-temp-buffer
    (insert content)
    (setq buffer-file-name "magic-latex-symbol-parity.tex")
    (latex-mode)
    (font-lock-mode 1)
    (magic-latex-buffer 1)
    (font-lock-ensure)
    (ml-test/clear-overlays)
    (let ((ml/jit-point (point-max)))
      (set-syntax-table ml/syntax-table)
      (goto-char (point-min))
      (funcall prettifier (point-min) (point-max))
      (sort
       (mapcar
        (lambda (overlay)
          (list
           (overlay-start overlay)
           (overlay-end overlay)
           (buffer-substring-no-properties
            (overlay-start overlay) (overlay-end overlay))
           (format "%S" (overlay-get overlay 'display))
           (format "%S" (overlay-get overlay 'priority))))
        (cl-remove-if-not
         (lambda (overlay)
           (eq (overlay-get overlay 'category) 'ml/ov-pretty))
         (overlays-in (point-min) (point-max))))
       (lambda (left right)
         (or (< (car left) (car right))
             (and (= (car left) (car right))
                  (< (cadr left) (cadr right)))))))))

(defun ml-test/block-snapshot (highlighter content)
  "Run HIGHLIGHTER over CONTENT and capture block rendering semantics."
  (with-temp-buffer
    (insert content)
    (setq buffer-file-name "magic-latex-block-parity.tex")
    (latex-mode)
    (font-lock-mode 1)
    (magic-latex-buffer 1)
    (font-lock-ensure)
    (ml-test/clear-overlays)
    (let ((ml/jit-point (point-max))
          (magic-latex-enable-block-align nil))
      (set-syntax-table ml/syntax-table)
      (goto-char (point-min))
      (funcall highlighter (point-min) (point-max))
      (list
       (sort
        (mapcar
         (lambda (overlay)
           (let ((partner (overlay-get overlay 'partner)))
             (list
              (overlay-start overlay)
              (overlay-end overlay)
              (buffer-substring-no-properties
               (overlay-start overlay) (overlay-end overlay))
              (overlay-start partner)
              (overlay-end partner)
              (format "%S" (overlay-get partner 'face)))))
         (cl-remove-if-not
          (lambda (overlay)
            (eq (overlay-get overlay 'category) 'ml/ov-block))
          (overlays-in (point-min) (point-max))))
        (lambda (left right)
          (or (< (car left) (car right))
              (and (= (car left) (car right))
                   (< (cadr left) (cadr right))))))
       (cl-loop for position from (point-min) below (point-max)
                collect (format "%S" (get-char-property position 'face)))))))

(ert-deftest ml-test/search-regexp-skips-escaped-commands ()
  (with-temp-buffer
    (insert "\\\\alpha then \\alpha")
    (goto-char (point-min))
    (let ((ml/jit-point (point-min)))
      (should (ml/search-regexp "\\\\alpha\\>"))
      (should (equal "\\alpha" (match-string-no-properties 0)))
      (should (= 14 (match-beginning 0))))))

(ert-deftest ml-test/search-regexp-preserves-reference-contract ()
  (let ((alpha "\\\\alpha\\>"))
    (ml-test/compare-searches "\\alpha and \\alpha" alpha 1 1)
    (ml-test/compare-searches "\\alpha and \\alpha" alpha 18 1 t)
    (ml-test/compare-searches "\\\\alpha then \\alpha" alpha 1 1)
    (ml-test/compare-searches "\\alpha then \\\\alpha" alpha 20 1 t)
    (ml-test/compare-searches "\\alpha then \\alpha" alpha 1 4 nil t)
    (ml-test/compare-searches
     "x\\alpha then \\alpha" alpha 1 1 nil nil '(1 . 8))
    (ml-test/compare-searches "\\\\alpha" alpha 1 1)
    (ml-test/compare-searches "text" "[" 3 1)))

(ert-deftest ml-test/skip-blocks-preserves-match-data ()
  (with-temp-buffer
    (insert "{outer {inner} tail}")
    (goto-char (point-min))
    (set-match-data '(1 1))
    (should (ml/skip-blocks 0))
    (should (= (point) (point-max)))
    (should (equal '(1 1) (match-data t)))))

(ert-deftest ml-test/symbol-plan-preserves-every-rule ()
  (let ((rules
         (apply
          #'+
          (mapcar
           (lambda (segment)
             (if (eq 'exact (car segment))
                 (hash-table-count (nth 2 segment))
               1))
           (ml/build-symbol-plan)))))
    (should (= (length ml/symbols) rules))
    (should (< (length (ml/build-symbol-plan)) (length ml/symbols)))))

(ert-deftest ml-test/segmented-symbols-match-reference-on-generic-fixture ()
  (let ((content (with-temp-buffer
                   (insert-file-contents ml-test/fixture)
                   (buffer-string))))
    (should
     (equal
      (ml-test/symbol-snapshot #'ml-test/reference-prettify-symbols content)
      (ml-test/symbol-snapshot #'ml/prettify-symbols content)))))

(ert-deftest ml-test/symbol-plan-preserves-overlapping-rule-order ()
  (dolist
      (symbols
       '((("\\\\f\\(?:oo\\)\\>" . "regexp-first")
          ("\\\\foo\\>" . "exact-second"))
         (("\\\\foo\\>" . "exact-first")
          ("\\\\f\\(?:oo\\)\\>" . "regexp-second"))
         (("\\\\foo\\>" . "duplicate-first")
          ("\\\\foo\\>" . "duplicate-second"))
         (("\\\\\\([[:alpha:]]+\\)\\>" . (upcase (match-string 1)))
          ("\\\\foo\\>" . "exact-after-capture"))))
    (let ((ml/symbols symbols)
          (ml/symbol-plan-cache nil)
          (ml/symbol-plan-source nil)
          (content "\\foo \\\\foo % \\foo\n\\begin{verbatim}\\foo\\end{verbatim}\n"))
      (should
       (equal
        (ml-test/symbol-snapshot #'ml-test/reference-prettify-symbols content)
        (ml-test/symbol-snapshot #'ml/prettify-symbols content))))))

(ert-deftest ml-test/block-highlighter-preserves-nesting-and-command-order ()
  (let ((content
         (concat
          "{\\large outer {\\bfseries bold {\\color{blue} blue}} tail}\n"
          "{\\small one \\large two \\bfseries three}\n"
          "{\\tiny a} {\\scriptsize b} {\\footnotesize c} {\\small d} "
          "{\\large e} {\\Large f} {\\LARGE g} {\\huge h} {\\Huge i} "
          "{\\tt j} {\\em k} {\\it l} {\\sl m} {\\bf n} "
          "{\\bfseries o} {\\color{black} p} {\\color{white} q} "
          "{\\color{red} r} {\\color{green} s} {\\color{blue} t} "
          "{\\color{cyan} u} {\\color{magenta} v} {\\color{yellow} w} "
          "{\\color{purple} unsupported} % \\large ignored\n"
          "\\\\large escaped\n")))
    (should
     (equal
      (ml-test/block-snapshot
       #'ml-test/reference-jit-block-highlighter content)
      (ml-test/block-snapshot #'ml/jit-block-highlighter content)))))

(ert-deftest ml-test/generic-fixture-exercises-all-display-families ()
  (let ((content (with-temp-buffer
                   (insert-file-contents ml-test/fixture)
                   (buffer-string))))
    (should (> (string-bytes content) (* 120 1024)))
    (let ((snapshot (ml-test/render content)))
      (should (> (length snapshot) 1000))
      (dolist (source '("\\alpha" "\\sum" "\\mathbb{R}" "\\vec{x}"
                        "\\large" "\\bfseries" "_" "^" "~"))
        (should (cl-find source snapshot :key #'caddr :test #'equal))))))

(ert-deftest ml-test/comments-and-verbatim-remain-literal ()
  (let ((snapshot
         (ml-test/render
          (concat
           "Visible: $\\alpha$.\n"
           "% Hidden: \\alpha.\n"
           "\\begin{verbatim}\n\\alpha\n\\end{verbatim}\n"))))
    (should (= 1 (cl-count "\\alpha" snapshot :key #'caddr :test #'equal)))))

;;; magic-latex-buffer-test.el ends here
