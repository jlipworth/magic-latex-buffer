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

(ert-deftest ml-test/search-regexp-skips-escaped-commands ()
  (with-temp-buffer
    (insert "\\\\alpha then \\alpha")
    (goto-char (point-min))
    (let ((ml/jit-point (point-min)))
      (should (ml/search-regexp "\\\\alpha\\>"))
      (should (equal "\\alpha" (match-string-no-properties 0)))
      (should (= 14 (match-beginning 0))))))

(ert-deftest ml-test/skip-blocks-preserves-match-data ()
  (with-temp-buffer
    (insert "{outer {inner} tail}")
    (goto-char (point-min))
    (set-match-data '(1 1))
    (should (ml/skip-blocks 0))
    (should (= (point) (point-max)))
    (should (equal '(1 1) (match-data t)))))

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
