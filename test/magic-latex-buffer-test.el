;;; magic-latex-buffer-test.el --- Tests for Magic LaTeX -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Behavioral-contract tests for Magic LaTeX's filtered regexp search.
;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'magic-latex-buffer)

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
  "Compare reference and iterative searches over CONTENT from START."
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

(ert-deftest ml-test/search-regexp-preserves-reference-contract ()
  (let ((alpha "\\\\alpha\\>"))
    ;; Forward and backward matches.
    (ml-test/compare-searches "\\alpha and \\alpha" alpha 1 1)
    (ml-test/compare-searches "\\alpha and \\alpha" alpha 18 1 t)
    ;; Escaped matches are skipped in both directions.
    (ml-test/compare-searches "\\\\alpha then \\alpha" alpha 1 1)
    (ml-test/compare-searches "\\alpha then \\\\alpha" alpha 20 1 t)
    ;; A match containing the JIT point is skipped.
    (ml-test/compare-searches "\\alpha then \\alpha" alpha 1 4 nil t)
    ;; Fontified comments and verbatim regions are skipped.
    (ml-test/compare-searches
     "x\\alpha then \\alpha" alpha 1 1 nil nil '(1 . 8))
    ;; Ordinary failure and invalid regexps preserve the original behavior.
    (ml-test/compare-searches "\\\\alpha" alpha 1 1)
    (ml-test/compare-searches "text" "[" 3 1)))

(ert-deftest ml-test/search-regexp-handles-backslash-parity ()
  (with-temp-buffer
    (let (one three)
      (dolist (count '(1 2 3 4))
        (let ((begin (point)))
          (insert (make-string count ?\\) "alpha ")
          (pcase count
            (1 (setq one begin))
            (3 (setq three (+ begin 2))))))
      (let ((ml/jit-point (point-min)))
        (goto-char (point-min))
        (should (ml/search-regexp "\\\\alpha\\>"))
        (should (= one (match-beginning 0)))
        (should (ml/search-regexp "\\\\alpha\\>"))
        (should (= three (match-beginning 0)))
        (should-error (ml/search-regexp "\\\\alpha\\>"))))))

(ert-deftest ml-test/search-regexp-noerror-reports-ordinary-failure ()
  (with-temp-buffer
    (insert "plain text")
    (goto-char (point-min))
    (let ((ml/jit-point (point-min)))
      (should-not (ml/search-regexp-noerror "\\\\alpha\\>")))))

(ert-deftest ml-test/search-regexp-handles-many-rejected-matches ()
  (with-temp-buffer
    (dotimes (_ 2000)
      (insert "\\\\alpha "))
    (let ((expected (point)))
      (insert "\\alpha")
      (goto-char (point-min))
      (let ((ml/jit-point (point-min)))
        (should (ml/search-regexp "\\\\alpha\\>"))
        (should (= expected (match-beginning 0)))))))

(ert-deftest ml-test/skip-blocks-preserves-match-data ()
  (with-temp-buffer
    (insert "{outer {inner} tail}")
    (goto-char (point-min))
    (set-match-data '(1 1))
    (should (ml/skip-blocks 0))
    (should (= (point) (point-max)))
    (should (equal '(1 1) (match-data t)))))

;;; magic-latex-buffer-test.el ends here
