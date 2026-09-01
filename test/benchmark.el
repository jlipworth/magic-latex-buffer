;;; benchmark.el --- Magic LaTeX viewport benchmark -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Time repeatable viewport-sized JIT work against a supplied LaTeX file:
;;
;;   MLB_BENCH_ITERATIONS=10 emacs -Q --batch -L . \
;;     -l test/benchmark.el -- test/fixtures/large-document.tex

;;; Code:

(require 'cl-lib)
(require 'magic-latex-buffer)

(defconst ml-bench/positions '(0.02 0.25 0.50 0.75 0.95))

(defun ml-bench/env-number (name default)
  "Return numeric environment variable NAME, or DEFAULT."
  (if-let ((value (getenv name)))
      (string-to-number value)
    default))

(defun ml-bench/region-at (fraction lines)
  "Return a LINES-line region near buffer FRACTION."
  (goto-char (+ (point-min)
                (floor (* fraction (- (point-max) (point-min))))))
  (let ((beg (line-beginning-position)))
    (forward-line lines)
    (cons beg (point))))

(defun ml-bench/clear-overlays ()
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

(defun ml-bench/overlay-count ()
  "Return the number of Magic LaTeX command and pretty overlays."
  (cl-count-if
   (lambda (overlay)
     (memq (overlay-get overlay 'category)
           '(ml/ov-pretty ml/ov-block ml/ov-align)))
   (overlays-in (point-min) (point-max))))

(defun ml-bench/run-with (beg end symbols suscript blocks)
  "Run Magic LaTeX from BEG to END with selected display features."
  (let ((magic-latex-enable-pretty-symbols symbols)
        (magic-latex-enable-suscript suscript)
        (magic-latex-enable-block-highlight blocks)
        (magic-latex-enable-block-align nil))
    (goto-char beg)
    (ml/jit-block-highlighter beg end)
    (goto-char beg)
    (ml/jit-prettifier beg end)))

(defconst ml-bench/scenarios
  `((full . ,(lambda (beg end) (ml-bench/run-with beg end t t t)))
    (symbols . ,(lambda (beg end) (ml-bench/run-with beg end t nil nil)))
    (no-symbols . ,(lambda (beg end) (ml-bench/run-with beg end nil t t)))
    (blocks . ,(lambda (beg end) (ml-bench/run-with beg end nil nil t)))))

(defun ml-bench/percentile (sorted fraction)
  "Return FRACTION percentile from SORTED numeric values."
  (nth (min (1- (length sorted))
            (floor (* fraction (length sorted))))
       sorted))

(defun ml-bench/measure (function regions iterations)
  "Measure FUNCTION over REGIONS for ITERATIONS traversals."
  (let (elapsed overlays)
    (dotimes (_ iterations)
      (dolist (region regions)
        (ml-bench/clear-overlays)
        (garbage-collect)
        (goto-char (car region))
        (let ((started (current-time)))
          (funcall function (car region) (cdr region))
          (push (float-time (time-subtract (current-time) started)) elapsed))
        (push (ml-bench/overlay-count) overlays)))
    (setq elapsed (sort elapsed #'<))
    (list (/ (apply #'+ elapsed) (length elapsed))
          (ml-bench/percentile elapsed 0.50)
          (ml-bench/percentile elapsed 0.95)
          (car (last elapsed))
          (/ (float (apply #'+ overlays)) (length overlays)))))

(defun ml-bench/main ()
  "Run the Magic LaTeX viewport benchmark."
  (when (equal (car command-line-args-left) "--")
    (pop command-line-args-left))
  (let ((file (or (pop command-line-args-left)
                  "test/fixtures/large-document.tex"))
        (iterations (ml-bench/env-number "MLB_BENCH_ITERATIONS" 5))
        (lines (ml-bench/env-number "MLB_BENCH_VIEWPORT_LINES" 80)))
    (with-temp-buffer
      (insert-file-contents file)
      (setq buffer-file-name (expand-file-name file))
      (latex-mode)
      (font-lock-mode 1)
      (magic-latex-buffer 1)
      (font-lock-ensure)
      (ml-bench/clear-overlays)
      (let ((ml/jit-point (point-max))
            (regions
             (mapcar
              (lambda (fraction)
                (save-excursion (ml-bench/region-at fraction lines)))
              ml-bench/positions)))
        (princ "scenario\tsamples\tmean_ms\tmedian_ms\tp95_ms\tmax_ms\tmean_overlays\n")
        (dolist (scenario ml-bench/scenarios)
          (pcase-let ((`(,mean ,median ,p95 ,maximum ,overlays)
                       (ml-bench/measure
                        (cdr scenario) regions iterations)))
            (princ
             (format "%s\t%d\t%.3f\t%.3f\t%.3f\t%.3f\t%.1f\n"
                     (car scenario) (* iterations (length regions))
                     (* 1000 mean) (* 1000 median) (* 1000 p95)
                     (* 1000 maximum) overlays))))))))

(ml-bench/main)

;;; benchmark.el ends here
