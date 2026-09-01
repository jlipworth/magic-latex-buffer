EMACS ?= emacs
FIXTURE := test/fixtures/large-document.tex

.PHONY: test benchmark fixture

test:
	$(EMACS) -Q --batch -L . -l ert \
		-l test/magic-latex-buffer-test.el \
		-f ert-run-tests-batch-and-exit

benchmark:
	MLB_BENCH_ITERATIONS=10 $(EMACS) -Q --batch -L . \
		-l test/benchmark.el -- $(FIXTURE)

fixture:
	$(EMACS) -Q --batch -l test/generate-large-fixture.el -- $(FIXTURE) 128
