#!/bin/sh
set -eu

baseline_ref=${1:-master}
iterations=${MLB_BENCH_ITERATIONS:-20}
fixture=${MLB_BENCH_FIXTURE:-test/fixtures/large-document.tex}
temporary=$(mktemp -d "${TMPDIR:-/tmp}/magic-latex-benchmark.XXXXXX")
trap 'rm -rf "$temporary"' EXIT HUP INT TERM

git show "$baseline_ref:magic-latex-buffer.el" > "$temporary/magic-latex-buffer.el"

printf 'baseline_ref\t%s\n' "$baseline_ref"
MLB_BENCH_ITERATIONS=$iterations emacs -Q --batch \
  -L "$temporary" -l test/benchmark.el -- "$fixture"

printf '\ncandidate_ref\t%s\n' "$(git rev-parse --short HEAD)"
MLB_BENCH_ITERATIONS=$iterations emacs -Q --batch \
  -L . -l test/benchmark.el -- "$fixture"
