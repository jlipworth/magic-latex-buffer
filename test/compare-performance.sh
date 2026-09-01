#!/bin/sh
set -eu

baseline_ref=${1:-master}
candidate_ref=${2:-HEAD}
iterations=${MLB_BENCH_ITERATIONS:-20}
fixture=${MLB_BENCH_FIXTURE:-test/fixtures/large-document.tex}
compiled=${MLB_BENCH_COMPILED:-1}
temporary=$(mktemp -d "${TMPDIR:-/tmp}/magic-latex-benchmark.XXXXXX")
trap 'rm -rf "$temporary"' EXIT HUP INT TERM

prepare_revision() {
  ref=$1
  directory=$2
  mkdir -p "$directory"
  git show "$ref:magic-latex-buffer.el" > "$directory/magic-latex-buffer.el"
  if [ "$compiled" -eq 1 ]; then
    emacs -Q --batch -L "$directory" \
      -f batch-byte-compile "$directory/magic-latex-buffer.el" >/dev/null
  fi
}

run_revision() {
  ref=$1
  directory=$2
  printf '%s\t%s\n' "$3" "$ref"
  MLB_BENCH_ITERATIONS=$iterations emacs -Q --batch \
    -L "$directory" -L . -l test/benchmark.el -- "$fixture"
}

prepare_revision "$baseline_ref" "$temporary/baseline"
prepare_revision "$candidate_ref" "$temporary/candidate"

printf 'compiled\t%s\n' "$compiled"
run_revision "$baseline_ref" "$temporary/baseline" baseline_ref
printf '\n'
run_revision "$candidate_ref" "$temporary/candidate" candidate_ref
