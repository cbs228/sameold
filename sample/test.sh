#!/usr/bin/env bash
#
# Runs integration tests for samedec.
#
#     cd sample/
#     ./test.sh
#
# By default, this will build and run a debug-mode samedec
# with cargo. You may also set SAMEDEC to the path to an
# existing executable.
#
# The integration tests ensure that samedec's child process
# spawning and environment variable assignment works.

set -euo pipefail

if [ -n "${SAMEDEC:-}" ]; then
  RUNARGS=("${SAMEDEC}")
else
  RUNARGS=('cargo' 'run' '-q' '-p' 'samedec' '--')
fi

run_samedec() {
  # Usage: run_samedec FILE
  # Runs `samedec` on given input file stem

  infile="$1"

  "${RUNARGS[@]}" \
    --rate 22050 \
    --file "${infile}.bin" \
    -- \
    bash \
    "${infile}.bash" | tee /dev/stderr
}

for file in $(basename -s .bin *.s16le.bin); do
  [ -e "${file}.bin" ] || exit 1

  printf '[%s]\n' "$file"

  cmp <(run_samedec "$file") "$file.txt" || {
      printf '[%s]: FAIL\n' "$file"
      exit 1
  };

  printf '[%s]: PASS\n' "$file"
done
