#!/usr/bin/env sh
#
# Runs integration tests for samedec.
#
#     cd sample/
#     ./test.sh
#
# By default, this will build and run a debug-mode samedec
# with cargo. If you want to run an installed samedec binary,
# instead invoke it as:
#
#     ./test.sh path/to/samedec
#
# The integration tests ensure that samedec's child process
# spawning and environment variable assignment works.

set -eu

run_samedec() {
  # Usage: run_samedec FILE ARGS
  # Runs `samedec` on given input file stem

  infile="$1"
  shift

  "$@" \
    --rate 22050 \
    --file "${infile}.bin" \
    -- \
    sh \
    "${infile}.sh"
}

if [ "$#" -lt 1 ]; then
  exec "$0" cargo run -q -p samedec --
fi

exe="${0?no executable path}"
cd "$(dirname "$exe")"

for file in $(basename -s .bin -- *.s16le.bin); do
  [ -e "${file}.bin" ] || exit 1

  printf '[%s]\n' "$file"

  output="$(run_samedec "$file" "$@")"
  expect="$(cat "${file}.txt")"

  echo "$output"

  if [ "$output" = "$expect" ]; then
    printf '[%s]: PASS\n' "$file"
  else
    printf '[%s]: FAIL\n' "$file"
    exit 1
  fi
done
