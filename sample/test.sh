#!/usr/bin/env sh
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

set -eu

if [ -z "${SAMEDEC:-}" ]; then
  SAMEDEC="cargo"
  ARGS="run -q -p samedec --"
else
  ARGS=""
fi

run_samedec() {
  # Usage: run_samedec FILE
  # Runs `samedec` on given input file stem

  infile="$1"

  #shellcheck disable=SC2086
  "$SAMEDEC" $ARGS \
    --rate 22050 \
    --file "${infile}.bin" \
    -- \
    sh \
    "${infile}.sh"
}

for file in $(basename -s .bin -- *.s16le.bin); do
  [ -e "${file}.bin" ] || exit 1

  printf '[%s]\n' "$file"

  output="$(run_samedec "$file")"
  expect="$(cat "${file}.txt")"

  echo "$output"

  if [ "$output" = "$expect" ]; then
    printf '[%s]: PASS\n' "$file"
  else
    printf '[%s]: FAIL\n' "$file"
    exit 1
  fi
done
