#!/usr/bin/env bash
#
# Mimic sameold's container-based CI jobs locally, without github.
# Requires Linux and podman.

REGISTRY="ghcr.io/cbs228/sameold/builder/"

TARGETS=(
    x86_64-unknown-linux-gnu
    i686-unknown-linux-gnu
    armv7-unknown-linux-gnueabihf
    aarch64-unknown-linux-gnu
)

CONTAINER_TAG=latest

# podman volumes to be removed on exit
declare -a REMOVE_VOLUMES=()

container_runner() {
  # Usage: container_runner
  #
  # Automatically detect container platform command

  if [ -x "${BUILDER:-}" ]; then
    echo "${BUILDER}"
  elif [ -n "${BUILDER:-}" ]; then
    command -v "${BUILDER}"
  else
    {
      command -v podman || \
      command -v docker
    } 2>/dev/null || {
      echo >&2 "FATAL: container platform tools not found"
      return 1;
    }
  fi
}

is_any() {
  # Usage: is_any NEEDLE HAYSTACK0 HAYSTACK1 HAYSTACK2 ...
  #
  # Returns true if any of the haystacks match NEEDLE, exactly.

  local match="$1"
  shift
  local e
  for e; do
    [[ "$e" == "$match" ]] && return 0;
  done
  return 1
}

make_volume() {
  # Usage: make_temporary_volume NAME TEMPORARY
  #
  # Create a podman volume with NAME. If TEMPORARY is set, the
  # volume will be destroyed when this script exits

  local vol
  vol="${1?expected NAME}"

  local is_temp
  is_temp="${2:-}"

  if [ -n "${is_temp:-}" ]; then
    "$BUILDER" >/dev/null 2>&1 volume rm "$vol" || true
    REMOVE_VOLUMES+=("$vol")
  fi

  "$BUILDER" >/dev/null 2>&1 volume create --ignore "$vol"
}

run() {
  # Usage: run CMD ARG0 ARG1 ...
  #
  # Echo and run

  echo >&2 "$@"
  exec "$@"
}

run_default_image_rw() {
  # Usage: run_default_image_rw CMD ARG1 ARG2
  #
  # Run the given CMD in the native-architecture image, with
  # caches *read/write*. The image has the cargo registry cache
  # and the vendored sources volumes mounted read-write.

  run "$BUILDER" run --rm \
    --init \
    --security-opt=label:disable \
    --userns=keep-id:uid=1001,gid=1001 \
    --workdir /src \
    -v .:/src:ro \
    -v "sameold-target-$DEFAULT_TARGET":/src/target:rw \
    -v sameold-vendored-sources:/src/vendor:rw \
    -v sameold-cargo-cache:/cargo:rw \
    -v sameold-cargo-config:/src/.cargo:rw \
    "$DEFAULT_IMAGE" \
    "$@" &

  wait -f "$!"
}

run_default_image_ro() {
  # Usage: run_default_image_ro CMD ARG1 ARG2
  #
  # Run the given CMD in the native-architecture image, with
  # caches read-only. The image has the vendored sources volume
  # mounted read-only and the offline flag set.

  run "$BUILDER" run --rm \
    --init \
    --security-opt=label:disable \
    --userns=keep-id:uid=1001,gid=1001 \
    --workdir /src \
    -e CARGO_NET_OFFLINE=true \
    -e RUSTFLAGS='-C strip=symbols' \
    -v .:/src:ro \
    -v "sameold-target-$DEFAULT_TARGET":/src/target:rw \
    -v sameold-vendored-sources:/src/vendor:ro \
    -v sameold-cargo-config:/src/.cargo:ro \
    "$DEFAULT_IMAGE" \
    "$@" &

  wait -f "$!"
}

run_all_images() {
  # Usage: run_all_images CMD ARG1 ARG2
  #
  # Runs the given CMD on all images, in parallel with
  # GNU parallel. The image has the vendored sources volume
  # mounted read-only and the offline flag set.

  run parallel -i "$BUILDER" run --rm \
    --init \
    --security-opt=label:disable \
    --userns=keep-id:uid=1001,gid=1001 \
    --workdir /src \
    -e CARGO_NET_OFFLINE=true \
    -e RUSTFLAGS='-C strip=symbols' \
    -v .:/src:ro \
    -v 'sameold-target-{}:/src/target:rw' \
    -v sameold-vendored-sources:/src/vendor:ro \
    -v sameold-cargo-config:/src/.cargo:ro \
    -v sameold-artifacts:/artifacts:rw \
    "${REGISTRY}"'{}'":$CONTAINER_TAG" \
    "$@" \
    -- \
    "${TARGETS[@]}" &

  wait -f "$!" || return 1

  echo >&2
  echo >&2 "All targets passed:"
  printf >&2 '  ✔ %s\n'  "${TARGETS[@]}"
}

copy_artifacts_to() {
  # Usage: copy_artifacts_to DIR
  #
  # Copy all artifacts from the build to the given DIRectory
  # on the host. The output directory will be created if it
  # does not exist. Existing files in DIR will be overwritten.

  local dir="${1?missing DIR}"
  mkdir -p -- "$dir" || return 1
  dir="$(realpath -e "$dir")" || return 1

  run "$BUILDER" run --rm \
    --init \
    --security-opt=label:disable \
    --userns=keep-id \
    --user=root \
    -v sameold-artifacts:/artifacts:ro \
    -v "$dir":/out:rw \
    "$DEFAULT_IMAGE" \
    bash -c 'umask 002 && cp -f -R --archive /artifacts/* /out/' &

  wait -f "$!"
}

cleanup_exit() {
  # Actions executed when the script exits, either error or not
  trap '' ERR EXIT

  # Kill all background jobs
  #shellcheck disable=SC2046
  kill $(jobs -p) 2>/dev/null || true
  wait

  # remove volumes
  local vol
  for vol in "${REMOVE_VOLUMES[@]}"; do
    [ -n "${vol:-}" ] || continue
    "$BUILDER" >/dev/null 2>&1 volume rm "$vol" || true
  done
  REMOVE_VOLUMES=()

  echo >&2 "Cleanup finished."
}

BUILDER="$(container_runner)"
DEFAULT_TARGET="$(uname -m)-unknown-linux-gnu"

# compute full image names
DEFAULT_IMAGE="${REGISTRY}${DEFAULT_TARGET}:${CONTAINER_TAG}"

# git version → source date epoch, if possible
SOURCE_DATE_EPOCH="${SOURCE_DATE_EPOCH:-$(git log -1 --pretty=%ct || date +%s)}"

# git version
SOURCE_REV="$(git log -1 --pretty=%h --abbrev=13 || echo nogit)"

# where to store artifacts
ARTIFACTS="${ARTIFACTS:-"./.artifacts"}"

# return if sourced
(return 0 2>/dev/null) && return 0

set -euo pipefail

if ! is_any "$DEFAULT_TARGET" "${TARGETS[@]}"; then
  echo >&2 "error: expected image \"$DEFAULT_TARGET\" for native architecture,"
  echo >&2 "but none exists."
  exit 1
fi

# install cleanup hooks
trap cleanup_exit ERR EXIT

# 0. CHECK FOR WHITESPACE ERRORS
git diff --check develop..

# make volume for cargo registry (cached across runs)
make_volume sameold-cargo-cache

# make volume for vendored sources (cached across runs)
make_volume sameold-vendored-sources

# make volume for cargo config (destroyed on exit)
make_volume sameold-cargo-config temporary

# make volumes for built binaries (destroyed on exit, shared)
for target in "${TARGETS[@]}"; do
  make_volume sameold-target-"$target" temporary
done

# make volume for artifacts
make_volume sameold-artifacts temporary

# 1. VENDOR SOURCES
#
# Vendor sources, updating our cargo cache of crates.io
# while we do it. Also audit all dependencies.

{ cargo_vendor=$(cat) ; } <<'SCRIPT'
cargo vendor --versioned-dirs --locked >.cargo/config.toml
SCRIPT

run_default_image_rw bash -c "$cargo_vendor"


# 2. RUN LINTS
#
# Check that the code is cargo-fmt compliant, lints OK, and
# builds documentation without warnings
{ cargo_check=$(cat) ; } <<'SCRIPT'
cargo fmt --check &&
cargo check --frozen --locked --workspace &&
cargo doc --frozen --locked --no-deps --workspace
SCRIPT

run_default_image_ro bash -c "$cargo_check"


# 3. RUN ALL BUILDS on all architectures
#
# Test in debug-mode
# Test in release-mode
# Build and install in release-mode
# Run integration tests
# Copy to samedec-artifacts volume
{ cargo_build=$(cat) ; } <<'SCRIPT'
cargo test --offline --frozen --workspace &&
cargo test --offline --frozen --release --workspace &&
cargo install --offline --frozen --path=crates/samedec &&
qemu-run-maybe samedec --version &&
./sample/test.sh qemu-run-maybe samedec &&
cp --archive "$(command -v samedec)" "/artifacts/samedec-$CARGO_BUILD_TARGET" &&
echo >&2 "✔ OK $CARGO_BUILD_TARGET"
SCRIPT

run_all_images bash -c "$cargo_build"

# 4. COPY ARTIFACTS
copy_artifacts_to "$ARTIFACTS/$SOURCE_REV"
(
  cd "$ARTIFACTS" || exit 1
  ln -srfT "$SOURCE_REV" "latest"
)
echo "$ARTIFACTS/latest"
