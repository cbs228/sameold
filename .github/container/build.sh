#!/usr/bin/env bash
#
# (re)build the CI container image
#
# run with --push to push the images.

now="$(date -u +'%Y-%m-%d')"

CONTAINER_PREFIX="ghcr.io/cbs228"
CONTAINER_FQNAME="${CONTAINER_PREFIX}/sameold/builder/%s"
CONTAINER_TAGS=("$now" "latest")

RUST_VERSIONS=("1.84.0")

usage() {
  cat <<EOF
Usage: $0 [--push]

Build container images for the CI environment. To select
a particular container platform tool like podman, set

    BUILDER=podman

Prior to pushing, make sure to

    podman login "$CONTAINER_PREFIX"

or equivalent.
EOF
}

run() {
  # Echo and run
  echo >&2 "$@"
  "$@"
}

container_builder() {
  # Usage: automatically detect container platform command

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

container_name() {
  # Usage: container_name SUFFIX

  #shellcheck disable=SC2059
  printf "$CONTAINER_FQNAME" "$1"
}

tagall() {
  # Usage: tagall SHORTNAME TAG0 TAG1 ...
  #
  # Apply all tags to the given image, which must already be tagged as
  # "current/container/prefix/$SHORTNAME:$TAG0"

  local prefix
  prefix="$(container_name "${1?}")"
  shift

  run "$BUILDER" tag "${@/#/"$prefix:"}"
}

pushall() {
  # Usage: pushall SHORTNAME TAG0 TAG1 ...
  #
  # Push the given image "current/container/prefix/$SHORTNAME" and
  # all the tags specified on the command line. The pushes are not
  # atomic and will happen sequentially.

  local prefix
  prefix="$(container_name "${1?}")"
  shift

  local tag
  for tag in "$@"; do
    run "$BUILDER" push "${prefix}:${tag}"
  done
}

# return if sourced
(return 0 2>/dev/null) && return 0

set -euo pipefail

if ! options="$(getopt -o 'hp' --long help,push -- "$@")"; then
  usage >&2
  exit 1
fi

eval set -- "${options:-}"
push_images=''
while true; do
  case "${1:-}" in
    (-h | --help)
      usage
      exit 0 ;;
    (-p | --push)
      push_images=y ;;
    ('') ;;
  esac
  shift || break
done

BUILDER="$(container_builder)"
selfdir="$(dirname "${0?}")"

# build the base image
base_tag="$(container_name base):${CONTAINER_TAGS[0]}"

run "$BUILDER" build \
  -f "${selfdir?}/Dockerfile.base" \
  --tag "$base_tag" \
  "${selfdir?}"

# add rust to the base image
rust_tag="$(container_name rust):${CONTAINER_TAGS[0]}"

run "$BUILDER" build \
  -f "${selfdir?}/Dockerfile.rust" \
  --from "$base_tag" \
  --build-arg RUST_VERSIONS="${RUST_VERSIONS[*]}" \
  --tag "$rust_tag" \
  "${selfdir?}"

# build architecture-specific images
for containerfile in "${selfdir}/"Dockerfile.rust.*; do
  platform_triple="${containerfile##*.}"
  cur_tag="$(container_name "$platform_triple"):${CONTAINER_TAGS[0]}"

  run "$BUILDER" build \
    --from "$rust_tag" \
    -f "${containerfile}" \
    --tag "${cur_tag}" \
    "${selfdir?}"
done

# if all builds succeed, apply remaining tags...
tagall base "${CONTAINER_TAGS[@]}"
tagall rust "${CONTAINER_TAGS[@]}"
for containerfile in "${selfdir}/"Dockerfile.rust.*; do
  platform_triple="${containerfile##*.}"

  tagall "$platform_triple" "${CONTAINER_TAGS[@]}"
done

[ -n "${push_images:-}" ] || exit 0

# ... and push
pushall base "${CONTAINER_TAGS[@]}"
pushall rust "${CONTAINER_TAGS[@]}"
for containerfile in "${selfdir}/"Dockerfile.rust.*; do
  platform_triple="${containerfile##*.}"

  pushall "$platform_triple" "${CONTAINER_TAGS[@]}"
done
