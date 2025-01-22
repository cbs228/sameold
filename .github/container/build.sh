#!/usr/bin/env bash
#
# (re)build the CI container image
#
# run with --push to push the images.

# set to disable cache
NO_CACHE="${NO_CACHE:-}"

DEBIAN_TAG="buster-20240612-slim"
CONTAINER_PREFIX="ghcr.io/cbs228"
CONTAINER_FQNAME="${CONTAINER_PREFIX}/sameold/builder/%s"

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

buildcontainer() {
  # Usage: buildcontainer ARGS
  #
  # Run the $BUILDER to build a container image. Some standardized
  # arguments are passed to every build.

  if [[ $BUILDER =~ podman$ ]]; then
    run "$BUILDER" build \
      --build-arg SOURCE_DATE_EPOCH="$SOURCE_DATE_EPOCH" \
      --timestamp "$SOURCE_DATE_EPOCH" \
      ${NO_CACHE:+--no-cache} \
      "$@"
  else
    # docker mode; 100% untested
    run "$BUILDER" buildx build \
      --build-arg SOURCE_DATE_EPOCH="$SOURCE_DATE_EPOCH" \
			--output type=docker,rewrite-timestamp=true \
      ${NO_CACHE:+--no-cache} \
      "$@"
  fi
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
selfdir="$(dirname "$(realpath -e "${0?}")")"

# set SOURCE_DATE_EPOCH if possible
SOURCE_DATE_EPOCH="${SOURCE_DATE_EPOCH:-$(git log -1 --pretty=%ct -- "$selfdir" || date +%s)}"
export SOURCE_DATE_EPOCH

# tag with "latest" and SOURCE_DATE_EPOCH
CONTAINER_TAGS=("latest" "$(date --date '@'"$SOURCE_DATE_EPOCH" +'%Y-%m-%d')")

# build the base image
base_tag="$(container_name base):${CONTAINER_TAGS[0]}"

buildcontainer \
  --build-arg DEBIAN_TAG="$DEBIAN_TAG" \
  --tag "$base_tag" \
  "${selfdir?}/base"

# add rust to the base image
rust_tag="$(container_name rust):${CONTAINER_TAGS[0]}"

buildcontainer \
  --from "$base_tag" \
  --build-arg RUST_VERSIONS="${RUST_VERSIONS[*]}" \
  --tag "$rust_tag" \
  "${selfdir?}/rust"

# build architecture-specific images
for containerdir in "${selfdir?}/"*-*-*; do
  [ -d "$containerdir" ] || continue

  platform_triple="$(basename ${containerdir})"
  cur_tag="$(container_name "$platform_triple"):${CONTAINER_TAGS[0]}"

  buildcontainer \
    --from "$rust_tag" \
    --tag "${cur_tag}" \
    "${containerdir}"
done

# if all builds succeed, apply remaining tags...
tagall base "${CONTAINER_TAGS[@]}"
tagall rust "${CONTAINER_TAGS[@]}"
for containerdir in "${selfdir?}/"*-*-*; do
  [ -d "$containerdir" ] || continue

  tagall "$(basename "$containerdir")" "${CONTAINER_TAGS[@]}"
done

[ -n "${push_images:-}" ] || exit 0

# ... and push
for containerdir in "${selfdir?}/"*; do
  [ -d "$containerdir" ] || continue

  pushall "$(basename "$containerdir")" "${CONTAINER_TAGS[@]}"
done
