#!/usr/bin/env bash
#
# (re)build the CI container image

set -euo pipefail

now="$(date -u +'%Y-%m-%d')"

CONTAINER_PREFIX="ghcr.io/cbs228"
CONTAINER_FQNAME="${CONTAINER_PREFIX}/sameold/builder/%s"
CONTAINER_TAGS=("$now" "latest")

RUST_VERSIONS=("1.84.0")

usage() {
  cat <<EOF
Usage: $0 [--push]

Build container images for the CI environment
EOF
}

container_name() {
  # Usage: container_name SUFFIX

  #shellcheck disable=SC2059
  printf "$CONTAINER_FQNAME" "$1"
}

run() {
  # Echo and run
  echo >&2 "$@"
  "$@"
}

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

selfdir="$(dirname "${0?}")"

[ -z "${push_images:-}" ] || \
  podman login --authfile="${HOME}/.docker/config.json" "$CONTAINER_PREFIX"

# build the base image
base_tag="$(container_name base):latest"

run podman build \
  -f "${selfdir?}/Dockerfile.base" \
  --build-arg RUST_VERSIONS="${RUST_VERSIONS[*]}" \
  --tag "$base_tag" \
  "${selfdir?}"

# build architecture-specific images
for containerfile in "${selfdir}/"Dockerfile.rust.*; do
  platform_triple="${containerfile##*.}"
  cur_tag="$(container_name "$platform_triple"):${CONTAINER_TAGS[0]}"

  run podman build \
    --from "$base_tag" \
    -f "${containerfile}" \
    --tag "${cur_tag}" \
    "${selfdir?}"
done

# if all builds succeed, apply remaining tags and push
for containerfile in "${selfdir}/"Dockerfile.rust.*; do
  platform_triple="${containerfile##*.}"
  cur_base_name="$(container_name "$platform_triple")"

  run podman tag "${CONTAINER_TAGS[@]/#/"$cur_base_name:"}"

  if [ -n "${push_images:-}" ]; then
    for t in "${CONTAINER_TAGS[@]}"; do
      run podman push "$cur_base_name:$t"
    done
  fi
done
