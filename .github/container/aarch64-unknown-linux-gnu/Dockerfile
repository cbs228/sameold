#
# Debian base image, with arm64 toolchains
#

FROM ghcr.io/cbs228/sameold/builder/rust:latest

ARG SOURCE_DATE_EPOCH

RUN --mount=target=/var/lib/apt,type=cache,sharing=locked \
    --mount=target=/var/cache/apt,type=cache,sharing=locked \
    --mount=destination=/var/log,type=tmpfs \
    [ "$(dpkg --print-architecture)" != arm64 ] || exit 0; \
    set -eux; \
    apt-get update; \
    apt-get install -y \
        binutils-aarch64-linux-gnu \
        gcc-aarch64-linux-gnu \
        libc6-dev-arm64-cross \
        libc6:arm64 \
    ; \
    rm -f -- /etc/machine-id /var/cache/ldconfig/aux-cache

ARG RUST_TARGET="aarch64-unknown-linux-gnu"

RUN set -eux; \
    export CARGO_HOME=/usr/local/cargo; \
    rustup target add "$RUST_TARGET";

ENV CARGO_BUILD_TARGET="$RUST_TARGET" \
    CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_LINKER=aarch64-linux-gnu-gcc \
    CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_RUNNER=qemu-run-maybe

USER builder

LABEL org.opencontainers.image.description="A Debian-based Rust cross-compiling environment."
