#
# Debian base image, with i386 toolchains
#

FROM ghcr.io/cbs228/sameold/builder/rust:latest

ARG SOURCE_DATE_EPOCH

RUN --mount=target=/var/lib/apt,type=cache,sharing=locked \
    --mount=target=/var/cache/apt,type=cache,sharing=locked \
    --mount=destination=/var/log,type=tmpfs \
    [ "$(dpkg --print-architecture)" != i386 ] || exit 0; \
    set -eux; \
    apt-get update; \
    apt-get install -y \
        binutils-i686-linux-gnu \
        gcc-i686-linux-gnu \
        libc6-dev-i386-cross \
        libc6:i386 \
    ; \
    rm -f -- /etc/machine-id /var/cache/ldconfig/aux-cache

ARG RUST_TARGET="i686-unknown-linux-gnu"

RUN set -eux; \
    export CARGO_HOME=/usr/local/cargo; \
    rustup target add "$RUST_TARGET";

ENV CARGO_BUILD_TARGET="$RUST_TARGET" \
    CARGO_TARGET_I686_UNKNOWN_LINUX_GNU_LINKER=i686-linux-gnu-gcc \
    CARGO_TARGET_I686_UNKNOWN_LINUX_GNU_RUNNER=qemu-run-maybe

USER builder

LABEL org.opencontainers.image.description="A Debian-based Rust cross-compiling environment."
