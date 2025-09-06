#
# Debian base image, with armhf toolchains
#

FROM ghcr.io/cbs228/sameold/builder/rust:latest

ARG SOURCE_DATE_EPOCH

RUN --mount=target=/var/lib/apt,type=cache,sharing=locked \
    --mount=target=/var/cache/apt,type=cache,sharing=locked \
    --mount=destination=/var/log,type=tmpfs \
    [ "$(dpkg --print-architecture)" != armhf ] || exit 0; \
    set -eux; \
    apt-get update; \
    apt-get install -y \
        binutils-arm-linux-gnueabi \
        gcc-arm-linux-gnueabihf \
        libc6-dev-armhf-cross \
        libc6:armhf \
    ; \
    rm -f -- /etc/machine-id /var/cache/ldconfig/aux-cache

ARG RUST_TARGET="armv7-unknown-linux-gnueabihf"

RUN set -eux; \
    export CARGO_HOME=/usr/local/cargo; \
    rustup target add "$RUST_TARGET";

ENV CARGO_BUILD_TARGET="$RUST_TARGET" \
    CARGO_TARGET_ARMV7_UNKNOWN_LINUX_GNUEABIHF_LINKER=arm-linux-gnueabihf-gcc \
    CARGO_TARGET_ARMV7_UNKNOWN_LINUX_GNUEABIHF_RUNNER=qemu-run-maybe

USER builder

ARG SOURCE_REV=nogit

LABEL org.opencontainers.image.description="A Debian-based Rust cross-compiling environment."
LABEL org.opencontainers.image.revision="$SOURCE_REV"
