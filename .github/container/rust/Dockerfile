#
# Debian base image, with platform-native Rust
#

FROM ghcr.io/cbs228/sameold/builder/base:latest

ARG SOURCE_DATE_EPOCH

# Install rustup and platform-native target
#
#    RUST_VERSIONS: list of version numbers or strings
#                   like `stable`. The first entry becomes the
#                   default toolchain version.
#
#      RUSTUP_ARCH: native architecture as llvm platform triple
#
#   RUSTUP_VERSION: version of rustup to download
#
#    RUSTUP_SHA256: checksum for this version of rustup on
#                   ${RUST_ARCH}.
#
# Default values are provided only for x86_64.
ARG RUST_VERSIONS="stable" \
    RUSTUP_ARCH="x86_64-unknown-linux-gnu" \
    RUSTUP_VERSION="1.27.1" \
    RUSTUP_SHA256="6aeece6993e902708983b209d04c0d1dbb14ebb405ddb87def578d41f920f56d" \
    RUSTUP_URL=""

# Basic cargo/rust configuration, including a `cargo install`
# location of `/install`.
ENV PATH=/install/bin:/usr/local/cargo/bin:$PATH \
    CARGO_INSTALL_ROOT=/install \
    CARGO_HOME=/cargo \
    CARGO_TERM_COLOR=always \
    RUST_BACKTRACE=1 \
    RUSTUP_HOME=/usr/local/rustup

# Instructions adapted from official Docker image
# <https://github.com/rust-lang/docker-rust/blob/master/Dockerfile-slim.template>
RUN set -eux; \
    export CARGO_HOME=/usr/local/cargo; \
    [ -n "${RUSTUP_URL:-}" ] || RUSTUP_URL="https://static.rust-lang.org/rustup/archive/${RUSTUP_VERSION}/${RUSTUP_ARCH}/rustup-init"; \
    curl -O -sSf "$RUSTUP_URL"; \
    echo "${RUSTUP_SHA256} *rustup-init" | sha256sum -c -; \
    chmod +x rustup-init; \
    default_rust="$(echo "$RUST_VERSIONS" | cut -f1 -d' ')"; \
    ./rustup-init -y --no-modify-path --profile minimal --default-toolchain "$default_rust"; \
    rm rustup-init; \
    rustup --version; \
    cargo --version; \
    rustc --version; \
    rustup set auto-self-update disable; \
    echo "$RUST_VERSIONS" | xargs rustup toolchain add --profile minimal; \
    rustup component add --toolchain "$default_rust" rustfmt; \
    (umask 022 && echo "$RUSTUP_ARCH" >/etc/rust-native-arch)

# Install scripts
COPY rootfiles /

LABEL org.opencontainers.image.description="A pinned Rust toolchain with rustup."
