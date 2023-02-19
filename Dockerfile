# syntax = docker/dockerfile:1.2

###
### BUILD OPTIONS
###
###   See <https://hub.docker.com/_/rust> for current
###   list of tags.

# Rust version, like "1" or "1.67.0"
ARG BUILD_RUST_TAG=1.67

# Build operating system
# Use slim-buster for glibc or alpine for musl
ARG BUILD_OS_TAG=slim-buster

###
### THIRD-PARTY BUILD IMAGE
###   The base image includes all compilers and build tools.
###   We track an old Debian release to maintain compatibility
###   with el8's glibc.
###
###   This image fetches and builds all of samedec's dependencies.
###

FROM docker.io/library/rust:${BUILD_RUST_TAG}-${BUILD_OS_TAG} AS samedec-build-deps

ARG CARGO_BUILD_TARGET=

# Fetch with CLI for Github Runners, see
# <https://github.com/docker/build-push-action/issues/621>
ENV CARGO_NET_GIT_FETCH_WITH_CLI="true" \
    CARGO_INSTALL_ROOT=/usr/local \
    CARGO_TERM_COLOR=always \
    RUST_BACKTRACE=1 \
    RUSTFLAGS='-C strip=symbols'

WORKDIR /build

# Capture build environment versions
RUN cat /etc/os-release && \
    cargo --version

# Modify image OS if required. Install git
# for CARGO_NET_GIT_FETCH_WITH_CLI
RUN eval "$(cat </etc/os-release)" && \
    case "$ID" in \
      alpine) \
        # install static musl so we can statically link
        apk add --no-cache musl-dev git ;; \
      debian) \
        # record glibc version for posterity
        apt-get update && \
        apt-get install -y git && \
        ldd --version ldd ;; \
    esac

# Create dummy versions of our crates
# We will only use these for building dependencies
RUN cargo new --vcs none --lib crates/sameold && \
    cargo new --vcs none --bin crates/samedec

# Capture dependencies
COPY Cargo.lock \
     Cargo.toml \
     .

COPY crates/sameold/Cargo.toml crates/sameold/Cargo.toml
COPY crates/samedec/Cargo.toml crates/samedec/Cargo.toml

# Download dependencies
RUN [ -n "$CARGO_BUILD_TARGET" ] || unset CARGO_BUILD_TARGET && \
    cargo fetch --locked

# Build dependencies
RUN [ -n "$CARGO_BUILD_TARGET" ] || unset CARGO_BUILD_TARGET && \
    cargo build --offline --tests --frozen --release --workspace

###
### FIRST-PARTY BUILD IMAGE
###   Build's samedec itself
###

FROM samedec-build-deps AS samedec-build

ARG CARGO_BUILD_TARGET=

# Replace dummy crates with our sources
COPY crates crates

# Build first-party code. Update timestamps to force new
# build. samedec is installed to /usr/local/bin/samedec
RUN [ -n "$CARGO_BUILD_TARGET" ] || unset CARGO_BUILD_TARGET && \
    touch crates/sameold/src/lib.rs crates/samedec/src/main.rs && \
    cargo test --offline --frozen --release --workspace && \
    cargo install --offline --frozen --path=crates/samedec

# Perform tests
RUN samedec --version

COPY sample/long_message.22050.s16le.* .

RUN EXPECT="$(cat <long_message.22050.s16le.txt)" && \
    OUT="$(samedec -r 22050 <long_message.22050.s16le.bin)" && \
    echo "$OUT" && \
    if [ "$OUT" = "$EXPECT" ]; then \
      echo "PASS"; \
    else \
      echo "FAIL!"; \
      exit 1; \
    fi

###
### NON-IMAGE
###  Use this build stage as your target to output a local file.
###  Build with `--target localfile --output $outdir`.
###

FROM scratch AS localfile

WORKDIR /

COPY --from=samedec-build /usr/local/bin/samedec .

###
### RUNTIME IMAGE
### Use slimmed-down Buster
###

FROM docker.io/library/debian:buster-slim AS samedec

LABEL name="samedec-buster-slim" \
      org.opencontainers.image.title="samedec" \
      org.opencontainers.image.authors="Colin S." \
      org.opencontainers.image.source="https://github.com/cbs228/sameold" \
      org.opencontainers.image.documentation="https://crates.io/crates/samedec"

COPY --from=samedec-build /usr/local/bin/samedec /usr/local/bin/
COPY ./docker-entrypoint.sh /

RUN samedec --version

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["--help"]
