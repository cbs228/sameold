# syntax = docker/dockerfile:1.2

###
### BUILD OPTIONS
###
###   See <https://hub.docker.com/_/rust> for current
###   list of tags. You may use vendored sources by
###   running
###
###     cargo vendor --versioned-dirs --locked >.cargo/config.toml
###
###   on the HOST. When building with vendored sources,
###   set --build-arg CARGO_NET_OFFLINE=true

# Rust version, like "1" or "1.67.0"
ARG BUILD_RUST_TAG=1.70

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

ENV CARGO_INSTALL_ROOT=/usr/local \
    CARGO_TERM_COLOR=always \
    RUST_BACKTRACE=1 \
    RUSTFLAGS='-C strip=symbols'

WORKDIR /build

# Capture build environment versions
RUN cat /etc/os-release && \
    cargo --version

# Modify image OS if required
RUN eval "$(cat </etc/os-release)" && \
    case "$ID" in \
      alpine) \
        # install static musl so we can statically link
        apk add --no-cache musl-dev ;; \
      debian) \
        # record glibc version for posterity
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

# Add vendored sources
COPY vendor vendor
COPY .cargo .cargo

# Target platform triple. Leave unset to autodetect.
ARG CARGO_BUILD_TARGET=

# Set to true if using vendored sources
ARG CARGO_NET_OFFLINE=false

# Fetch and build dependencies
RUN [ -n "$CARGO_BUILD_TARGET" ] || unset CARGO_BUILD_TARGET && \
    cargo build --tests --locked --release --workspace

###
### FIRST-PARTY BUILD IMAGE
###   Builds samedec itself
###

FROM samedec-build-deps AS samedec-build

ARG CARGO_BUILD_TARGET=

ENV CARGO_NET_OFFLINE=true

# Replace dummy crates with our sources
COPY crates crates

# Build first-party code. Update timestamps to force new
# build. samedec is installed to /usr/local/bin/samedec
RUN [ -n "$CARGO_BUILD_TARGET" ] || unset CARGO_BUILD_TARGET && \
    touch crates/sameold/src/lib.rs crates/samedec/src/main.rs && \
    cargo test --frozen --release --workspace && \
    cargo install --frozen --path=crates/samedec

# Perform integration tests
COPY sample sample

RUN samedec --version && \
    cd sample && \
    SAMEDEC=/usr/local/bin/samedec ./test.sh && \
    cd ..

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
