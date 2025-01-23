#
# Debian base image
#

# or buster-20240612-slim for a snapshot-capable, reproducible image
ARG DEBIAN_TAG=buster-slim

FROM docker.io/library/debian:${DEBIAN_TAG}

ARG DEBIAN_TAG
ARG SOURCE_DATE_EPOCH

# Create a non-root user for running builds
# GHA wants uid 1001
ARG UID=1001

RUN --mount=target=/var/lib/apt,type=cache,sharing=locked \
    --mount=target=/var/cache/apt,type=cache,sharing=locked \
    --mount=destination=/var/log,type=tmpfs \
    set -eux; \
    # create an unprivileged user for builds
    groupadd builder -g "$UID"; \
    useradd builder -u "$UID" -g "$UID" --create-home; \
    # build directories for any user
    mkdir -m 1777 /src /install /cargo; \
    # re-enable apt caching (we have a cache mount)
    rm -f /etc/apt/apt.conf.d/docker-clean; \
    # if tag contains numerics, like buster-20240612-slim, use the
    # snapshot URL that's baked into the image
    if echo "${DEBIAN_TAG}" | grep -q "[0-9]"; then \
        sed -i -r \
                -e 's/^deb/# deb/' \
                -e 's|^#\s*(.*http://snapshot\.)|\1|' \
            /etc/apt/sources.list; \
        cat >&2 /etc/apt/sources.list; \
        echo 'Acquire::Check-Valid-Until "false";' > /etc/apt/apt.conf.d/use-snapshot.conf; \
        echo 'Acquire::Retries "10";' >> /etc/apt/apt.conf.d/use-snapshot.conf; \
        echo 'Acquire::Retries::Delay::Maximum "600";' >> /etc/apt/apt.conf.d/use-snapshot.conf; \
    fi; \
    # enable packages from multiple architectures
    dpkg --add-architecture amd64; \
    dpkg --add-architecture armhf; \
    dpkg --add-architecture arm64; \
    dpkg --add-architecture i386; \
    # install native C compilers, CI utilities, and qemu
    apt-get update; \
    apt-get install -y --no-install-recommends \
        build-essential \
        ca-certificates \
        curl \
        gcc \
        git \
        libc6-dev \
        qemu-user \
        qemu-user-binfmt \
        tar \
        zstd; \
    git config --system --add safe.directory '*'; \
    rm -f -- /etc/machine-id /var/cache/ldconfig/aux-cache

# Directories for sources and installed binaries
VOLUME ["/src", "/install"]

LABEL org.opencontainers.image.source="https://github.com/cbs228/sameold"
LABEL org.opencontainers.image.description="A minimal debian cross-compiling environment with good glibc compatibility."

WORKDIR "/src"
