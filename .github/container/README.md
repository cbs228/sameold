# Rust Cross-Compiling Containers

[cross-rs](https://github.com/cross-rs/cross) splits the build environment into:

1. An "outer" build environment, which contains `cargo` and a toolchain.

2. An "inner" build environment, which includes a gcc cross-compiler, foreign-architecture runtime libraries, and qemu.

If the outer build environment is containerized, this introduces problems. The inner build environment (2) needs access to the host's Docker or podman socket. This can be done, but it does not mesh well with Github Actions' `jobs:*:container` option.

Further, with containers, we want all parts of the build environment:

* Included and ready-to-use
* Fully-defined by the `Containerfile` and the image's `sha256:` hash

Depending on an external container at runtime defeats some of these design goals.

Instead of using cross-rs, we create our own environment that includes all of these tools. See

```
./build.sh
```

to build it. The build instructions are heavily influenced by the cross-rs project but do not depend on it.

You can use any of these containers offline to build samedec. Define the following volumes:

* `/src`: the repository root of a Rust project
* `/install`: destination for binaries installed with `cargo install`
* `/src/target` (**optional**): a build directory.
* `/cargohome` (**optional**): a persistent `$CARGO_HOME` directory

Example:

```bash
mkdir -p out/aarch64-unknown-linux-gnu

podman run \
  --security-opt label=disable \
  --userns=keep-id:uid=1001,gid=1001 \
  --rm -it \
  --volume .:/src:ro \
  --volume ./target:/src/target:rw \
  --volume ./out/aarch64-unknown-linux-gnu:/install \
  ghcr.io/cbs228/sameold/builder/aarch64-unknown-linux-gnu \
  cargo install --path crates/samedec
```

You should run your build as UID 1001 within the container. The above `--userns` mapping will accomplish this.
