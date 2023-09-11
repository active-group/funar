# Pin debian:unstable-slim
FROM gitpod/workspace-base@sha256:4b44d2c165e07398d4718d4cf801865c6874dbcf1428634b07a8e668357d48d9

USER root

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates sudo curl xz-utils

# Flakes einschalten (bevor User gewechselt wird)
RUN mkdir -p /etc/nix && \
    echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf

# Necessary for ARM64 for some reason
RUN mkdir -m 0755 /nix && chown gitpod /nix
USER gitpod

# Install Nix
RUN curl -L https://nixos.org/nix/install | sh

# Add Nix-installed binaries to the PATH.  This needs to be done via
# ENV, as Docker starts separate shells for each layer, so sourced
# environments don't propagate to the "final" session.
ENV PATH=/home/gitpod/.nix-profile/bin:$PATH

# Install git
RUN nix profile install nixpkgs#gitMinimal

# Pre-install/load GHC and HLS in a very hacky way
WORKDIR /tmp
COPY haskell-code haskell-code
COPY flake.nix flake.lock ./
RUN nix profile install .#ghcForFunar .#hls .#cabal-install

WORKDIR /home/gitpod
