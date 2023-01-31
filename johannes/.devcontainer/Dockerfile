# Pin debian:unstable-slim
FROM debian@sha256:a726325fb0180a84cf1cb540df42959f413bf190aa2e6090d111222f3ddf8a06

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates sudo curl xz-utils

# Add a user and enable them to use sudo (without a password), which
# is necessary to install Nix in the image
RUN groupadd -g 1000 code && useradd -m code -u 1000 -g 1000 -G sudo
RUN sed -i 's/%sudo.*ALL/%sudo ALL=(ALL:ALL) NOPASSWD:ALL/' /etc/sudoers
USER code

# Install Nix
RUN curl -L https://nixos.org/nix/install | sh

# Add Nix-installed binaries to the PATH.  This needs to be done via
# ENV, as Docker starts separate shells for each layer, so sourced
# environments don't propagate to the "final" session.
ENV PATH=/home/code/.nix-profile/bin:$PATH

# Install git and direnv
RUN nix-env -iA nixpkgs.direnv nixpkgs.gitMinimal

# We can speed up the Nix environment creation later on by using our
# pin to "prebuild" (which usually amounts to a simple download)
WORKDIR /home/code
COPY nix nix
RUN nix-build -E "(import (import ./nix/sources.nix).nixpkgs {}).haskell-language-server" \
              -E "(import (import ./nix/sources.nix).nixpkgs {}).cabal-install"
