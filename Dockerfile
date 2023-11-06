ARG base_image
FROM $base_image

USER root
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates sudo curl xz-utils

# Add a user and enable them to use sudo (without a password), which is
# necessary to install Nix in the image. NOTE for trainees who run into
# permission problems inside the container (for instance, not being able to save
# files qualifies): You might want to try changing the UID and GID here
# according to the ones on your host machine, and also use the commented line in
# devcontainer.json instead of the "image" one.
#
# We only do this for the VScode "Dev Container" extension, as gitpod already
# has an existing 'gitpod' user with the correct settings.
#
# TODO: use multi-stage here instead, and branch out
ARG username
RUN if [ "$username" != gitpod ]; then \
        groupadd -g 1000 $username && useradd -m $username -u 1000 -g 1000 -G sudo \
        && sed -i 's/%sudo.*ALL/%sudo ALL=(ALL:ALL) NOPASSWD:ALL/' /etc/sudoers ; \
    fi

# Enable flakes (before changing the user)
RUN mkdir -p /etc/nix && \
    echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf

# Necessary for ARM64 for some reason
RUN mkdir -m 0755 /nix && chown "$username" /nix
USER $username

# Install Nix
RUN curl -L https://nixos.org/nix/install | sh

# Add Nix-installed binaries to the PATH. This needs to be done via ENV, as
# Docker starts separate shells for each layer, so sourced environments don't
# propagate to the "final" session.
ENV PATH=/home/$username/.nix-profile/bin:$PATH

# Install git
RUN nix profile install nixpkgs#gitMinimal

# Pre-install/load GHC and HLS in a very hacky way
WORKDIR /tmp
COPY haskell-code haskell-code
COPY hearts hearts
COPY flake.nix flake.lock ./
RUN nix profile install .#ghcForFunar .#hls .#cabal-install

WORKDIR /home/$username
