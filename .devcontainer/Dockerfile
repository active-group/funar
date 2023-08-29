# Pin debian:unstable-slim
FROM debian@sha256:a726325fb0180a84cf1cb540df42959f413bf190aa2e6090d111222f3ddf8a06

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates sudo curl xz-utils

# Add a user and enable them to use sudo (without a password), which
# is necessary to install Nix in the image.  NOTE for trainees who run
# into permission problems inside the container (for instance, not
# being able to save files qualifies): You might want to try changing
# the UID and GID here according to the ones on your host machine, and
# also use the commented line in devcontainer.json instead of the
# "image" one.
RUN groupadd -g 1000 code && useradd -m code -u 1000 -g 1000 -G sudo
RUN sed -i 's/%sudo.*ALL/%sudo ALL=(ALL:ALL) NOPASSWD:ALL/' /etc/sudoers

# Flakes einschalten (bevor User gewechselt wird)
RUN mkdir -p /etc/nix && \
    echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf

USER code

# Install Nix
RUN curl -L https://nixos.org/nix/install | sh

# Add Nix-installed binaries to the PATH.  This needs to be done via
# ENV, as Docker starts separate shells for each layer, so sourced
# environments don't propagate to the "final" session.
ENV PATH=/home/code/.nix-profile/bin:$PATH

# Install git and direnv
RUN nix-env -iA nixpkgs.direnv nixpkgs.gitMinimal

WORKDIR /home/code
