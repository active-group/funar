#!/usr/bin/env sh

# Build and publish two versions of our development Docker image:
#
# - gitpod
# - devcontainer
#
# Those share the same version number.
#
# Invoke this script from the top-level dir as follows:
#
#     ./scripts/build_and_publish_docker_images.sh MY_TAG
#
# This will then build the following two images (both multi-platform, supporting
# x86_64 and arm64), and push them into the Active Group Docker registry:
#
# - docker.active-group.de/funar-gitpod:MY_TAG
# - docker.active-group.de/funar-devcontainer:MY_TAG

set -e

cd "$(dirname "$0")" || exit 1

# Change pins here if you want to user newer base image versions
devcontainer_base=debian@sha256:a726325fb0180a84cf1cb540df42959f413bf190aa2e6090d111222f3ddf8a06
gitpod_base=gitpod/workspace-base@sha256:4b44d2c165e07398d4718d4cf801865c6874dbcf1428634b07a8e668357d48d9

# Default tag is 'latest', otherwise it's the first arg
tag=latest
if [ "$1" != "" ]; then
       tag="$1"
fi

gitpod_image="docker.active-group.de/funar-gitpod:$tag"
echo "Building $gitpod_image and pushing..."
docker buildx build \
       -t "$gitpod_image" \
       --platform linux/amd64 \
       --build-arg username=gitpod \
       --build-arg base_image="$gitpod_base" \
       --push \
       ..

devcontainer_image="docker.active-group.de/funar-devcontainer:$tag"
echo "Building $devcontainer_image and pushing..."
docker buildx build \
       -t "$devcontainer_image" \
       --platform linux/arm64,linux/amd64 \
       --build-arg username=code \
       --build-arg base_image="$devcontainer_base" \
       --push \
       ..
