#!/usr/bin/env sh

set -e

cd "$(dirname "$0")" || exit 1

tag=latest
if [ "$1" != "" ]; then
    tag="$1"
fi

image="funar-devcontainer:$tag"
registry_image=docker.active-group.de/"$image"

echo "Building $image and pushing..."
docker buildx build \
       --platform linux/arm64,linux/amd64 \
       -f ../.devcontainer/Dockerfile \
       -t "$registry_image" \
       --push \
       ..
