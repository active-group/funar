#!/usr/bin/env sh

cd "$(dirname "$0")" || exit 1

tag=latest
if [ "$1" != "" ]; then
    tag="$1"
fi

image="funar-devcontainer:$tag"
echo "Building $image..."
docker build -f ../.devcontainer/Dockerfile -t "$image" ..

registry_image=docker.active-group.de/"$image"
echo "Creating $registry_image..."
docker login docker.active-group.de
docker tag "$image" docker.active-group.de/"$image"
docker push docker.active-group.de/"$image"
