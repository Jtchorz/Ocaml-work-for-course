.PHONY: dev-shell mini-shell clean

CONTAINER_RUNTIME ?= podman

IMAGE_DEV_SHELL = docker.io/johnwikman/id2202:0.3.0
IMAGE_MINI_SHELL = docker.io/johnwikman/id2202:0.3.0-minimal

# Enter a ID2202 development shell
dev-shell:
	$(CONTAINER_RUNTIME) run --rm -it \
	    --hostname id2202-shell \
	    --env "ID2202_INSIDE_SHELL=y" \
	    -v "$$(pwd -P):/id2202:z" \
	    -w /id2202 \
	    $(IMAGE_DEV_SHELL) bash

# Enter a minimal ID2202 emulated x86 development shell
mini-shell:
	$(CONTAINER_RUNTIME) run --rm -it \
	    --hostname id2202-mini-shell \
	    --platform "linux/amd64" \
	    --env "ID2202_INSIDE_SHELL=y" \
	    -v "$$(pwd -P):/id2202:z" \
	    -w /id2202 \
	    $(IMAGE_MINI_SHELL) bash

# Remove the container images from your system
clean:
	$(CONTAINER_RUNTIME) rmi -f $(IMAGE_DEV_SHELL) $(IMAGE_MINI_SHELL)
