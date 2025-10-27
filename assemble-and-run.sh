#!/bin/sh

if [ $# -ne 1 ]; then
    echo "USAGE: $(basename "$0") FILE"
    echo "assembles FILE and runs the resuling binary in an x86 environment"
    exit 1
fi

container_runtime="podman"

if [ ! -z "$ID2202_INSIDE_SHELL" ]; then
    if [ "$(uname -m)" != "x86_64" ]; then
        echo "Error: This script must be run outside the dev-shell on non-x86 hardware."
        exit 1
    fi
    tmpdir="$(mktemp -d -p /tmp)"
    nasm -felf64 -o "$tmpdir/obj.o" $1
    gcc -z noexecstack -no-pie -o "$tmpdir/a.out" "$tmpdir/obj.o"
    "$tmpdir/a.out"
    rm -rf "$tmpdir"
else
    srcdir="$(cd $(dirname $1) && pwd -P)"
    srcfile="$(basename $1)"
    "$container_runtime" run --rm -it \
        --platform "linux/amd64" \
        -v "$srcdir:/id2202:z,ro" \
        docker.io/johnwikman/id2202:0.3.0-minimal \
        bash -c " nasm -felf64 -o /root/obj.o /id2202/$srcfile \
               && gcc -z noexecstack -no-pie -o /root/a.out /root/obj.o \
               && /root/a.out"
fi
