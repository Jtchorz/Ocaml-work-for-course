# ID2202 Student Repository

This is your private Git repository, where you will submit all programming assignments in the course ID2202. For information on how to use this repository, see the [Assignment Tasks](https://canvas.kth.se/courses/57471/pages/assignment-tasks) page on Canvas. Please read those instructions carefully before starting to work on your assignments.

All instructions in this repository assume that you are working in a Unix-style shell, which is standard on Linux and Mac. On Windows, you need to install [Windows Subsystem for Linux (WSL)](https://learn.microsoft.com/en-us/windows/wsl) to access a Unix-style shell. If you are using Windows and do not have WSL installed, we recommend installing WSL and getting familiar with it before proceeding with these instructions.

## Setting up your development environment

Our proposed development environment involves using Podman. After the course starts, any complementary instructions will be posted on the [Assignment Tasks](https://canvas.kth.se/courses/57471/pages/assignment-tasks) page on Canvas.

The instructions assume that you have `make` available on your system.

### Install Podman

#### Windows

1. Enable [Windows Subsystem for Linux (WSL)](https://learn.microsoft.com/en-us/windows/wsl/install).
2. Follow the instructions for Linux.

#### Linux and Intel Mac

Install [Podman](https://podman.io/docs/installation).

#### ARM Mac

1. Install [Podman Desktop](https://podman-desktop.io/downloads) and follow the setup instructions.
2. Disable the use of macOS Rosetta within the settings of Podman Desktop. You will find this option within `Settings > Resources > Setup Podman > Rosetta`. Refer to the [Assignment Tasks](https://canvas.kth.se/courses/57471/pages/assignment-tasks) page on Canvas for screenshots that show how to locate this option.

### Workflow (all)

In this directory, run

```sh
make dev-shell
```

This will spawn a container with the development environment and mount the current working directory with read-write access to `/id2202` inside the container. This means that changes to files and folders are synced between your current working directory and `/id2202` in the container. The first time you run this command, it will take some time to download the image. To exit the container, press `Ctrl-D` or type `exit`.

We recommend that you edit the source code in your preferred editor on your host system. You can use the container to compile and run your compiler or x86 assembly code.

#### Working with x86 assembly on ARM Mac

This processor uses an ARM architecture, and we are compiling for x86. Therefore, you will need to use another container that emulates x86 when assembling and running the assembly code in Modules 2 and 3. The reason for not emulating the whole `dev-shell` is that emulation is slow. We provide a script, `assemble-and-run.sh`, that you should run on your host (i.e., NOT inside the container) to assemble and run an assembly file.

First, make the script executable with

```sh
chmod +x assemble-and-run.sh
```

Then, you can assemble and run the resulting binary for any x86 assembly file `FILE` with

```sh
./assemble-and-run.sh FILE
```

The first time you run this command it will take a little time to download the container. You can test it with

```sh
./assemble-and-run.sh template-projects/asm/main-asm.asm
```

which should print

```
Hello, World!
```

##### Interactive shell

You can also spawn an interactive shell to the emulated x86 environment, where you have access to `make`, `nasm`, and `gcc`, by running

```sh
make mini-shell
```

Inside `mini-shell`, you will be able to build [template-projects/asm](./template-projects/asm) with

```sh
cd template-projects/asm
make clean && make
```

and run the resulting `main-asm` with

```sh
./main-asm
```

which again should print

```
Hello, World!
```

## Removing the container images

The container images used in this course are hosted [here](https://hub.docker.com/r/johnwikman/id2202). To remove the images from your system, run

```sh
make clean
```

## Using a language-specific container image to reduce disk usage

There are smaller variants of the `dev-shell` that only contain the necessary dependencies for a specific language. Using a language-specific variant reduces disk usage by 50-75%.

To use a language-specific `dev-shell`, edit the Makefile in the root of your repository and add a language suffix to the `IMAGE_DEV_SHELL` variable. For example, to only download dependencies for OCaml:

```Makefile
IMAGE_DEV_SHELL=docker.io/johnwikman/id2202:0.3.0-ocaml
```

Valid suffixes are `-ocaml`, `-scala`, `-rust`, `-java`, and `-haskell`.

## Additional resources

It is not necessary for this course, but if you are interested, here is additional documentation on working with containers in [podman](https://docs.podman.io/en/latest/).
