FROM ubuntu:22.04

RUN apt-get update -y && apt-get install -y gcc gcc-riscv64-linux-gnu ca-certificates cmake git ninja-build
RUN git config --global --add safe.directory '*'

ENV CARGO_TARGET_RISCV64GC_UNKNOWN_LINUX_GNU_LINKER=riscv64-linux-gnu-gcc
