#!/bin/sh

set -ex

opam install -y mirage
(cd unikernel && mirage configure -t unix && make depends && mirage build && mirage clean && cd ..) || exit 1
