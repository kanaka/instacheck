#!/bin/bash

set -e

SUT() { python -c "from __future__ import division; print '%d' % (${1})"; }
ORACLE() { ruby -e "require 'mathn'; printf '%d', (${1})"; }

res=$(SUT $(cat "${1}"))
check=$(ORACLE $(cat "${1}"))

echo "SUT: ${res}, Oracle: ${check}"
[[ "${res}" == "${check}" ]]
