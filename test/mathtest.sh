#!/bin/bash

set -e

exp=$(cat "${1}")

py_result=$(python -c "from __future__ import division; print '%d' % (${exp})")
rb_result=$(ruby -e "require 'mathn'; printf '%d', (${exp})")

echo "Python result: ${py_result}, Ruby result: ${rb_result}"
[[ "${py_result}" == "${rb_result}" ]]
