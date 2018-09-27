#!/bin/bash

set -e

SUT() {
  python -c "from __future__ import division; print '%d' % ($1)"
}
ORACLE() {
  ruby -e "require 'mathn'; printf '%d', ($1)"
}

[[ "$(SUT $(cat "$1"))" == "$(ORACLE $(cat "$1"))" ]]
