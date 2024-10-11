#!/bin/bash

out=$(cat "${1}" | bc -q 2>&1)
echo "${out}"
if echo "${out}" | grep -qi "error"; then
    exit 1 
else
    exit 0
fi
