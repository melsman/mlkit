#!/bin/sh
# Allocation-volume counters have been replaced by sampled site occupancy.
set -eu
exec sh "$(dirname "$0")/check-occupancy.sh"
