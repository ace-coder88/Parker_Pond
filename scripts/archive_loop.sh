#!/usr/bin/env bash
# Run the detection archiver once, then sleep 24h, forever.
set -euo pipefail

ROOT="${PARKER_APP_ROOT:-/app}"
INTERVAL_SEC="${HAIKUBOX_ARCHIVE_INTERVAL_SEC:-86400}"

echo "archiver starting; interval=${INTERVAL_SEC}s"
while true; do
  if Rscript "${ROOT}/scripts/archive_detections.R"; then
    echo "archiver run succeeded"
  else
    echo "archiver run failed (will retry after interval)" >&2
  fi
  sleep "${INTERVAL_SEC}"
done
