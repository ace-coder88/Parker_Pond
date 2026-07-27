#!/usr/bin/env bash
# Pull shiny-ec2-dashboard and rebuild containers on the EC2 host.
# Used by GitHub Actions auto-deploy and for manual updates.
set -euo pipefail

APP_DIR="${APP_DIR:-/opt/parker-birds}"
BRANCH="${BRANCH:-shiny-ec2-dashboard}"

cd "$APP_DIR"

echo "==> $(date -u +%Y-%m-%dT%H:%M:%SZ) updating $APP_DIR ($BRANCH)"

git fetch origin
git checkout "$BRANCH"
git pull --ff-only origin "$BRANCH"

echo "==> rebuilding containers"
docker compose up -d --build

echo "==> status"
docker compose ps

echo "==> $(date -u +%Y-%m-%dT%H:%M:%SZ) deploy complete"
