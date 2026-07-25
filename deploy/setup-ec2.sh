#!/usr/bin/env bash
# Bootstrap an Ubuntu EC2 host for the Parker Birds Shiny dashboard.
# Run as root (or with sudo) on a fresh Ubuntu 22.04/24.04 arm64 or amd64 instance.
set -euo pipefail

APP_DIR="${APP_DIR:-/opt/parker-birds}"
REPO_URL="${REPO_URL:-}"

export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y --no-install-recommends \
  ca-certificates \
  curl \
  git \
  nginx \
  certbot \
  python3-certbot-nginx \
  ufw

# Docker Engine
if ! command -v docker >/dev/null 2>&1; then
  curl -fsSL https://get.docker.com | sh
  systemctl enable --now docker
fi

# Docker Compose plugin is included with get.docker.com on modern Ubuntu

mkdir -p "$APP_DIR"

if [[ -n "$REPO_URL" ]]; then
  if [[ -d "$APP_DIR/.git" ]]; then
    git -C "$APP_DIR" pull --ff-only
  else
    git clone "$REPO_URL" "$APP_DIR"
  fi
fi

if [[ ! -f "$APP_DIR/docker-compose.yml" ]]; then
  echo "Place the project in $APP_DIR (or set REPO_URL=… when running this script)."
  echo "Expected files: docker-compose.yml, Dockerfile, app.R, data/"
  exit 1
fi

cp "$APP_DIR/deploy/nginx-parker-birds.conf" /etc/nginx/sites-available/parker-birds
ln -sfn /etc/nginx/sites-available/parker-birds /etc/nginx/sites-enabled/parker-birds
rm -f /etc/nginx/sites-enabled/default

# Replace placeholder hostname if DOMAIN is provided
if [[ -n "${DOMAIN:-}" ]]; then
  sed -i "s/server_name _;/server_name ${DOMAIN};/" /etc/nginx/sites-available/parker-birds
fi

nginx -t
systemctl enable --now nginx

ufw allow OpenSSH
ufw allow 'Nginx Full'
ufw --force enable

cd "$APP_DIR"
docker compose up -d --build

echo
echo "Shiny is running behind nginx on port 80."
echo "Point DNS at this instance, then enable HTTPS:"
echo "  sudo certbot --nginx -d your.domain.example"
echo
echo "Reload after dropping new Excel files into $APP_DIR/data/:"
echo "  open the app and click 'Reload data'"
