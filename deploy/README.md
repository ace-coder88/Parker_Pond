# Parker Birds — EC2 deployment

Interactive Shiny dashboard for Haikubox detections at Parker Pond, Mt. Vernon, ME.

## Local development

```bash
# From the repo root (requires R + shiny, tidyverse, readxl, ggplot2, httr2, RSQLite)
R -e "shiny::runApp('.', port = 3838)"
```

Or with Docker (dashboard + daily archiver):

```bash
docker compose up --build
# open http://localhost:3838
```

## Live panel vs durable archive

| Piece | Cadence | Purpose |
|---|---|---|
| Live now / JSON cache | ~10 minutes | Recent detections in the UI |
| SQLite archiver | **once per day** | Keep hour-level history without Excel exports |

- Archive file: `data/archive/detections.sqlite`
- Serial (public, already on the listen URL): `ECDA3B96F3AC`
- Env: `HAIKUBOX_SERIAL`, `HAIKUBOX_TZ`, `HAIKUBOX_LAT` / `HAIKUBOX_LON`, `HAIKUBOX_REFRESH_MS`, `HAIKUBOX_ARCHIVE_INTERVAL_SEC` (default `86400`)
- **Do not commit account API keys** (e.g. `weft_…`). The public `/haikubox/<serial>/…` endpoints do not need them. If a key was shared in chat or email, rotate/revoke it in the Haikubox account.
- EC2 needs outbound HTTPS to `api.haikubox.com` (and working DNS).

**Gap risk:** the API only retains ~24 hours of hour-level detections. If the archiver or host is down for more than about a day, that window cannot be recovered. After an outage, run a one-shot catch-up (only recovers what is still in the API window):

```bash
docker compose run --rm archiver Rscript /app/scripts/archive_detections.R
```

Legacy monthly Excel files under `data/` still load and merge (Excel wins on duplicate `Species` + `datetime`). New months do not require exports once the archiver has been running.

## EC2 (one-time)

1. Launch **Ubuntu 22.04/24.04** on a small instance (`t3.small` or larger; ≥20 GB disk).
2. Security group: allow **22**, **80**, **443** inbound.
3. SSH in and run:

```bash
sudo git clone -b shiny-ec2-dashboard <your-fork-url> /opt/parker-birds
sudo bash /opt/parker-birds/deploy/setup-ec2.sh
```

4. Point DNS at the instance (Cloudflare: grey-cloud while running certbot).
5. Enable HTTPS:

```bash
sudo certbot --nginx -d birds.yourdomain.example
```

### Auto-deploy (GitHub Actions → SSH)

Pushes to `shiny-ec2-dashboard` on the fork trigger [`.github/workflows/deploy-ec2.yml`](../.github/workflows/deploy-ec2.yml), which SSHs into EC2 and runs [`deploy/update.sh`](update.sh) (`git pull` + `docker compose up -d --build`).

**1. Deploy SSH key (on your laptop):**

```bash
ssh-keygen -t ed25519 -f parker-deploy -N "" -C "github-actions-parker-birds"
```

**2. On EC2** (as the SSH user, usually `ubuntu`):

```bash
# Allow GitHub Actions to log in
mkdir -p ~/.ssh && chmod 700 ~/.ssh
cat >> ~/.ssh/authorized_keys  # paste contents of parker-deploy.pub, then Ctrl-D
chmod 600 ~/.ssh/authorized_keys

# Repo + Docker without sudo (needed for update.sh)
sudo chown -R "$USER:$USER" /opt/parker-birds
sudo usermod -aG docker "$USER"
# log out and back in (or newgrp docker) so the docker group applies

# Smoke-test
git -C /opt/parker-birds pull --ff-only
docker compose -f /opt/parker-birds/docker-compose.yml ps
bash /opt/parker-birds/deploy/update.sh
```

**3. GitHub secrets** on the fork (`Settings` → `Secrets and variables` → `Actions`):

| Secret | Value |
|---|---|
| `EC2_HOST` | Instance public IP or DNS name |
| `EC2_USER` | SSH user (e.g. `ubuntu`) |
| `EC2_SSH_KEY` | Full private key from `parker-deploy` (including `BEGIN` / `END` lines) |

Security group must allow inbound **22** from the internet (or at least from GitHub Actions IP ranges if you tighten that later).

**4. Verify:** push to `shiny-ec2-dashboard`, open the repo **Actions** tab, and confirm the deploy job succeeds. On the host: `docker compose -f /opt/parker-birds/docker-compose.yml ps`.

## Updating data

Optional Excel drop (legacy backfill only):

```bash
scp Parker.2026.08.xlsx ubuntu@YOUR_HOST:/opt/parker-birds/data/
```

Then click **Reload data** in the UI.

### App code updates

Push to `shiny-ec2-dashboard` — GitHub Actions runs `deploy/update.sh` on the instance.

Manual fallback (SSH):

```bash
bash /opt/parker-birds/deploy/update.sh
```

Check archiver logs:

```bash
cd /opt/parker-birds
docker compose logs -f archiver
```
