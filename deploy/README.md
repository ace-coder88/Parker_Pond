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

## Updating data

Optional Excel drop (legacy backfill only):

```bash
scp Parker.2026.08.xlsx ubuntu@YOUR_HOST:/opt/parker-birds/data/
```

Then click **Reload data** in the UI.

App updates:

```bash
cd /opt/parker-birds
sudo git pull
sudo docker compose up -d --build
```

Check archiver logs:

```bash
sudo docker compose logs -f archiver
```
