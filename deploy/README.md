# Parker Birds — EC2 deployment

Interactive Shiny dashboard for Haikubox detections at Tuxedo Rock.

## Local development

```bash
# From the repo root (requires R + shiny, tidyverse, readxl, ggplot2, httr2)
R -e "shiny::runApp('.', port = 3838)"
```

Or with Docker:

```bash
docker compose up --build
# open http://localhost:3838
```

## Live / semi-live Haikubox data

The app merges monthly Excel files with the **public** Haikubox detections API (last 24 hours), cached under `data/cache/detections.json` and refreshed about every 10 minutes.

- Serial (public, already on the listen URL): `64E8334476B0`
- Env overrides: `HAIKUBOX_SERIAL`, `HAIKUBOX_TZ` (default `America/New_York`), `HAIKUBOX_REFRESH_MS`
- **Do not commit account API keys** (e.g. `weft_…`). The public `/haikubox/<serial>/…` endpoints do not need them. If a key was shared in chat or email, rotate/revoke it in the Haikubox account.
- EC2 needs outbound HTTPS to `api.haikubox.com` (and working DNS).

## EC2 (one-time)

1. Launch **Ubuntu 22.04/24.04** on a small instance (`t3.small` or larger recommended; leave ≥20 GB disk for Docker images).
2. Security group: allow **22**, **80**, **443** inbound.
3. SSH in and run:

```bash
sudo git clone -b shiny-ec2-dashboard <your-fork-url> /opt/parker-birds
sudo bash /opt/parker-birds/deploy/setup-ec2.sh
```

4. Point DNS A/AAAA records at the instance public IP (Cloudflare: grey-cloud / DNS-only while running certbot).
5. Enable HTTPS:

```bash
sudo certbot --nginx -d birds.yourdomain.example
```

6. Share the HTTPS URL with friends.

## Updating monthly data

Copy new Excel files onto the host (volume-mounted into the container):

```bash
scp Parker.2026.08.xlsx ubuntu@YOUR_HOST:/opt/parker-birds/data/
```

Then open the dashboard and click **Reload Excel data** (no rebuild required). Live detections refresh automatically; use **Refresh live data** to force an API pull.

To pull app code updates:

```bash
cd /opt/parker-birds
sudo git pull
sudo docker compose up -d --build
```
