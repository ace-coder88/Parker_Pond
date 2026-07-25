# Parker Birds — EC2 deployment

Interactive Shiny dashboard for Haikubox detections at Tuxedo Rock.

## Local development

```bash
# From the repo root (requires R + shiny, tidyverse, readxl, ggplot2)
R -e "shiny::runApp('.', port = 3838)"
```

Or with Docker:

```bash
docker compose up --build
# open http://localhost:3838
```

## EC2 (one-time)

1. Launch **Ubuntu 22.04/24.04** on a small instance (`t4g.small` arm64 recommended).
2. Security group: allow **22**, **80**, **443** inbound.
3. SSH in and run:

```bash
sudo git clone <your-repo-url> /opt/parker-birds
sudo REPO_URL=<your-repo-url> APP_DIR=/opt/parker-birds DOMAIN=birds.example.com \
  bash /opt/parker-birds/deploy/setup-ec2.sh
```

If the repo is already on the box at `/opt/parker-birds`:

```bash
sudo bash /opt/parker-birds/deploy/setup-ec2.sh
```

4. Point DNS A/AAAA records at the instance public IP.
5. Enable HTTPS:

```bash
sudo certbot --nginx -d birds.example.com
```

6. Share `https://birds.example.com` with friends.

## Updating monthly data

Copy new Excel files onto the host (volume-mounted into the container):

```bash
scp Parker.2026.08.xlsx ubuntu@YOUR_HOST:/opt/parker-birds/data/
```

Then open the dashboard and click **Reload data** (no rebuild required).

To pull app code updates:

```bash
cd /opt/parker-birds
sudo git pull
sudo docker compose up -d --build
```
