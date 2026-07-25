# Prebuilt tidyverse image (includes shiny, readxl, ggplot2) — avoids hour-long
# package compiles that often OOM small EC2 instances.
FROM rocker/tidyverse:4.4.2

WORKDIR /app

COPY app.R .
COPY data ./data

ENV PARKER_DATA_DIR=/app/data

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
