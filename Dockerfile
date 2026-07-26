# Prebuilt tidyverse image (includes shiny, readxl, ggplot2) — avoids hour-long
# package compiles that often OOM small EC2 instances.
FROM rocker/tidyverse:4.4.2

RUN R -e 'install.packages(c("httr2"), repos = "https://cloud.r-project.org")'

WORKDIR /app

COPY app.R .
COPY R ./R
COPY data ./data

ENV PARKER_DATA_DIR=/app/data
ENV HAIKUBOX_SERIAL=64E8334476B0
ENV HAIKUBOX_TZ=America/New_York
ENV HAIKUBOX_LAT=41.1915
ENV HAIKUBOX_LON=-74.1171
ENV HAIKUBOX_REFRESH_MS=600000

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
