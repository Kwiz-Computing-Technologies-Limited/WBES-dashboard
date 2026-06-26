# Dockerfile — WBES dashboard for Cloud Run
# Built by Cloud Build (no local Docker daemon needed):
#   gcloud builds submit ...   (see scripts/deploy_cloudrun.sh)
#
# rocker/r-ver pins R and configures Posit Package Manager, so install.packages()
# pulls precompiled Linux BINARIES (fast, no per-package compilation).
FROM rocker/r-ver:4.5.2

# System libraries the R packages link against at runtime.
RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libicu-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libpng-dev \
      libjpeg-dev \
      zlib1g-dev \
      libbz2-dev \
      liblzma-dev \
      ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Runtime R packages (transitive deps resolved automatically).
RUN R -q -e "install.packages(c( \
      'shiny','bslib','cachem','htmltools','plotly','leaflet','DT','reactable', \
      'dplyr','tidyr','purrr','stringr','readr','haven','labelled','arrow', \
      'countrycode','wbstats','digest','scales','RColorBrewer','sass','waiter', \
      'shinyjs','shinyMobile','logger','rlang','here','box','rhino','jsonlite', \
      'httr','MASS'), Ncpus = 4); \
      if (!all(c('shiny','rhino','arrow','plotly','shinyMobile') %in% rownames(installed.packages()))) quit(status = 1)"

WORKDIR /srv/app
COPY . /srv/app

# Cloud Run provides $PORT (default 8080); the app must listen on 0.0.0.0:$PORT.
ENV PORT=8080
EXPOSE 8080

CMD ["R", "-q", "-e", "options(shiny.port = as.integer(Sys.getenv('PORT', '8080')), shiny.host = '0.0.0.0'); shiny::runApp('/srv/app')"]
