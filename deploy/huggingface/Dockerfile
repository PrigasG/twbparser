# twbparser Shiny app — Hugging Face Docker Space
# Builds the published package from GitHub and serves the bundled Shiny app.
FROM rocker/r-ver:4.4.1

# System libraries required by xml2 (libxml2), igraph (libglpk), and
# remotes/curl downloads (libcurl/openssl).
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libglpk-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Runtime packages. rocker/r-ver ships with Posit Public Package Manager
# configured, so these install as Linux binaries (fast, no compilation).
RUN R -e "install.packages(c('remotes','shiny','dplyr','igraph','purrr','R6','rlang','stringr','tibble','tidyr','withr','xml2'))"

# Install twbparser itself (small, pure-R). Pin to a package tag so Docker
# rebuilds cannot reuse an older HEAD layer and serve stale parser/report code.
ARG TWBPARSER_REF=v0.5.0
RUN R -e "remotes::install_github('PrigasG/twbparser', ref = '${TWBPARSER_REF}', upgrade = 'never')"

# Hugging Face Spaces route traffic to this port (see README app_port).
ENV TWBPARSER_MAX_UPLOAD_MB=100
EXPOSE 7860

CMD ["R", "-e", "twbparser::run_twbparser_app(host = '0.0.0.0', port = 7860)"]
