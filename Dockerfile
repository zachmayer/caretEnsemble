# Use OpenHands runtime as base
FROM docker.all-hands.dev/all-hands-ai/runtime:0.15-nikolaik

# Switch to root for system installs
USER root

# Install R and required system dependencies
RUN apt-get update && apt-get install -y \
    r-base \
    r-base-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages system-wide
RUN R -e "install.packages(c('data.table', 'ggplot2', 'reshape2', 'glmnet', 'xgboost', 'devtools', 'roxygen2'), repos='https://cran.rstudio.com/')"

# Add after R package installation
RUN R -e " \
    library(data.table); \
    library(ggplot2); \
    library(reshape2); \
    library(glmnet); \
    library(xgboost); \
    library(devtools); \
    library(roxygen2); \
    cat('All required R packages loaded successfully\n') \
"

# Switch back to openhands user
USER openhands

# Set working directory
WORKDIR /workspace

# Switch to openhands user
USER openhands 