FROM --platform=linux/amd64 bioconductor/bioconductor_docker:RELEASE_3_17

LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor container."
WORKDIR /home/rstudio/miadash
COPY --chown=rstudio:rstudio . /home/rstudio/miadash

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libglpk-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libhdf5-dev \
    libgit2-dev \
    libigraph-dev \
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

# Install CRAN packages first
RUN Rscript -e "options(timeout = 600); install.packages(c('igraph', 'ggnewscale', 'tidygraph'), repos = 'https://cloud.r-project.org/')"

# Install the package with dependencies
RUN Rscript -e "options(timeout = 600); devtools::install('.', dependencies = TRUE, repos = BiocManager::repositories(), build_vignettes = FALSE)"
