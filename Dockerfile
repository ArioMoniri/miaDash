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
ENV BIOCONDUCTOR_USE_CONTAINER_REPOSITORY=FALSE

# Install package with all dependencies in binary form if available
RUN Rscript -e "options(timeout = 600, repos = BiocManager::repositories()); \
    pkg_deps <- tools::package_dependencies('miaDash', recursive = TRUE, db = available.packages())[[1]]; \
    BiocManager::install(pkg_deps, update = FALSE, ask = FALSE, type = 'binary'); \
    devtools::install('.', dependencies = FALSE, build_vignettes = FALSE)"
