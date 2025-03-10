FROM bioconductor/bioconductor_docker:devel
LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor/bioconductor_docker:devel container."
WORKDIR /home/rstudio/miadash
COPY --chown=rstudio:rstudio . /home/rstudio/miadash

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libglpk-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

# Try installing the package dependencies first
RUN Rscript -e "devtools::install_deps('.', dependencies = TRUE, repos = BiocManager::repositories())"

# Then install the package itself (without building vignettes at first to isolate issues)
RUN Rscript -e "devtools::install('.', dependencies = FALSE, repos = BiocManager::repositories(), build_vignettes = FALSE)"
