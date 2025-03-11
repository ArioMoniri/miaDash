FROM --platform=linux/amd64 bioconductor/bioconductor_docker:devel

LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor container."
WORKDIR /home/rstudio/miadash
COPY --chown=rstudio:rstudio . /home/rstudio/miadash

# Install only essential system dependencies
RUN apt-get update && apt-get install -y \
    libglpk-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

# Copy the package but don't install it or its dependencies
# You can install it manually later
RUN echo "# Manual package installation" > /home/rstudio/README.txt && \
    echo "To install the package, run in R:" >> /home/rstudio/README.txt && \
    echo "  BiocManager::install()" >> /home/rstudio/README.txt && \
    echo "  BiocManager::install(c('biomformat', 'iSEE', 'mia', 'iSEEtree'))" >> /home/rstudio/README.txt && \
    echo "  devtools::install('/home/rstudio/miadash')" >> /home/rstudio/README.txt

# Make sure the package is accessible to the rstudio user
RUN chown -R rstudio:rstudio /home/rstudio
