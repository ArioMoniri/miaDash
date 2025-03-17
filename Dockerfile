FROM bioconductor/bioconductor_docker:devel

LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor/bioconductor_docker:devel container."

# Set the working directory
WORKDIR /home/rstudio/miadash

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libglpk-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Set environment variables to improve build speed
ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true
ENV MAKEFLAGS="-j$(nproc)"
ENV PASSWORD=bioc

# Install Bioconductor dependencies before copying package files
# This creates a separate cache layer for dependencies, improving rebuild times
RUN Rscript -e "BiocManager::install(c(\
    'MatrixGenerics', \
    'matrixStats', \
    'SparseArray', \
    'DelayedArray', \
    'SummarizedExperiment', \
    'SingleCellExperiment', \
    'iSEE', \
    'miaViz', \
    'mia', \
    'TreeSummarizedExperiment', \
    'iSEEtree', \
    'ComplexHeatmap'), \
    update = FALSE, ask = FALSE)"

# Copy package files
COPY --chown=rstudio:rstudio . /home/rstudio/miadash

# Run document() to update Rd files before installation
RUN Rscript -e "setwd('/home/rstudio/miadash'); devtools::document()"

# Install the package
RUN Rscript -e "devtools::install('/home/rstudio/miadash', \
    dependencies = TRUE, \
    repos = BiocManager::repositories(), \
    build_vignettes = TRUE, \
    quiet = TRUE)"

# Expose port 8787 for RStudio Server
EXPOSE 8787

# The rocker/rstudio images already have the CMD set up to start RStudio Server
# So we don't need to override the CMD
