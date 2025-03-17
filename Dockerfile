FROM bioconductor/bioconductor_docker:devel

LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor/bioconductor_docker:devel container."

# Set the working directory
WORKDIR /home/rstudio/miadash

# Set environment variables
ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true
ENV PASSWORD=bioc
# Limit number of parallel processes to reduce memory usage
ENV MAKEFLAGS="-j2"
# Set memory limit for R processes
ENV R_MAX_VSIZE=4Gb

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libglpk-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install Bioconductor dependencies one at a time to reduce memory usage
RUN Rscript -e "options(repos = BiocManager::repositories()); \
    install.packages('matrixStats'); \
    BiocManager::install('MatrixGenerics', update = FALSE, ask = FALSE)"

RUN Rscript -e "options(repos = BiocManager::repositories()); \
    BiocManager::install(c('SparseArray', 'DelayedArray'), update = FALSE, ask = FALSE)"

RUN Rscript -e "options(repos = BiocManager::repositories()); \
    BiocManager::install('SummarizedExperiment', update = FALSE, ask = FALSE)"

RUN Rscript -e "options(repos = BiocManager::repositories()); \
    BiocManager::install('SingleCellExperiment', update = FALSE, ask = FALSE)"

RUN Rscript -e "options(repos = BiocManager::repositories()); \
    BiocManager::install(c('TreeSummarizedExperiment', 'ComplexHeatmap'), update = FALSE, ask = FALSE)"

RUN Rscript -e "options(repos = BiocManager::repositories()); \
    BiocManager::install(c('iSEE', 'mia'), update = FALSE, ask = FALSE)"

RUN Rscript -e "options(repos = BiocManager::repositories()); \
    BiocManager::install(c('miaViz', 'iSEEtree'), update = FALSE, ask = FALSE)"

# Copy package files
COPY --chown=rstudio:rstudio . /home/rstudio/miadash



# Install the package with minimal memory usage
RUN Rscript -e "options(repos = BiocManager::repositories()); \
    devtools::install('/home/rstudio/miadash', \
    dependencies = TRUE, \
    build_vignettes = FALSE, \
    quiet = TRUE)"

# Build vignettes separately if needed (optional)
# RUN Rscript -e "devtools::build_vignettes('/home/rstudio/miadash')"

# Expose port 8787 for RStudio Server
EXPOSE 8787
