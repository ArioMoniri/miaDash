FROM bioconductor/bioconductor_docker:devel

LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor/bioconductor_docker:devel container."

# Set environment variables
ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true
ENV PASSWORD=bioc
ENV MAKEFLAGS="-j2"
ENV R_MAX_VSIZE=8Gb

# Set the working directory
WORKDIR /home/rstudio/miadash

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libglpk-dev \
    libhdf5-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install core dependencies first
RUN Rscript -e "options(repos = BiocManager::repositories()); \
    install.packages(c('devtools', 'remotes', 'BiocManager')); \
    BiocManager::install(version = 'devel', ask = FALSE, update = TRUE);"

RUN Rscript -e "options(repos = BiocManager::repositories()); \
    BiocManager::install('biomformat', dependencies = TRUE, ask = FALSE); \
    BiocManager::install('rhdf5', dependencies = TRUE, ask = FALSE);"


RUN Rscript -e "options(repos = BiocManager::repositories()); \
    BiocManager::install('biomformat', dependencies = TRUE, ask = FALSE); \
    BiocManager::install('rhdf5', dependencies = TRUE, ask = FALSE);"
    
# Install mia (another key dependency)
RUN Rscript -e "options(repos = BiocManager::repositories()); \
    BiocManager::install('mia', dependencies = TRUE, ask = FALSE);"

# Install iSEE and iSEEtree
RUN Rscript -e "options(repos = BiocManager::repositories()); \
    BiocManager::install('iSEE', dependencies = TRUE, ask = FALSE); \
    BiocManager::install('iSEEtree', dependencies = TRUE, ask = FALSE);"

# Copy package files
COPY --chown=rstudio:rstudio . /home/rstudio/miadash



# Install the package
RUN Rscript -e "options(repos = BiocManager::repositories()); \
    remotes::install_local('/home/rstudio/miadash', \
    dependencies = TRUE, \
    build_vignettes = FALSE, \
    force = TRUE)"

