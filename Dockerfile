FROM --platform=linux/amd64 bioconductor/bioconductor_docker:devel
LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor/bioconductor_docker:devel container."
WORKDIR /home/rstudio/miadash
COPY --chown=rstudio:rstudio . /home/rstudio/miadash

# Install system dependencies including HDF5 libraries
RUN apt-get update && apt-get install -y \
    libglpk-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libhdf5-dev \
    libhdf5-serial-dev \
    libgit2-dev \
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

# Install the HDF5-related packages with specific options
RUN Rscript -e "options(timeout = 300); BiocManager::install('Rhdf5lib', configure.args = c(Rhdf5lib = '--with-hdf5=/usr/lib/x86_64-linux-gnu/hdf5/serial'), update = FALSE, ask = FALSE)"
RUN Rscript -e "options(timeout = 300); BiocManager::install(c('rhdf5filters', 'rhdf5'), update = FALSE, ask = FALSE)"

# Install biomformat with extended timeout
RUN Rscript -e "options(timeout = 300); BiocManager::install('biomformat', update = FALSE, ask = FALSE)"

# Install other Bioconductor dependencies
RUN Rscript -e "options(timeout = 300); BiocManager::install(c('iSEE', 'iSEEtree', 'mia', 'scater'), update = FALSE, ask = FALSE)"

# Then install the package itself
RUN Rscript -e "options(timeout = 300); devtools::install('.', dependencies = TRUE, repos = BiocManager::repositories(), build_vignettes = FALSE)"
