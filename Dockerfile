FROM --platform=linux/amd64 bioconductor/bioconductor_docker:devel
LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor/bioconductor_docker:devel container."
WORKDIR /home/rstudio/miadash
COPY --chown=rstudio:rstudio . /home/rstudio/miadash
RUN apt-get update && apt-get install -y libglpk-dev && apt-get clean && rm -rf /var/lib/apt/lists/*
RUN apt-get update && apt-get install -y \
    libglpk-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libudunits2-dev \
    libgdal-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*
ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

# Install dependencies first
RUN Rscript -e "options(warn=2); BiocManager::install(c('devtools', 'remotes'))"
# Try installing without building vignettes first
RUN Rscript -e "options(warn=2); devtools::install_deps('.', dependencies = TRUE, repos = BiocManager::repositories())"
# Then install the package
RUN Rscript -e "options(warn=2); devtools::install('.', dependencies = FALSE, repos = BiocManager::repositories(), build_vignettes = FALSE)"
