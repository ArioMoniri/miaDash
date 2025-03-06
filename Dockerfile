FROM bioconductor/bioconductor_docker:devel
LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor/bioconductor_docker container."
WORKDIR /home/rstudio/miadash
COPY --chown=rstudio:rstudio . /home/rstudio/miadash
RUN apt-get update && apt-get install -y libglpk-dev && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install BiocManager and update Bioconductor
RUN R -e "if (!require('BiocManager', quietly = TRUE)) install.packages('BiocManager', repos='https://cloud.r-project.org/'); BiocManager::install(ask=FALSE)"

# Try to install iSEEtree from Bioconductor
RUN R -e "BiocManager::install('iSEEtree')"

# Install the package
RUN R -e "devtools::install('.', dependencies = TRUE, repos = BiocManager::repositories(), build_vignettes = FALSE)"

# Set environment variable to avoid warnings becoming errors
ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true
