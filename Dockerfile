FROM bioconductor/bioconductor_docker:latest
LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor/bioconductor_docker:devel container."
WORKDIR /home/rstudio/miadash
COPY --chown=rstudio:rstudio . /home/rstudio/miadash
RUN apt-get update && apt-get install -y libglpk-dev && apt-get clean && rm -rf /var/lib/apt/lists/*

# Fix the corrupt pkgbuild package
RUN R -e "if(file.exists('/usr/local/lib/R/site-library/pkgbuild/R/pkgbuild.rdb')) { remove.packages('pkgbuild'); }; install.packages('pkgbuild', repos='https://cloud.r-project.org/')"

# Install BiocManager and update
RUN R -e "if (!require('BiocManager', quietly = TRUE)) install.packages('BiocManager', repos='https://cloud.r-project.org/'); BiocManager::install(ask=FALSE)"

# Install iSEEtree dependency from GitHub since it's not available in regular repositories
RUN R -e "if (!require('remotes', quietly = TRUE)) install.packages('remotes', repos='https://cloud.r-project.org/'); remotes::install_github('iSEE/iSEEtree')"

# Set environment variable to avoid warnings becoming errors
ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

# Install the package
RUN Rscript -e "devtools::install('.', dependencies = TRUE, repos = BiocManager::repositories(), build_vignettes = FALSE)"
