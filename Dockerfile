FROM bioconductor/bioconductor_docker:latest
LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor/bioconductor_docker:devel container."
WORKDIR /home/rstudio/miadash
COPY --chown=rstudio:rstudio . /home/rstudio/miadash
RUN apt-get update && apt-get install -y libglpk-dev && apt-get clean && rm -rf /var/lib/apt/lists/*

# Fix the corrupt pkgbuild package
RUN R -e "if(file.exists('/usr/local/lib/R/site-library/pkgbuild/R/pkgbuild.rdb')) { remove.packages('pkgbuild'); }; install.packages('pkgbuild', repos='https://cloud.r-project.org/')"

# Install BiocManager and update Bioconductor
RUN R -e "if (!require('BiocManager', quietly = TRUE)) install.packages('BiocManager', repos='https://cloud.r-project.org/'); BiocManager::install(ask=FALSE)"

# Try installing iSEE which might be a prerequisite 
RUN R -e "BiocManager::install('iSEE')"

# Install remotes and try alternative repository structure for iSEEtree
RUN R -e "if (!require('remotes', quietly = TRUE)) install.packages('remotes', repos='https://cloud.r-project.org/'); remotes::install_github('iSEE/iSEE.options')"

# Try a more direct approach to install miaDash bypassing dependency checks
RUN R -e "options(repos = BiocManager::repositories()); remotes::install_local('.', dependencies=FALSE, INSTALL_opts='--no-lock')"

# Set environment variable to avoid warnings becoming errors
ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true
