FROM bioconductor/bioconductor_docker:latest
LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor/bioconductor_docker:devel container."
WORKDIR /home/rstudio/miadash
COPY --chown=rstudio:rstudio . /home/rstudio/miadash
RUN apt-get update && apt-get install -y libglpk-dev && apt-get clean && rm -rf /var/lib/apt/lists/*

# Fix the corrupt pkgbuild package
RUN R -e "if(file.exists('/usr/local/lib/R/site-library/pkgbuild/R/pkgbuild.rdb')) { remove.packages('pkgbuild'); }; install.packages('pkgbuild', repos='https://cloud.r-project.org/')"

# Set environment variable to avoid warnings becoming errors
ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

# Install dependencies first
RUN Rscript -e "devtools::install_deps('.', dependencies = TRUE, repos = BiocManager::repositories())"

# Then install the package without building vignettes initially
RUN Rscript -e "devtools::install('.', dependencies = FALSE, repos = BiocManager::repositories(), build_vignettes = FALSE)"
