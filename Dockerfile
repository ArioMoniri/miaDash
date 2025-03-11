FROM --platform=linux/amd64 bioconductor/bioconductor_docker:devel


LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package in a bioconductor container."

# Set up permissions correctly
USER root
WORKDIR /home/rstudio

# Copy the package files
COPY . /home/rstudio/miadash

# Fix permissions
RUN chown -R rstudio:rstudio /home/rstudio/miadash && \
    chmod -R 755 /home/rstudio/miadash

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libglpk-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Switch back to rstudio user
USER rstudio
WORKDIR /home/rstudio
