FROM rocker/r-ver:4.3.1
WORKDIR /home/rstudio/miadash

RUN apt-get update && apt-get install -y libglpk-dev && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install Bioconductor if you want 3.17
RUN R -e "install.packages('BiocManager'); \
           BiocManager::install(version='3.17'); \
           BiocManager::install(c('devtools', 'mia', 'iSEE', 'iSEEtree'))" 

COPY . /home/rstudio/miadash

RUN R -e "devtools::install('/home/rstudio/miadash', dependencies=TRUE, build_vignettes=TRUE)"
