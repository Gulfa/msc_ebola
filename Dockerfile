FROM rocker/geospatial

RUN apt-get install -y libgsl-dev jq libjq-dev libudunits2-dev libprotobuf-dev  libv8-dev

RUN install2.r --error \
    --deps TRUE \
    gsl

RUN install2.r --error \
    --deps TRUE \
    EpiEstim \
    R6 \
    bsts \
    data.table \
    scoringRules \
    goftest \
    xtable 

RUN install2.r --error \
    --deps TRUE \
    geojsonio \
    ggrepel \
    ISOweek \
    matrixStats


RUN Rscript -e 'devtools::install_github("dirkschumacher/rhxl")'

RUN Rscript -e 'devtools::install_github("romainfrancois/tie")'

RUN apt-get -y install libxt-dev