FROM rocker/tidyverse

RUN apt-get install -y libgsl-dev

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

RUN R 

RUN Rscript -e 'devtools::install_github("dirkschumacher/rhxl")'
RUN R 

RUN Rscript -e 'devtools::install_github("romainfrancois/tie")'

RUN apt-get install libxt-dev