FROM docker.io/rocker/tidyverse:4.1.3
RUN echo "options(repos = c(CRAN = 'https://cloud.r-project.org'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site

RUN Rscript -e 'remotes::install_version("Unicode",upgrade="never", version = "14.0.0-1")'
RUN Rscript -e 'remotes::install_version("pacman",upgrade="never", version = "0.5.1")'
RUN Rscript -e "pacman::p_load_gh('trinker/lexicon','trinker/textclean', 'trinker/textshape', 'trinker/syllable')"

COPY ./Kren.R /home/src/Kren.R
COPY ./examples/usage.R /home/src/examples/usage.R
COPY ./examples/dummy_lyrics.txt /home/src/examples/dummy_lyrics.txt

CMD R -e "setwd('/home/src/examples'); \
          source('/home/src/examples/usage.R')"
