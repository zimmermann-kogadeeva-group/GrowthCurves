
FROM rocker/shiny:4.4.1

ENV DEBIAN_FRONTEND noninteractive
ENV TZ Europe/Berlin
ENV LC_ALL C

RUN apt update && apt upgrade -y

RUN install2.r --error --skipinstalled \
    shiny \
    ggplot2 \
    ggh4x \
    dplyr \
    tidyr \
    readr \
    purrr \
    lubridate \
    readxl \
    growthrates \
    plotly


COPY R/ /srv/shiny-server/

USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
