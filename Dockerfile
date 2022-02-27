FROM rocker/tidyverse:4.0.0

RUN R -e "install.packages('svars')"
RUN R -e "install.packages('readxl')"
RUN R -e "install.packages('vars')"
RUN R -e "install.packages('svars')"
RUN R -e "install.packages('tseries')"
RUN R -e "install.packages('VARsignR')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('forecast')"
RUN R -e "install.packages('stats')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('Quandl')"


COPY /codigo.r /codigo.r
COPY /dados.xlsx /dados.xlsx

CMD Rscript /codigo.r