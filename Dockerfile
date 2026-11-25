FROM rstudio/plumber

WORKDIR /app

RUN R -e "install.packages('dplyr', repos='https://cloud.r-project.org')"

COPY . /app

EXPOSE 8000

ENTRYPOINT ["Rscript", "run_api.R"]