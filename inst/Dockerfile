FROM rocker/r-ver:4.0.3

RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc && rm -rf /var/lib/apt/lists/*

COPY init.R ./

RUN Rscript init.R

RUN Rscript -e "remotes::install_github('rsprojeto/Rossmann')"

#COPY app.R plumber.R ./

WORKDIR /API/
COPY [".", "./"]

EXPOSE 5000

#CMD ["Rscript", "-e", "Rossmann::run_api(host='0.0.0.0', port=5000)"]

#CMD [RScript -e "Rossmann::run_api()"]

ENTRYPOINT ["R", "-e", "source('plumber.R')"]