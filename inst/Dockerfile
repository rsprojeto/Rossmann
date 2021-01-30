FROM rocker/r-ver:4.0.3

RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc && rm -rf /var/lib/apt/lists/*

COPY init.R ./

RUN Rscript init.R

COPY app.R xg_fit_final.rds plumber.R ./

EXPOSE 5000

# CMD ["Rscript", "-e", "Rossmann::run_api(host='0.0.0.0', port=5000)"]

CMD RScript -e "Rossmann::rodar_api()"