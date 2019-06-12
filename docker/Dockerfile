# start with the official R project base image
FROM r-base:3.5.1

# Install what is needed in linux 
RUN apt-get update
RUN apt-get -y install libssl-dev # /unstable
RUN apt-get -y install libcurl4-openssl-dev libxml2-dev


COPY ./install /install
COPY ./scripts /scripts
COPY ./data /data

# Install the R packages
RUN Rscript /install/install-packages.R

	
# Execute the target script
CMD ["Rscript", "scripts/script.R"]