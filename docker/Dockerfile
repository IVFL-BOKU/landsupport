# An image for the r_calc docker container
# used by the python/eu_class_user.py and scripts/eu_class_user.R
FROM ubuntu:focal
MAINTAINER Mateusz Żółtak <zozlak@zozlak.org>
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y locales
ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8
RUN locale-gen en_US.UTF-8 && \
    apt-get install -y r-base wget libpq-dev libcurl4-openssl-dev libssl-dev git gdal-bin libgdal-dev python3-gdal libxml2-dev libudunits2-dev libproj-dev apache2 libapache2-mod-php python3-requests python3-psycopg2 postgresql-client curl vim && \
    apt-get clean
RUN echo "install.packages('devtools'); devtools::install_github('IVFL-BOKU/sentinel2'); devtools::install_github('IVFL-BOKU/landsupport');" > /tmp/rpackages.R && \
    Rscript /tmp/rpackages.R && \
    rm /tmp/rpackages.R
RUN groupmod -g 3007 www-data  && \
    usermod -u 3007 www-data && \
    chown -R www-data:www-data /var/run/apache2 /var/www/html
CMD ["/bin/bash" "-c" "source /etc/apache2/envvars && /usr/sbin/apache2 -D FOREGROUND"]
EXPOSE 80

