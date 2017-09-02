FROM postgres:9.6
ENV POSTGRES_USER nts
ENV POSTGRES_PASSWORD password
ENV POSTGRES_DB nts
COPY ./pg.sql /docker-entrypoint-initdb.d/
COPY ./config.sh /docker-entrypoint-initdb.d/
