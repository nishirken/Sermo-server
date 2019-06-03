FROM haskell:8.6.5
LABEL name="haskell"
LABEL version="8.6.5"
WORKDIR /app
RUN mkdir app && cd app
RUN apt-get update && apt install postgresql libpq-dev -y
EXPOSE 8080
