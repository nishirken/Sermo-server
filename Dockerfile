FROM haskell:8.6.2
LABEL name="haskell"
LABEL version="8.6.3"
WORKDIR /app
RUN mkdir app && cd app && stack setup --install-ghc 8.6.3
RUN apt-get update && apt install postgresql libpq-dev -y
CMD stack build --fast --file-watch-poll --exec app-exe
EXPOSE 8080
