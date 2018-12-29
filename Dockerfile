FROM haskell:8.6.2
WORKDIR /app
RUN mkdir app && cd app && stack setup --install-ghc 8.6.3
RUN apt-get update && apt install postgresql libpq-dev -y
