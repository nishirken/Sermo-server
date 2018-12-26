FROM haskell:8.4.4
WORKDIR /app
RUN apt-get update && apt install postgresql libpq-dev -y
