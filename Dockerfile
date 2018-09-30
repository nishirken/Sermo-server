FROM haskell:8.4.3
WORKDIR /app
RUN apt-get update && apt install postgresql libpq-dev -y
