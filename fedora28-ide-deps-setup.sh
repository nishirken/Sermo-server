#!/usr/bin/env bash

stack upgrade
sudo dnf install postgresql libpqxx-devel.x86_64 zlib-devel.x86_64 ncurses-devel -y
stack install hindent stylish-haskell haskeline intero
stack build --fast --test
