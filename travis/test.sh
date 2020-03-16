#!/usr/bin/env bash

echo "> Setting up R dependencies"
sudo chmod +x pkr
sudo ./pkr --version
sudo ./pkr in --file pkrfile --global

echo "Testing formatting"
Rscript ./code/test-covid-forecast-formatting.R