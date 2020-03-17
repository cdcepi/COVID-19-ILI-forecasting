#!/usr/bin/env bash

echo "> Setting up R dependencies"
wget https://raw.githubusercontent.com/reichlab/pkr/6780f41cc9220d5f2593680a0e2e5501ccd2f152/pkr
sudo chmod +x pkr
sudo ./pkr --version
sudo ./pkr in --file pkrfile --global

echo "> TESTING FORECAST SUBMISSIONS..."
Rscript ./code/test-covid-forecast-formatting.R