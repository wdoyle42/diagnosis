#!/bin/bash

##Change your download directory here
download_dir=<YOUR DIRECTORY HERE>


cd "$download_dir"

wget http://www2.census.gov/programs-surveys/acs/data/pums/2013/3-Year/csv_hus.zip

wget http://www2.census.gov/programs-surveys/acs/data/pums/2012/3-Year/csv_hus.zip

wget http://www2.census.gov/programs-surveys/acs/data/pums/2011/3-Year/csv_hus.zip

wget http://www2.census.gov/programs-surveys/acs/data/pums/2010/3-Year/csv_hus.zip

wget http://www2.census.gov/programs-surveys/acs/data/pums/2009/3-Year/csv_hus.zip

wget http://www2.census.gov/programs-surveys/acs/data/pums/2008/3-Year/csv_hus.zip
