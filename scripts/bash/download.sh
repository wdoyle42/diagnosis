#!/bin/bash

##Change your download directory here
download_dir=../../data/acs/

cd "$download_dir"

YEAR=2008

while [ $YEAR -lt 2017 ]; do
  echo Downloading $YEAR . . .
  echo wget http://www2.census.gov/programs-surveys/acs/data/pums/$YEAR/1-Year/csv_hus.zip
  let YEAR=YEAR+1
done
