# College Affordability Diagnosis
Collage Affordability Diagnosis Report Files

These files will create the measures for all 50 states that serve as
the basis for college affordability diagnosis. To begin, clone this
directory onto your computer.

The files should be run in the following order:

1. Using bash, run download.sh to get the acs files.
2. Using [R](https://cran.r-project.org/), run the acs.r file. This
   will create a file that includes family income by income group for
   all 50 states.
3. Run the ipeds.r file. This will create a file that
includes net price by sector for all 50 states. The ipeds.r file will
call the functions.r file. This file downloads data directly from
IPEDS if you haven't previously run it. For this step and every subsequent step,
it's strongly recommended that you start a new instance of R. 
4. Run the headcount.r file. This will create a file for headcount 
enrollment by sector.
5.  Run the analysis.r file. This will create a file
comparing net price with income for all 50 states.
6. Run the fte2.r file. This will create a file with fte enrollment in
   public and private instituitions. 
6. Run the state_grants.r file. This will create a file for grant aid
per fte in public and private institutions. 
7. Run the loans.r file. This will create a file for loans per fte.
8. Tech college data for three states (GA, OK, TN) can be created by
   running the _tech files for these states. 
 
 
