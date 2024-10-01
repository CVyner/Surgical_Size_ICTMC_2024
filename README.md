# Surgical sample size

A small directory with code to recreate the poster presented at ICTMC

## gernerate_parameter_files.R

A script to generate all the different scenarios we want to simulate

## functions.R

Utility functions to simulate data, and fit the model

## arc_run_file.R

A file to be submitted via Rscript to allow for parallelisation. Saves the convergence and significant results rate for each simulation

## graphs.R

Produces graphs from the results of arc_run_file.R 
