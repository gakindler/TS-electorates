#!/bin/bash

qsub -I -l select=1:ncpus=12:mem=60GB  -l walltime=05:00:00