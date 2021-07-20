#!/bin/bash

qsub -I -A UQ-SCI-SEES -l select=1:ncpus=12:mem=60GB  -l walltime=12:00:00