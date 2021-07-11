#!/bin/bash

qsub -I -A UQ-SCI-SEES -l select=1:ncpus=24:mem=120GB  -l walltime=02:00:00