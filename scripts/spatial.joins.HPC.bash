#!/bin/bash
#
#PBS -A UQ-SCI-SEES
#
#PBS -l select=1:ncpus=16:mem=120GB
#PBS -l walltime=12:00:00
#PBS -N SimpleDemo
#PBS -m g.kindler@uq.edu.au
#

module load R/4.0.2+Spatial

R/

library(sf)
library(dplyr)

electorates <- st_read("/QRISdata/Q4107/raw_data/AEC_2019_superseded/COM_ELB_region.shp")
species <-  st_read("/QRISdata/Q4107/raw_data/SNES_public_1july2021.gdb")

species.sl <- slice_sample(species, n = 25) %>% select(c("SCIENTIFIC_NAME", "VERNACULAR_NAME", "THREATENED_STATUS", "Shape_Area", "Shape"))
electorates.ss <- slice_sample(electorates, n = 25) %>% select(-c("Numccds", "Actual", "Projected", "Total_Popu", "Australian", "Sortname"))

species.sl <- st_make_valid(species.sl)
electorates.ss <- st_make_valid(electorates.ss)


intersection <- st_intersection(species.sl, electorates.ss)
