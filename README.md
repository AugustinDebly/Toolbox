# Toolbox
Hey, welcome to my toolbox! This is where I store all the small utility functions that don't deserve their own repo. I'll keep adding and updating stuff here... until I lose motivation.

## Intertidal chambers
This part is about creating scenarios for ElectricBlue intertidal chambers (https://electricblue.eu/intertidal-chamber) using R. The folder has a <code>functions.R</code> file where all the functions live, and it generates a <code>functions.RData</code> file that you can load directly in your own script. Check out <code>example_workflow.R</code> to see how it works.

## Spectral analysis
This part is about spectral analysis of hyperspectral data. The SRFs folder contains the Spectral Response Functions of various multispectral sensors. The file <code>SRFs.R</code> has all the R functions to interpolate SRF data to a given spectrum, and they are saved in <code>SRFs.RData</code>, ready to load in your script.

## Scaling bias
Just sharing here a repo on Zenodo about the scaling bias : https://zenodo.org/records/19693062. The paper is on its way.

## Underwater image correction
I quickly created some R functions to visually correct underwater images. This was based on a simple method involving histogram equalisation, which is described in a review (https://doi.org/10.1371/journal.pone.0317306).

## Ellipsoidal height to SHOM references
This part is about converting ellipsoidal heights obtained from GPS, into SHOM altitudes (French Hydrographic Service). The conversion is made using the product "BathyElli" (https://refmar.shom.fr/bathyelli). Version 2.1 is used here, but any other version can be uploaded to a new folder in "INPUTS". The name of this folder should be changed to match the R script.
