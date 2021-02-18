# Project-Tahoe-Central-Sierra-2019

This repository contains the documents, installers, and model inputs for the Tahoe Central Sierra Initiative:  https://sierranevada.ca.gov/what-we-do/tcsi/

The project was funded by The Nature Conservancy and ran from 2019-2021.  The goal was to forecast the effectiveness of management actions on Sierra forests under climate change.  Management scenarios were developed by a separate team of forest and fire managers.  

Charles Maxwell and Robert Scheller, NCSU, were the primary scientists running LANDIS-II for this landscape.  Kristen Wilson and Patricia Manly provided guidance and support.  

This repository contains the following folders:

# LANDIS Inputs

These are the primary inputs for running the LANDIS-II model for the TCSI landscape.  LANDIS inputs include both maps and text files, dependent upon the extensions selected.

# LANDIS Installers

The version of each LANDIS extension used for forecasting forest change.

Extensions included:

- Extension Biomass BDA: Insect mortality (https://github.com/LANDIS-II-Foundation/Extension-Biomass-BDA)
- Extension NECN Succession: Succession and Carbon dynamics (https://github.com/LANDIS-II-Foundation/Extension-NECN-Succession)
- Extension SCRPPLE: Fire mortality (https://github.com/LANDIS-II-Foundation/Extension-SCRPPLE)
- Extension Biomass Harvest: Harvest management (https://github.com/LANDIS-II-Foundation/Extension-Biomass-Harvest)
- Extension Output Biomass: Species biomass (https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass)
- Extension Output Biomass Community: Produces list of all cohorts (https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-Community)
- Extension Output Age Reclass: Produces map outpus by species-age classes (https://github.com/LANDIS-II-Foundation/Extension-Output-Age-Reclassification)

# Scripts

Contains R scripts that were used to derive inputs and to analyze LANDIS-II model outputs.

# Stanislaus_insect_testing

We parameterized the southern Sierra Nevada, using inputs from the 2007 fisher project (https://github.com/LANDIS-II-Foundation/Project-Sierra-Nevada-2007), to calibrate and test our insect extension against the data provided by Chris Fettig in: (https://doi.org/10.1016/j.foreco.2018.09.006).
