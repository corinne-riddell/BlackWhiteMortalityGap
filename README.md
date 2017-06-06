# Long-term trends in the contribution of major causes of death to the black-white life expectancy gap by US state
### Corinne A Riddell, Kathryn T Morrison, Sam Harper, Jay S Kaufman

This repository contains the data and code used to conduct an in-depth investigation of trends in the black-white life expectancy gap by US state. All the code needed to conduct the analysis described in our [working paper](http://biorxiv.org/content/early/2017/05/25/140152) can be found in this repository. A full set of our results are publicly available online in an interactive [web application](https://corinne-riddell.shinyapps.io/black-white-life-expectancy/). 

### Significance

Racial differences have been a fixture of American life for centuries, but their continued evolution is a topic of great relevance for public policy and social justice. States have fared differently in eliminating the black-white life expectancy gap. Using vital statistics and census data, we describe how six major causes of death contributed to the life expectancy gap between blacks and whites from 1969 to 2013. We find that states diverged in their cause-of-death specific contributions to the life expectancy gap in ways that either resulted in the near or total elimination of the gap, or alternatively its exacerbation. This analysis provides a foundation for future work to investigate state characteristics that may result in inequalities in mortality by race.

### How to replicate these analyses

If your intention is to replicate our analysis and you are familiar with github, please clone this repository. All of the analysis is contained within the numerically ordered files contained in the Code directory. There are two lines of code (#161 and #508)you will need to remove or modify the pathway in 2_entire_analysis.R -- see the NOTE at the top of this file for more information.

If you are unfamiliar with github, but familiar with R and RStudio, you may wish to download the analysis files by navigating to Code directory and download the numeric files (i.e., from 1_Explore_Data.Rmd to 4_combined-meta-regression.Rmd). You can download the raw version of these file and open locally within RStudio. You will also need to download the primary data file deaths-cause-6913.txt found in the Data directory. You can then run the code chunks within RStudio to replicate the analysis. 

### How to reproduce the working paper

To reproduce the working paper, use RStudio to "Knit" the manuscript file Manuscript_COD_Trends_PNAS.Rmd found in the Manuscript folder. This file uses data outputs (referenced in lines 37-50) that will also need to be downloaded. Alternatively, you can reproduce these data outputs yourself by following the steps in the "How to replicate these analyses" section.

### Code for the web application

The code for the web application is contained in the app.R file found in the shiny_app folder. Like the working paper, it depends on data outputs that you will need to download from this repository or replicate yourself by re-running the analysis.

### Copyright

The preprint is made available under a [CC-BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/) International license.

The code has its own DOI that can be used to link to, and cite this work: [![DOI](https://zenodo.org/badge/68300074.svg)](https://zenodo.org/badge/latestdoi/68300074).
