# Salmonid-Body-Size
Analysis of spatial &amp; temporal trends in salmonid body size
Scripts for the paper: "Shrinking body size and climate warming: Many freshwater salmonid fishes do not follow the rule"

Steps for running analysis:
1) BodySizeMixedModelCode.r- This file reads in the length dataset, formats the data, and runs the top three mixed effects models from the paper. It saves the results (change in length per year) for each population (lake/species/gear combination) into a table with their associated variances.

Publishable length data used in the analysis- allFilteredData.csv; dataset needs to have year, length, gear, and species columns
NEED TO EDIT THIS BASED ON WHAT CAN BE PUBLISHED BEFORE UPLOADING

2) BodySizeSecondaryAnalysesCode.R- This file runs 6 secondary analyses, each with its own predictor variable regressed against change in body size. hte predictor variables are: maximum asymptotic body size (L infinity values), average body size at the population level (data average), range position, average chlorophyll-a, average water temperature, average change in water temperature. Must have run BodySizeMixedModelCode.R prior to running this code.

Data with latitude, average chlorophyll-a values, average water temperature and change in water temperature for the lakes is needed for the analyses, as well as range position and L infinity values. The data used in the paper can be found in the script and the following files: BodySizeLatLongTable.csv, BodySize_LakeChlorophyllData.csv, LakeTempChangeData.csv, LakeAvgTempData.csv.

3) BodySizeFigureCode.R- This files creates Figures 4-6 from the paper. Must have run both BodySizeMixedModelCode.R and BodySizeSecondaryAnalysesCode.R prior to running this code.



