# Salmonid-Body-Size
Analysis of spatial &amp; temporal trends in freshwater salmonid body size
Data & scripts for the paper: "Shrinking body size and climate warming: Many freshwater salmonid fishes do not follow the rule"

Data files:
-filteredData.csv: all of the data used in the analysis, except for the data providers who did not approve; NEED TO EDIT THIS BASED ON WHAT WE GET APPROVED
-bodysizeTrends.csv: the body size trends for each population from the top model, with their associated conditional variances
-need to make table(s) with: lake latitude, mean chlorophyl, mean temp, and temp change
  BodySizeLatLongTable.csv
  BodySize_LakeChlorophyllData.csv
  LakeTempChangeData.csv
  LakeAvgTempData.csv.
-potentially excel/csv with all models run if i need to include that in body size mixed model code

R Scripts:
-BodySizeMixedModelCode.R
-BodySizeSecondaryAnalysesCode.R
-BodySizeFigureCode.R


Steps for running analysis:
1) BodySizeMixedModelCode.r- This file reads in the length data from filteredData.csv and runs the top three mixed effects models from the paper. It saves the results (change in length per year) for each population (lake/species/gear combination) into a table with their associated variances. The full table,  bodysizeTrends.csv, lists the model results for the full dataset used in the paper and is also included here.


2) BodySizeSecondaryAnalysesCode.R- This file runs 6 secondary analyses, each with its own predictor variable regressed against change in body size. The predictor variables are: maximum asymptotic body size (L infinity values), average body size at the population level (data average), range position, mean chlorophyll-a, mean water temperature, change in water temperature. It uses the body size changes listed in bodysizeTrends.csv 

Data with latitude, mean chlorophyll-a values, mean water temperature and change in water temperature for the lakes is needed for the analyses and can be found in the data files: BodySizeLatLongTable.csv, BodySize_LakeChlorophyllData.csv, LakeTempChangeData.csv, LakeAvgTempData.csv.

3) BodySizeFigureCode.R- This files creates the figures from the paper. It uses data from the files: need to check and see what files you need to have loaded to run this



