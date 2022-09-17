# Salmonid-Body-Size
Analysis of spatial &amp; temporal trends in freshwater salmonid body size

Data & scripts for the paper: "Shrinking body size and climate warming: Many freshwater salmonid fishes do not follow the rule"

# Data files:
1) filteredData.csv- dataset used in the model analysis. Dataset is incomplete due to permission issues, but results produced from the most parsimonious model (lowest AIC) using the full dataset can be found in the file bodysizeModelResults.csv. In order to fully recreate the analysis, separate permission is required to access X% of the data from the following agencies:



Metadata: The Lake column is name of each lake. If the lake name includes a number, it is an associated DNR number (for the lakes in Minnesota) or water body identification code (for the lakes in Wisconsin). The Year column is the year the individual fish was sampled. The Gear column is the type of gear used to capture the individual fish. In some instances, the mesh size or mesh range of the gillnet used is included and is listed in mm. The Species column is the common name of the species of the individual fish caught. See Table 1 in the manuscript for scientific names of each species. The Length column is the length of the individual fish caught, in mm. For Saskatchewan data, length refers to fork length. For the rest of the dataset, length refers to total length. The DataSource column refers to the sampling program that collected each line of data. More information regarding the data source, including contact information and details on sampling protocol/methods collection can be found in the supplementary material associated with the manuscript (Table S1).



2) bodysizeModelResults.csv- the body size trends for each population from the most parsimonious model, with their associated conditional variances. The Lake column is name of each lake. If the lake name includes a number, it is an associated DNR number (for the lakes in Minnesota) or water body identification code (for the lakes in Wisconsin). The Gear column is the type of gear used to capture the individual fish. In some instances, the mesh size or mesh range of the gillnet used is included and is listed in mm. The Species column is the common name of the species of the individual fish caught. See Table 1 in the manuscript for scientific names of each species. The OverallSlope column is the change in length for each population (model slope), not backtransformed- in units of (standard deviation of lengths)/(standard deviation of years). The OverallSlopeBacktransformed column is the change in body length for each population (model slope), in units of mm/year (backtransformed). The totalVar column is the variance of each slope estimate.

3) need to make table(s) with: lake latitude, mean chlorophyl, mean temp, and temp change
  (BodySizeLatLongTable.csv, BodySize_LakeChlorophyllData.csv, LakeTempChangeData.csv, LakeAvgTempData.csv)


# R Scripts:
1) BodySizeMixedModelCode.R
2) BodySizeSecondaryAnalysesCode.R
3) BodySizeFigureCode.R


# Steps for running analysis:
1) BodySizeMixedModelCode.r- This file reads in the length data from filteredData.csv and runs the top three mixed effects models from the main body of the paper, tested with and without random slopes for each species, along with the models that did not include year as a predictor. Using the most parsimonious model (lowest AIC), it saves the results (mm change in length per year) for each population (lake/species/gear combination) into a table with their associated variances. The full table,  bodysizeModelResults.csv, lists the model results for the full dataset used in the paper and is included here.


2) BodySizeSecondaryAnalysesCode.R- This file runs 6 secondary analyses, each with its own predictor variable regressed against change in body size. The predictor variables are: maximum asymptotic body size (L infinity values), average body size at the population level (data average), range position, mean chlorophyll-a, mean water temperature, change in water temperature. It uses the body size changes listed in bodysizeTrends.csv. Data with latitude, mean chlorophyll-a values, mean water temperature and change in water temperature for each lake is needed for the analyses and can be found in the following data files: BodySizeLatLongTable.csv, BodySize_LakeChlorophyllData.csv, LakeTempChangeData.csv, LakeAvgTempData.csv.

3) BodySizeFigureCode.R- This files creates the figures from the paper. It uses data from the files: need to check and see what files you need to have loaded to run this



