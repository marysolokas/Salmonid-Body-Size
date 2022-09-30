# Salmonid-Body-Size
Analysis of spatial &amp; temporal trends in freshwater salmonid body size

Data & scripts for the paper: "Shrinking body size and climate warming: Many freshwater salmonid fishes do not follow the rule"

# R Scripts:
1) BodySizeMixedModelCode.R
2) BodySizeSecondaryAnalysesCode.R
3) BodySizeFigureCode_Final.R

*File paths in R scripts may need to be modified to fit the user's working directory

# Steps for running analyses:
1) BodySizeMixedModelCode.r- This file reads in the length data from filteredData.csv and runs the top three mixed effects models from the main body of the paper, tested with and without random slopes for each species, along with the models that did not include year as a predictor. Using the most parsimonious model (lowest AIC), it saves the results (mm change in length per year) for each population (lake/species/gear combination) into a table with their associated variances. The full table,  bodysizeModelResults.csv, lists the model results for the full dataset used in the paper and is included here. The calculation to get average body sizes at the population level, needed for a secondary analysis, is also included at the end of this file. The average body sizes at the population level for the full dataset can be found in the data file BodySize_PopAvgSize.csv.

2) BodySizeSecondaryAnalysesCode.R- This file runs 6 secondary analyses, each with its own predictor variable regressed against change in body size. The predictor variables are: asymptotic body size (L infinity values), average body size at the population level (data average), range position, mean chlorophyll-a, mean water temperature, change in water temperature. It uses the body size changes listed in bodysizeModelResults.csv. Data for the predictor variables are needed for the analyses and can be found in the following data files: BodySize_PopAvgSize.csv, BodySize_SpeciesAvgSize.csv, BodySize_LatLongTable.csv, BodySize_LakeChlorophyllData.csv, BodySize_LakeAvgTempData.csv, and BodySize_LakeTempChangeData.csv.

3) BodySizeFigureCode_Final.R- This files creates Figures 2, 4, 5, & 6 from the manuscript. Figures 1 & 3 were created using Powerpoint. Figure 2 requires the data file BodySize_WorldMap.csv. Figure 4 requires the data files bodysizeModelResults.csv and BodySize_LatLongTable.csv. Figure 5 requires the data file bodysizeModelResults.csv. Figure 6 requires the BodySizeSecondaryAnalysesCode.R be run first, so that the following dataframes are in the environment: LinfFullTable, popAvgFullTable, rangePosFullTable, chlaFullTable, LakeAvgTempFullTable, TempChangeFullTable.


# Data files:
1) filteredData.csv- dataset used in the model analysis. Dataset is incomplete due to permission issues, but results produced from the most parsimonious model (lowest AIC) using the full dataset can be found in the file bodysizeModelResults.csv. In order to fully recreate the analysis, separate permission is required to access X% of the data from the following agencies:


Metadata: The Lake column is name of each lake. If the lake name includes a number, it is an associated DNR number (for the lakes in Minnesota) or water body identification code (for the lakes in Wisconsin). The Year column is the year the individual fish was sampled. The Gear column is the type of gear used to capture the individual fish. In some instances, the mesh size or mesh range of the gillnet used is included and is listed in mm. The Species column is the common name of the species of the individual fish caught. See Table 1 in the manuscript for scientific names of each species. The Length column is the length of the individual fish caught, in mm. For Saskatchewan data, length refers to fork length. For the rest of the dataset, length refers to total length. The DataSource column refers to the sampling program that collected each line of data. More information regarding the data source, including contact information and details on sampling protocol/methods collection can be found in the supplementary material associated with the manuscript (Table S1).


2) bodysizeModelResults.csv- the body size trends for each population from the most parsimonious model, with their associated conditional variances. 

Metadata: The Lake column is the name of each lake. If the lake name includes a number, it is an associated DNR number (for the lakes in Minnesota) or water body identification code (for the lakes in Wisconsin). The Gear column is the type of gear used to capture the individual fish. In some instances, the mesh size or mesh range of the gillnet used is included and is listed in mm. The Species column is the common name of the species of the individual fish caught. See Table 1 in the manuscript for scientific names of each species. The OverallSlope column is the change in length for each population (model slope), not backtransformed- in units of (standard deviation of lengths)/(standard deviation of years). The OverallSlopeBacktransformed column is the change in body length for each population (model slope), in units of mm/year (backtransformed). The totalVar column is the variance of each slope estimate.


3) BodySize_PopAvgSize.csv- the average body lengths for each population (data average), needed for a secondary analysis

Metadata: The Lake column is the name of each lake. If the lake name includes a number, it is an associated DNR number (for the lakes in Minnesota) or water body identification code (for the lakes in Wisconsin). The Gear column is the type of gear used to capture the individual fish. In some instances, the mesh size or mesh range of the gillnet used is included and is listed in mm. The Species column is the common name of the species of the individual fish caught. See Table 1 in the manuscript for scientific names of each species. The avgPopLength column is the average length, in mm, of each population (unique species/gear/lake combination).


4) BodySize_SpeciesAvgSize.csv- the asymptotic (L infinity values) length of each species, needed for a secondary analysis

Metadata: The Species column is the common name of the species of the individual fish caught. See Table 1 in the manuscript for scientific names of each species. The Linf column is the asymptotic length of each species, in cm.


5) BodySize_LatLongTable.csv- latitudes and longitudes for each lake; latitudes are needed for a secondary analysis

Metadata: The Source column refers to the sampling program that collected data from the corresponding lake. More information regarding the data source, including contact information and details on sampling protocol/methods collection can be found in the supplementary material associated with the manuscript (Table S1). The Lake column is the name of each lake. If the lake name includes a number, it is an associated DNR number (for the lakes in Minnesota) or water body identification code (for the lakes in Wisconsin). Latitude and longitude correspond to the middle of each lake.


6) BodySize_LakeChlorophyllData.csv- mean chlorophyll-a values for 163 of our study lakes

Metadata: The Source column refers to the organization that collected the chlorophyll-a data from each. Additional details can be found in the methods section of the manuscript. The Lake column is the name of each lake. If the lake name includes a number, it is an associated DNR number (for the lakes in Minnesota) or water body identification code (for the lakes in Wisconsin). The AvgChlorophyll column refers to the mean chlorophyll-a concentration in each lake, in micrograms/L.


7) BodySize_LakeAvgTempData.csv- mean surface water temperature for 172 of our study lakes over the body size data survey period

Metadata: The Lake column is the name of each lake. If the lake name includes a number, it is an associated DNR number (for the lakes in Minnesota) or water body identification code (for the lakes in Wisconsin). The avgLakeTemp is the mean surface water temperature over the body-size survey period, in degrees C. More details on the body-size survey period can be found in the data file BodySize_LakeTempChangeData.csv.


8) BodySize_LakeTempChangeData.csv- change in surface water temperature over the body size data survey period for 172 of our study lakes

Metadata: The Lake column is the name of each lake. If the lake name includes a number, it is an associated DNR number (for the lakes in Minnesota) or water body identification code (for the lakes in Wisconsin). The Species column is the common name of the species of the individual fish caught. See Table 1 in the manuscript for scientific names of each species. The LengthDataStartYear and LengthDataEndYear refer to the body size data survey period. TempChange is the temperature changes over the body size data survey period, in degrees Celsius. The TempDataStartYear and TempDataEndYear refer to the temperature data time period. EarlyLengthYearsMissingTemp refer to how many years the length data started before temperature data started, if any. For example, in Lake Ontario length data started in 1992 but temperature data did not start until 1995, so there are 3 years of length data prior to temperature data begins. LateLengthYearsMissingTemp refer to how many years the length data continued after the temperature data ended, if any. For example, in Lake Constance length data ends in 2020 but temperature data only went through 2019.

9) BodySize_WorldMap.csv- general latitudes and longitudes for our study sites, by genus

Metadata: The Location column indicates each general location where we have data from, along with the different genera we have data for. The GeneralLat and GeneralLong columns indicate general latitude and longitudes of areas we have data from. For many, these aren't the exact locations of a lake with data but rather the middle point of a region where we have data from many lakes.
