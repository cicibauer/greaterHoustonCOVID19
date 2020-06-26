This folder consists of the ACS data processing and index mapping information

1. ACS data description word doc: description of some selected ACS variables from 2015 American Survey 5 year estimates. The high-lightened variables are the selected 15 variable for creating SES indices.

2. ACS_data_processing.rmd : R codes for downloading and cleaning ACS data using package 

3. acs.csv: csv file containing selected ACS data at census tract level for US

4. ACS_index_mapping.rmd: R codes for creation of SES indices
We included all the populated census tracts for the continental United States (n=73483) to preserve neighboring relationship across tracts. 15 variables are selected to quantify variations across small areas in socioeconomic status, demographic characteristics of disadvantaged groups, and housing and transportation. Then a multidimensional index was developed based on the results of PCA.  An index score was produced for each of the identified component and a composite SDoH index was constructed by summing up the weighted principal component scores. This multidimensional index, along with other indicators, can be applied to identify potential areas that need more health care recourses.

5. acs_index.csv: csv file containing selected ACS data and index at census tract level for US
