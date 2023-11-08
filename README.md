# pipeline
v1.0 is currently under development. When available for use, this message will be removed.

## Problem formulation 
Data manipulation and cleaning process is repeated any time models are trained; thus, training a model is extremely time intensive, before it is even clear if the exercise is likely to be fruitful. 

## Objective
To deliver a machine learning suitable database by uploading only the PUF. The dataframe output should be suitable for either 
neural networks (ie matrices of 1s and 0s) or random forests. All data is preserved, and variables are appropriately treated according to variable type.

The following is based on the 2020 NCDB PUF data dictionary, found here: https://www.facs.org/media/brilfbgu/puf-2020-data-dictionary.pdf

## Collaboration
Contributions and collaboration are welcomed.

Currently active problems:
- Review of NCDBRecode.R to ensure accuracy according to 2020 PUF data dictionary
- Need for specific variables to be encoded; SEQUENCE_NUMBER, PRIMARY_SITE, HISTOLOGY, Grade_Clin, Grade_Path, Grade_Path_Post

## How to Cite
Please cite this repository if it was useful for your research:
Collin Dougherty. (2023). pipeline Version 1.0

## How to Use
Download this repository to your device or access at the following [url.]() - note to self, use flask app to make interface simple.

## Credits
Thanks to [@Augersam](https://github.com/augersam) for extensive work on [NCDBRecode](https://github.com/augersam/NCDBRecode), which I have extensively used here as a submodule to recode NCDB variables, with some updates reflecting 2016 to 2020 changes.
