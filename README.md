# Code to analyze loon blood mercury

Kevin P. Kenow, Richard A. Erickson, Travis J. Harrison

This code supports the manuscript "Patterns of mercury and selenium exposure in Minnesota common loons".
Specifically, we used this code to examine Loon blood Hg concentrations were related with predictions of Hg concentrations in standardized 12-cm whole organism yellow perch (Perca flavescens), based on fish Hg records from lakes throughout Minnesota, using the U.S. Geological Survey National Descriptive Model for Mercury in Fish.

Code: https://doi.org/10.5066/P9QHT2DG
Data: https://doi.org/10.5066/P9TDCH3F

## Censored regression model

- `fixingLakeID2018_03_09.R` The censored regression model, model predictions, loon-perch Hg predctions join logic and minor QAQC.
- `LoonDataClean2018_03_09.R` QAQC for the loon data and lake-perch Hg prediction join logic.
- `LoonAnalysis2018_03_09.R` The analysis of the loon, perch Hg predctions and lake data.
- `perch_map_2018_03_09.R` Code for joining and mapping water data with predicted perch Hg.
- `coords/utmToDec.py` Tool for converting utm to decimal coordinates.


## Contact for code 

Primary code developer:  Richard A. Erickson (rerickson@usgs.gov).  
Secondary code developer: Travis J. Harrison (tharrison@usgs.gov).


## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](https://www2.usgs.gov/visual-id/credit_usgs.html#copyright/).


This software has been approved for release by the U.S. Geological Survey (USGSW). Although the software has been subjected to rigorous review, the USGS reserves the right to update the software as needed pursuant to further analysis and review. No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. Furthermore, the software is released on condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from its authorized or unauthorized use."

This software is provided "AS IS".
