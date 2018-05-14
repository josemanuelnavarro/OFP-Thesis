# OFP-Thesis
This is the R code that supports the basic Machine Learning model creation of the experiments of my doctoral dissertation. It is fairly simple, as `caret` provides a nice API that abstracts from most computations. I did not include the code to extract the Influence Matrix or its analysis nor the code to create models for my Networking data, as those data are protected by an NDA.

It contains two scripts: `lanl.R` and `lanlRF.R`. The former takes the raw data in csv format, processes it and stores it in rds format. After that, the latter creates and tests a Random Forest prediction model.
