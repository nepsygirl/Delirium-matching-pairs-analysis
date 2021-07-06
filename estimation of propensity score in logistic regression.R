#CODE FOR TREATMENT OF MISSING DATA BY MULTIPLE IMPUTATION
library(mice)  ###load package that performs multiple imputation by chained equations


#examine the number of missing data 
missing.indicator <- data.frame(is.na(POD_dataset))
propMissing <- apply(missing.indicator,2,mean)


###create dummy missing value indicators 
names(mis)