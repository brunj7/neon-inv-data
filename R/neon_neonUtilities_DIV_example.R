#######################################################################################
### The neonUtilities key functions

library(neonUtilities)
library(tidyverse)
### Use getProductInfo() to query via the API to determine product availability by site, product abstract, design description, etc.
divInfo <- getProductInfo(dpID = "DP1.10058.001")

# Determine availability by site
availDivInfo <- divInfo$siteCodes



### Use loadByProduct() to retrieve NEON plant presence and percent cover data via the API; use NEON website to find dpID for product of interest.
demoDiv <- loadByProduct(dpID = "DP1.10058.001", site = "CPER", check.size = FALSE)

# Extract second list object to get 1 m2 cover data
coverDiv <- demoDiv[[2]]
coverDiv <- demoDiv$div_1m2Data # This also works to do the same as above

# Filter to find introduced species at selected site
introCPER <- coverDiv %>% filter(nativeStatusCode == "I")

# Determine mean percent cover by nativeStatusCode by plotID for selected site
sumCoverDiv <- coverDiv %>% group_by(plotID, nativeStatusCode) %>% summarise(mean = mean(percentCover))

