library(testthat)
library(fishdictionary)
library(tools) # For test-validate-Rd.R

# Use "extdata" in "inst" because its loaded with R packages
validate_Rd_path <- system.file(file.path("extdata", "testthat_data"), package = "fishdictionary")

test_check("fishdictionary")