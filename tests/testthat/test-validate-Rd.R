test_that("validate_Rd validators work as expected", {

  pkg_path <- find.package("fishdictionary")
  
  if (!file.exists(file.path(pkg_path, "inst"))) {
    validate_Rd_path <- file.path(pkg_path, "extdata", "testthat_data")
  } else {
    validate_Rd_path <- file.path(pkg_path, "inst", "extdata", "testthat_data")
  }
  
  Rd_file <- file.path(validate_Rd_path, "validate_Rd_no_description.Rd")
  expect_error(validate_Rd(Rd_file), 
               regexp = "Description is not provided. This should be capitalized and not end in a period.")
  
  Rd_file <- file.path(validate_Rd_path, "validate_Rd_no_examples.Rd")
  expect_error(validate_Rd(Rd_file), 
               regexp = "Examples are not provided. Use NA if there is no input for examples.")
  
  Rd_file <- file.path(validate_Rd_path, "validate_Rd_no_rationale.Rd")
  expect_error(validate_Rd(Rd_file), 
               regexp = "Rationale is not provided.")
  
  Rd_file <- file.path(validate_Rd_path, "validate_Rd_no_alternatives.Rd")
  expect_error(validate_Rd(Rd_file), 
               regexp = "Synonym or similar terms are not provided. Use NA if there is no input for synonym or similar terms.")
  
  Rd_file <- file.path(validate_Rd_path, "validate_Rd_no_range_of_possible_values.Rd")
  expect_error(validate_Rd(Rd_file),
               regexp = "Range of possible values is not provided. Use NA if there is no input for range of possible values.")
  
  Rd_file <- file.path(validate_Rd_path, "validate_Rd_no_units.Rd")
  expect_error(validate_Rd(Rd_file),
               regexp = "Units are not provided. Use NA if there is no input for units.")
  
  Rd_file <- file.path(validate_Rd_path, "validate_Rd_good_input.Rd")
  expect_message(validate_Rd(Rd_file),
               regexp = paste("Term Biomass has passed all validations!"))
  
  Rd_file <- file.path(validate_Rd_path, "validate_Rd_missing_items.Rd")
  expect_error(validate_Rd(Rd_file))
})