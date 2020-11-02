context("Stratagies")
library(tibble)
library(dplyr)
library(heRomod2)

# Prepare data for test cases
stratategy_tests <- system.file("test_cases", "test_strategies.xlsx", package = "heRomod2") %>%
  read_workbook() 


test_that('duplicate strategy names are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::check_variables_df(stratategy_tests$dupe, context = "Strategies") ,
    "Strategies definition contained duplicate names for variables: \"chemo\"."
  )
})


test_that('wrong strategy names are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::check_variables_df(stratategy_tests$wrong_name, context = "Strategies") ,
    "Strategies definition contained invalid names for variables: \"1_chemo\". Names must start with a letter and contain only letters, numbers, and underscores."
  )
})


test_that('duplicate group names are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::check_variables_df(stratategy_tests$group_names_dupe, context = "Groups") ,
    "Groups definition contained duplicate names for variables: \"female_age_lt_35\"."
  )
})


test_that('wrong group names are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::check_variables_df(stratategy_tests$group_names_wrong, context = "Groups") ,
    "Groups definition contained invalid names for variables: \"female_age lt_35\". Names must start with a letter and contain only letters, numbers, and underscores."
  )
})


test_that('group-weights are numeric', {
  
  expect_error(
  heRomod2:::check_weights(stratategy_tests$groups_weights$weight,context='Groups'),
  "Groups weight has not numeric value: a."
  )
}
)
