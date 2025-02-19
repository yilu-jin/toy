test_that("dm_site_core returns two data frames", {
  # Create an example ADSL input (a data frame)
  test_adsl <- data.frame(
    SITEID = c("S1", "S1", "S2", "S2", "S3"),
    COUNTRY = c("US", "US", "Canada", "Canada", "US"),
    PARTICIPANT_ID = c(1, 2, 3, 4, 5),
    POPFL = c(rep("Y", 4), ""),
    TRTP = c("TRT1", "TRT2", "TRT1", "TRT2", "TRT1")
  )

  result <- dm_site_core(
    population_from = test_adsl,
    idvar = "PARTICIPANT_ID",
    treatment_var = "TRTP"
  )

  # Check if the result is a list containing exactly two data frames
  expect_true(is.list(result))
  expect_length(result, 2)
  expect_equal(names(result), c("report.table.n", "report.table.p"))
  expect_true(is.data.frame(result$report.table.n))
  expect_true(is.data.frame(result$report.table.p))
})

test_that("The returned data frames have the correct columns", {
  # Create an example ADSL input (a data frame)
  test_adsl <- data.frame(
    SITEID = c("S1", "S1", "S2", "S2", "S3"),
    COUNTRY = c("US", "US", "Canada", "Canada", "US"),
    PARTICIPANT_ID = c(1, 2, 3, 4, 5),
    POPFL = c(rep("Y", 4), ""),
    TRTP = c("TRT1", "TRT2", "TRT1", "TRT2", "TRT1")
  )

  result <- dm_site_core(
    population_from = test_adsl,
    idvar = "PARTICIPANT_ID",
    treatment_var = "TRTP",
    treatment_order = c("TRT1", "TRT2"),
    site_var = "SITEID",
    country_var = "COUNTRY",
    level_col_name = TRUE
  )
  count_df <- result$report.table.n # Extract the count data frame
  percent_df <- result$report.table.p # Extract the percentage data frame

  # Check the count data frame columns
  expect_true("name" %in% colnames(count_df))
  expect_true("COUNTRY" %in% colnames(count_df))
  expect_true("SITEID" %in% colnames(count_df))
  expect_true("TRT1" %in% colnames(count_df))
  expect_true("TRT2" %in% colnames(count_df))

  # Check the percentage data frame columns
  expect_true("name" %in% colnames(percent_df))
  expect_true("COUNTRY" %in% colnames(percent_df))
  expect_true("SITEID" %in% colnames(percent_df))
  expect_true("TRT1" %in% colnames(percent_df))
  expect_true("TRT2" %in% colnames(percent_df))
})

test_that("Correct count values are returned", {
  # Create an example ADSL input (a data frame)
  test_adsl <- data.frame(
    SITEID = c("S1", "S1", "S2", "S2", "S3"),
    COUNTRY = c("US", "US", "Canada", "Canada", "US"),
    PARTICIPANT_ID = c(1, 2, 3, 4, 5),
    POPFL = c(rep("Y", 4), ""),
    TRTP = c("TRT1", "TRT2", "TRT1", "TRT2", "TRT1")
  )

  result <- dm_site_core(
    population_from = test_adsl,
    idvar = "PARTICIPANT_ID",
    treatment_var = "TRTP",
    treatment_order = c("TRT1", "TRT2"),
    site_var = "SITEID",
    country_var = "COUNTRY",
    level_col_name = TRUE,
    display_total_list = list("total" = c("TRT1", "TRT2"))
  )
  count_df <- result$report.table.n # Extract the count data frame

  # Expected result
  expected_result <- dplyr::tribble(
        ~name,      ~COUNTRY,       ~SITEID, ~TRT1,       ~TRT2, ~total,
    "headerN", NA_character_, NA_character_,     3,           2,     5,
     "Canada",      "Canada", NA_character_,     1,           1,     2,
       "  S2",      "Canada",          "S2",     1,           1,     2,
         "US",          "US", NA_character_,     2,           1,     3,
       "  S1",          "US",          "S1",     1,           1,     2,
       "  S3",          "US",          "S3",     1, NA_integer_,     1
  )

  # Check that the count for each combination of SITEID and COUNTRY is correct
  expect_equal(count_df, expected_result)

})

test_that("Correct percentage values are returned", {

})

# Edge case test: Empty input
test_that("dm_site_core handles empty ADSL input", {
  adsl_empty <- data.frame(
    SITEID = character(0),
    COUNTRY = character(0),
    PARTICIPANT_ID = numeric(0),
    TRTP = character(0)
    )

  result <- dm_site_core(
    population_from = adsl_empty,
    idvar = "PARTICIPANT_ID",
    treatment_var = "TRTP",
    site_var = "SITEID",
    country_var = "COUNTRY",
    level_col_name = TRUE
  )

  # Expect two empty data frames
  expect_equal(nrow(result$report.table.n), 0)
  expect_equal(nrow(result$report.table.p), 0)
})

test_that("dm_site_core handles NA values in the input", {
  # Create an example ADSL input with NA value
  adsl_with_na <- data.frame(
    SITEID = c("S1", "S1", "S2", NA, "S3"),
    COUNTRY = c("US", "US", "Canada", "Canada", NA),
    PARTICIPANT_ID = c(1, 2, 3, 4, 5),
    POPFL = c(rep("Y", 4), ""),
    TRTP = c("TRT1", "TRT2", NA, "TRT2", "TRT1")
  )

  result <- dm_site_core(
    population_from = adsl_with_na,
    idvar = "PARTICIPANT_ID",
    treatment_var = "TRTP",
    site_var = "SITEID",
    country_var = "COUNTRY",
    level_col_name = TRUE
  )

  # Check that missing values (NA) are handled correctly
  count_df <- result$report.table.n
  percent_df <- result$report.table.p

  # Ensure that NA values don't appear in the output
  expect_equal(count_df$TRT1[count_df$name == "headerN"], 1)
  expect_equal(count_df$TRT2[count_df$name == "headerN"], 1)
})


# MORE TESTING IDEAS THAT ARE AWAITING TESTING FOR OUR CASE..

# test_that("Function handles incorrect data types", {
#   adsl_invalid_data_type <- data.frame(
#     SITEID = c("S1", "S2", "S3"),
#     COUNTRY = c("US", "Canada", "US"),
#     PARTICIPANT_ID = c("A", "B", "C")  # Invalid participant IDs (should be numeric)
#   )
#
#   expect_error(calculate_participants_by_site_country(adsl_invalid_data_type),
#                "Error in calculate_participants_by_site_country: PARTICIPANT_ID should be numeric")
# })

# test_that("Function handles duplicate rows in the input", {
#   adsl_duplicates <- data.frame(
#     SITEID = c("S1", "S1", "S2", "S2", "S3", "S3"),
#     COUNTRY = c("US", "US", "Canada", "Canada", "US", "US"),
#     PARTICIPANT_ID = c(1, 1, 2, 2, 3, 3)  # Duplicated participant IDs
#   )
#
#   result <- calculate_participants_by_site_country(adsl_duplicates)
#
#   count_df <- result[[1]]
#   percentage_df <- result[[2]]
#
#   # Check that duplicates are handled correctly (they should be counted)
#   expect_equal(count_df$count[count_df$SITEID == "S1" & count_df$COUNTRY == "US"], 2)
# })

# test_that("Function handles large datasets", {
#   # Create a large dataset (e.g., 1 million rows)
#   adsl_large <- data.frame(
#     SITEID = rep(c("S1", "S2", "S3"), length.out = 1e6),
#     COUNTRY = rep(c("US", "Canada", "UK"), length.out = 1e6),
#     PARTICIPANT_ID = 1:1e6
#   )
#
#   result <- calculate_participants_by_site_country(adsl_large)
#
#   count_df <- result[[1]]
#   percentage_df <- result[[2]]
#
#   # Check that the output is not empty and has correct structure
#   expect_true(nrow(count_df) > 0)
#   expect_true(nrow(percentage_df) > 0)
# })

# test_that("Function handles non-standard or unexpected values", {
#   adsl_non_standard <- data.frame(
#     SITEID = c("S1", "S@1", "S2", "S3$"),
#     COUNTRY = c("US", "Canada", "US", "UK"),
#     PARTICIPANT_ID = 1:4
#   )
#
#   result <- calculate_participants_by_site_country(adsl_non_standard)
#
#   count_df <- result[[1]]
#   percentage_df <- result[[2]]
#
#   # Ensure no errors occur, and check if the function handles unusual site names
#   expect_equal(length(unique(count_df$SITEID)), 4)  # 4 unique site IDs
# })
