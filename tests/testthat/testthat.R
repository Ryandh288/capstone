library(testthat)
library(capstone)
context("Test functions from capstone package")

test_check("capstone")

load(system.file("extdata", "df.rda", package = "capstone"))

# Testing eq_clean_data and eq_location_clean
test_that("Cleaning data",{
  df_clean = eq_clean_data(df)
  expect_that(df_clean, is_a("data.frame"))
  expect_that(df_clean$DATE, is_a("Date"))
  expect_that(df_clean$LONGITUDE, is_a("numeric"))
  expect_that(df_clean$LATITUDE, is_a("numeric"))

  df_loc_clean = eq_location_clean(df_clean)
  expect_that(df_loc_clean, is_a("data.frame"))
})

# Testing geom_timeline and geom_timeline_label
test_that("Plotting timeline", {
  df_clean = eq_clean_data(df)
  expect_that(
    GeomTimeline,
    is_a("Geom")
  )
  expect_that(
    ggplot() + geom_timeline(data = df_clean, aes(x = DATE)),
    is_a("ggplot")
  )
})

# Testing geom_timeline_label
test_that("Plotting timeline with locations",{
  df_clean = eq_clean_data(df)
  expect_that(
    GeomTimeline,
    is_a("GeomTimelineLabel")
  )
  expect_that(
    ggplot() + geom_timeline_label(data = df_clean, aes(x = DATE, label = LOCATION_NAME)),
    is_a("ggplot")
  )
  expect_that(
    ggplot() + geom_timeline_label(data = df_clean, aes(x = DATE)),
    throws_error("geom_timeline_label requires the following missing aesthetics: label")
  )
})

# Testing eq_map and eq_create_label
test_that("Building a leaflet map", {
  df_clean = eq_clean_data(df)
  expect_that(
    df_clean %>%
      dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
      eq_map(annot_col = "DATE"),
    is_a("htmlwidget")
  )

  expect_that(eq_create_label(df_clean),
              is_identical_to(str_c(
                ifelse(is.na(df_clean$LOCATION_NAME), "", str_c("<b>Location: </b>", df_clean$LOCATION_NAME, "<br/>")),
                ifelse(is.na(df_clean$EQ_PRIMARY), "", str_c("<b>Magnitude: </b>", df_clean$EQ_PRIMARY, "<br/>")),
                ifelse(is.na(df_clean$TOTAL_DEATHS), "", str_c("<b>Total deaths: </b>", df_clean$TOTAL_DEATHS, "<br/>")))
              )
  )
  }
  )
