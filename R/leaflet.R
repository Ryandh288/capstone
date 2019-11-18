#' Mapping the earthquake epicenters

#' The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each point
#' with in pop up window containing annotation data stored in a column of the data frame.
#' The user should be able to choose which column is used for the annotation in the
#' pop-up with a function argument named annot_col.
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom magrittr "%>%"
#' @param df_clean,  data containing the filtered data frame with earthquakes
#' @param annot_col column that should be used for the annotation in the pop-up
#' @return This function returns a leaflet map with earthquake epicentres and annotations
#' within pop-up window
#' @examples
#' \dontrun{
#' df %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
#'   }
#' @export
eq_map <- function(df_clean, annot_col) {
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = df_clean, lng = df_clean$LONGITUDE, lat = df_clean$LATITUDE,
                     radius = as.numeric(df_clean$EQ_PRIMARY), popup = df_clean[[annot_col]],
                     stroke = FALSE, weight=1, fillOpacity = 0.3)
}

#' Create informative pop-ups for the interactive map created with the eq_map() function
#'
#' This function takes the dataset as an argument and creates an HTML label that
#' can be used as the annotation text in the leaflet map. This function should put
#' together a character string for each earthquake that will show the cleaned
#' location (as cleaned by the eq_location_clean() function created in Module 1),
#' the magnitude (EQ_PRIMARY), and the total number of deaths (TOTAL_DEATHS), with
#' boldface labels for each ("Location", "Total deaths", and "Magnitude"). If an
#' earthquake is missing values for any of these, both the label and the value should
#' be skipped for that element of the tag.
#'
#' @importFrom stringr str_c
#' @param df_clean, a dataframe obtained after runing functions eq_clean_data()
#'and eq_location_clean(). Initial raw data should be downloaded from NOAA website#'
#' @return This function creates HTML label with annotations
#' @examples
#' \dontrun{
#' df %>%
#'   dplyr::filter(COUNTRY == "RUSSIA" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#'   }
#' @export
eq_create_label<- function(df_clean) {
  str_c(
    ifelse(is.na(df_clean$LOCATION_NAME), "", str_c("<b>Location: </b>", df_clean$LOCATION_NAME, "<br/>")),
    ifelse(is.na(df_clean$EQ_PRIMARY), "", str_c("<b>Magnitude: </b>", df_clean$EQ_PRIMARY, "<br/>")),
    ifelse(is.na(df_clean$TOTAL_DEATHS), "", str_c("<b>Total deaths: </b>", df_clean$TOTAL_DEATHS, "<br/>")))
}