#' Clean NOA Significant Earthquaqe data
#'
#' This function returns a clean data frame. Preprocessing includes:
#' date column obtained as a resulted of mergind YEAR, MONTH and DAY columns;
#' LATITUDE and LONGITUDE columns converted to numeric class
#'
#' @importFrom readr read_tsv
#' @importFrom dplyr mutate select
#' @importFrom stringr str_c
#' @importFrom lubridate ymd years
#'
#' @param df NOAA Significant Earthquake dataframe
#'
#' @return This function returns a data frame with preprocessed columns: date, longitude
#' and latitude
#'
#' @examples
#' \dontrun{
#' df = eq_clean_data(df)
#'}
#' @export
eq_clean_data = function(df) {
  df = mutate(df,
              MONTH = ifelse(is.na(MONTH), "01", MONTH),
              DAY = ifelse(is.na(DAY), "01", DAY),
              LATITUDE = as.numeric(LATITUDE),
              LONGITUDE = as.numeric(LONGITUDE),
              DATE = ifelse(YEAR >= 0, str_c(YEAR, MONTH, DAY, sep = "-"), NA) %>% as.Date())
  
  # dealing with negative years
  df[df$YEAR<0,] = df[df$YEAR<0,] %>% mutate(
    YEAR2 = YEAR * (-1),
    DATE = lubridate::ymd(str_c("0000", MONTH, DAY, sep = "-")) - lubridate::years(YEAR2)) %>%
    select(-YEAR2)
  
  return(df)
}


#' Clean location names for plotting
#'
#' This function returns a clean data frame.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace str_to_title
#' @importFrom magrittr "%>%"
#'
#' @param df NOAA Significant Earthquake dataframe
#'
#' @return This function returns a data frame with cleaned location names
#'
#' @examples
#' \dontrun{
#' df = eq_location_clean(df)
#'}
#' @export
eq_location_clean = function(df){
  df = mutate(df, LOCATION_NAME = LOCATION_NAME %>% str_replace(".*: *", "") %>% str_to_title())
  return(df)
}