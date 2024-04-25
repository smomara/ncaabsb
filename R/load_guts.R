#' Load and Prepare Guts Data for NCAA Baseball Statistics
#'
#' This function downloads and processes a CSV file containing "guts" statistics for NCAA baseball. 
#' "Guts" stats provide a comprehensive overview of various baseball metrics. 
#' The data is specifically filtered for the 2023 season, and the `wOBA` column is renamed to `lgwOBA` for clarity.
#'
#' @return A data frame of the guts statistics for the 2023 season, with the season column removed and 
#' the `wOBA` column renamed to `lgwOBA`. The data includes various metrics that are essential for in-depth baseball analysis.
#'
#' @examples
#' guts_data <- load_guts()
#'
#' @export
#'
#' @importFrom dplyr rename filter select
#' @importFrom readr read_csv
load_guts <- function() {
  url <- "https://raw.githubusercontent.com/smomara/ncaa_bsb_stats_platform/main/ncaabsb/data/guts.csv"
  guts <- read.csv(url) %>%
    rename(lgwOBA = wOBA) %>%
    filter(season == 2023) %>%
    select(-season)
  guts
}