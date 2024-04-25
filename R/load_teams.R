#' Load NCAA Baseball Teams Data for 2024
#'
#' This function retrieves data on NCAA baseball teams for the year 2024. It makes use of the `baseballr` package
#' to fetch the teams' information and then filters the dataset to include only teams for the specified year.
#'
#' @return A data frame containing information about NCAA baseball teams for the year 2024. The data frame includes
#' various details about each team, tailored to the 2024 season.
#'
#' @examples
#' teams_2024 <- load_teams()
#'
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom baseballr load_ncaa_baseball_teams
load_teams <- function() {
  url <- "https://raw.githubusercontent.com/smomara/ncaa_bsb_stats_platform/main/ncaabsb/data/teams.csv"
  teams <- read.csv(url)
  teams
}