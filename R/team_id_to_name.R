#' Retrieve the School Name of an NCAA Baseball Team by Team ID
#'
#' This function queries a dataset of NCAA baseball teams to find the school name associated with a specific team ID. 
#' It relies on the `load_teams` function to fetch the current list of teams, filtering this list to match the specified 
#' team ID and extracting the school name.
#'
#' @param id Numeric or character. The unique identifier for the team whose name is being retrieved.
#'
#' @return A character string representing the school name of the specified team. If no match is found, the function 
#' returns `NULL`.
#'
#' @examples
#' # Assuming '12345' is a valid team_id
#' school_name <- team_name(12345)
#'
#' @export
#'
#' @importFrom dplyr filter select pull
team_id_to_name <- function(id) {
  team_name <- load_teams() %>%
    filter(team_id == id) %>%
    select(team_name) %>%
    pull()
  team_name
}