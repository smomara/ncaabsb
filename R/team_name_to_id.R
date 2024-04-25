#' Retrieve the School ID of an NCAA Baseball Team by Team Name
#'
#' This function queries a dataset of NCAA baseball teams to find the school name associated with a specific team ID. 
#' It relies on the `load_teams` function to fetch the current list of teams, filtering this list to match the specified 
#' team ID and extracting the school name.
#'
#' @param name Character. The name of the team whose id is being retreived.
#'
#' @return A number representing the school id of the specified team. If no match is found, the function 
#' returns `NULL`.
#'
#' @examples
#' # Assuming '12345' is a valid team_id
#' school_name <- team_name(12345)
#'
#' @export
#'
#' @importFrom dplyr filter select pull
team_name_to_id <- function(name) {
  team_id <- load_teams() %>%
    filter(team_name == name) %>%
    select(team_id) %>%
    pull()
  team_id
}