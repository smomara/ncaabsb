#' Calculate the 5-Year Base Park Factor for an NCAA Baseball Team's Home Stadium
#' 
#' @param team_id Numeric or character. The unique identifier for the team whose park factor is being calculated.
#' 
#' @return A single numeric value representing the 5-year base park factor for
#' the team's home stadium
#' 
#' @examples 
#' base_park_factor(124)
#' 
#'  @export
#' 
#' @importFrom dplyr filter mutate case_when
#' @importFrom baseballr ncaa_schedule_info
base_park_factor <- function(team_id) {
  home_games <- 0
  away_games <- 0
  home_runs <- 0
  away_runs <- 0
  
  for (i in 1:5) {
    schedule <- baseballr::ncaa_schedule_info(team_id, 2024 - i)

    if (nrow(schedule) == 0 || ncol(schedule) == 0) {
      next
    }

    schedule <- schedule  %>%
      filter(!is.na(contest_id)) %>%
      mutate(game_type = case_when(
        home_team_id == team_id & is.na(neutral_site) ~ "home",
        TRUE ~ "away"
      ))
    
    home_games <- home_games + sum(schedule$game_type == "home", na.rm = TRUE)
    away_games <- away_games + sum(schedule$game_type == "away", na.rm = TRUE)
    
    home_runs <- home_runs + sum(schedule$home_team_score[schedule$game_type == "home"], na.rm = TRUE) + 
      sum(schedule$away_team_score[schedule$game_type == "home"], na.rm = TRUE)
    away_runs <- away_runs + sum(schedule$home_team_score[schedule$game_type == "away"], na.rm = TRUE) + 
      sum(schedule$away_team_score[schedule$game_type == "away"], na.rm = TRUE)
  }

  if (away_games == 0 || away_runs == 0) {
    return(1.00)
  } else {
    base_park_factor <- (home_runs / home_games) / (away_runs / away_games)
    return(base_park_factor)
  }
}