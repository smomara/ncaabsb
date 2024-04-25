#' Calculate Adjusted Park Factor for an NCAA Baseball Team
#'
#' This function calculates the adjusted park factor for an NCAA baseball team for the year 2024, taking into account the
#' team's schedule, home game frequency, and the basic park factor calculated by the `base_park_factor` function. The adjusted
#' park factor provides a more nuanced understanding of how a team's home stadium influences game outcomes compared to league averages.
#'
#' @param team_id Numeric or character. The unique identifier for the team whose park factor is being calculated.
#'
#' @return A single numeric value representing the adjusted park factor for the team, accounting for the proportion of
#' home games and the deviation of the team's basic park factor from 1. The park factor is adjusted to mitigate the influence
#' of extreme values, aiming to provide a balanced metric that reflects the impact of the team's stadium on game results.
#'
#' @examples
#' adjusted_pf <- park_factor(124)
#'
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom baseballr ncaa_schedule_info
park_factor <- function(team_id) {
  schedule <- baseballr::ncaa_schedule_info(team_id, 2024)
  valid_games <- schedule[!is.na(schedule$contest_id), ]

  if (nrow(valid_games) == 0) {
    1
  }

  total_games <- nrow(valid_games)
  home_games <- sum(valid_games$home_team_id == team_id, na.rm = TRUE)

  base_park_factor_value <- base_park_factor(team_id)

  adjustment_factor <- abs(base_park_factor_value - 1) * (home_games / total_games)
  home_game_adj <- base_park_factor_value + adjustment_factor * ifelse(base_park_factor_value > 1, -1, 1)
  pf <- 1 - (1 - home_game_adj) * 0.6

  pf
}