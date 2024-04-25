#' Retrieve and Process NCAA Baseball Team Statistics
#'
#' This function fetches and processes team statistics for an NCAA baseball team for the year 2024,
#' based on the team ID. It returns three dataframes: one with player information (`player_info`),
#' one with batting statistics (`batting_stats`), and one with pitching statistics (`pitching_stats`).
#' The function integrates park factor adjustments for batting statistics and offers detailed metrics 
#' such as wOBA, wRC+, and others for batting, as well as FIP, K/9, and BABIP for pitching.
#'
#' @param team_id Numeric or character. The unique identifier for the team whose statistics are being retrieved.
#'
#' @return A list of three dataframes: 
#'         - `player_info` contains common player information such as player ID, name, team, and division.
#'         - `batting_stats` includes batting metrics like games played, plate appearances, home runs,
#'           runs, RBIs, stolen bases, walk and strikeout percentages, ISO, BABIP, batting average,
#'           on-base percentage, slugging percentage, wOBA, and wRC+.
#'         - `pitching_stats` covers pitching metrics including games played, games started, innings pitched,
#'           strikeouts per 9 innings, walks per 9 innings, home runs per 9 innings, BABIP, ERA, and FIP.
#'         Each of the statistics dataframes also contains the player ID to link back to the `player_info` dataframe.
#'
#' @examples
#' stats <- team_stats(124)
#' player_info <- stats[[1]]
#' batting_stats <- stats[[2]]
#' pitching_stats <- stats[[3]]
#'
#' @export
#'
#' @importFrom dplyr %>% filter select mutate across left_join arrange rename distinct
#' @importFrom baseballr ncaa_team_player_stats
#' @importFrom tidyr replace_na

team_stats <- function(team_id) {
  
  guts <- load_guts()
  team_stats <- baseballr::ncaa_team_player_stats(team_id, 2024, "batting")

  player_info <- team_stats %>%
    filter(!grepl("Totals", player_name, ignore.case = TRUE)) %>%
    select(player_id, player_name, team_id, conference_id, division, Yr) %>%
    distinct() %>%
    rename(
      id = player_id,
      name = player_name,
      division_id = division,
      grade = Yr
    )
  
  park_factor <- park_factor(team_id)
  batting_stats <- team_stats %>%
    filter(!grepl("Totals", player_name, ignore.case = TRUE)) %>%
    select(-player_url) %>%
    mutate(across(!player_id, ~replace_na(.x, 0))) %>%
    mutate(across(c(AB, H, `2B`, `3B`, HR, BB, HBP, SF, SH), as.numeric)) %>%
    mutate(`1B` = H - `2B` - `3B` - HR, PA = AB + BB + HBP + SF + SH) %>%
    left_join(guts, by = c("division")) %>%
    mutate(woba = (wBB * BB + wHBP * HBP + w1B * `1B` + w2B * `2B` + w3B * `3B` + wHR * HR) / PA,
           wraa = (woba - lgwOBA) / wOBAScale * PA,
           pf = park_factor,
           wrc_plus = (((wraa / PA + `R.PA`) + (`R.PA` - pf * `R.PA`)) / `R.PA`) * 100,
           woba = round(woba, 3),
           wrc_plus = round(wrc_plus, 0),
           bb_percentage = round(BB / PA * 100, 1),
           k_percentage = round(K / PA * 100, 1),
           iso = round(SlgPct - BA, 3),
           babip = ifelse(AB - HR - K + SF == 0, 0, round((H - HR) / (AB - HR - K + SF), 3))) %>%
    select(c("player_id", "GP", "PA", "HR", "R", "RBI", "SB", "bb_percentage", "k_percentage", "iso", "babip", "BA", "OBPct", "SlgPct", "woba", "wrc_plus")) %>%
    rename(
      g = GP,
      pa = PA,
      hr = HR,
      r = R,
      rbi = RBI,
      sb = SB,
      avg = BA,
      obp = OBPct,
      slg = SlgPct
    )

  team_stats <- baseballr::ncaa_team_player_stats(team_id, 2024, "pitching")
  pitching_stats <- team_stats %>%
    filter(!grepl("Totals", player_name, ignore.case = TRUE)) %>%
    select(-player_url) %>%
    mutate(across(!player_id, ~replace_na(.x, 0))) %>%
    mutate(across(c(IP, SFA, BF, H, `2B-A`, `3B-A`, `HR-A`, BB, HB, SO), as.numeric)) %>%
    left_join(guts, by = c("division")) %>%
    mutate(IPx = floor(IP) + (IP - floor(IP)) * (10/3),
           fip = ifelse(IPx == 0, Inf, round((13 * `HR-A` + 3 * (BB + HB) - 2 * SO) / IPx + cFIP, 2)),
           k_per_9 = ifelse(SO == 0, 0, ifelse(IPx == 0, Inf, round(SO / IPx * 9, 2))),
           bb_per_9 = ifelse(BB == 0, 0, ifelse(IPx == 0, Inf, round(BB / IPx * 9, 2))),
           hr_per_9 = ifelse(`HR-A` == 0, 0, ifelse(IPx == 0, Inf, round(`HR-A` / IPx * 9, 2))),
           babip = round((H - `HR-A`) / (BF - SO - `HR-A` + SFA), 3),
           era = ifelse(IPx == 0, Inf, ERA)) %>%
    select(c("player_id", "GP", "GS", "IP", "k_per_9", "bb_per_9", "hr_per_9", "babip", "era", "fip")) %>%
    rename(
      g = GP,
      gs = GS,
      ip = IP
    )

  return(list(player_info, batting_stats, pitching_stats))
}