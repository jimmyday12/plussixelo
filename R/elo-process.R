#' X
#'
#' \code{process_historical_elo} x
#'
#' INSERT DESCRIPTION
#'
#' @param results x
#' @param HGA Home ground advantage, in arbitrary units
#' @param M Weighting factor for ELO calculations
#' @param stdev Standard Deviation of results
#' @return A dataframe of simulated results
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @export
process_historical_elo <- function(results, init_elo = 1500,
                                   HGA = 35, k = 20, M = 400,
                                   B = 0.025, carryover_weight = 0.6) {

  # Get initial team ELO
  teams <- distinct(results, Home.Team)
  team_elo <- data.frame(
    Team = teams$Home.Team,
    ELO = init_elo
  )

  # Run helper function, processed
  processed <- process_matches(
    results, team_elo = team_elo, type = "Historical",
    HGA = HGA, k = k, M = M, B = B,
    carryover_weight = carryover_weight,
    init_elo = init_elo
  )

  # Return
  return(processed)
}


#' Simulate a season based on ELO
#'
#' \code{simulated_season} takes a fixture and simulates remaining games based upon the teams starting ELO.
#'
#' INSERT DESCRIPTION
#'
#' @param fixture A dataframe containing upcoming matches EXPAND
#' @param team_elo A dataframe with each teams current ELO ratings
#' @param simulation An optional simulation ID number
#' @param HGA Home ground advantage, in arbitrary units
#' @param M Weighting factor for ELO calculations
#' @param stdev Standard Deviation of results
#' @return A dataframe of simulated results
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @export
simulate_season <- function(fixture, team_elo = data.frame(), simulation = 1,
                            stdev = 41, HGA = 35, k = 20, M = 400,
                            B = 0.025, carryover_weight = 0.6,
                            init_elo = 1500) {

  simulated_results <- process_matches(
    results, team_elo = team_elo, type = "Simulation",
    HGA = HGA, k = k, M = M, B = B,
    stdev = stdev,
    carryover_weight = carryover_weight,
    init_elo = 1500
  )

  simulated_results <- simulated_results %>%
    mutate(sim_number = simulation)

  return(simulated_results)
}

#' @importFrom magrittr %>%
#' @import dplyr
process_matches <- function(data, team_elo, type = "Historical",
                            stdev = 41, HGA = 35, k = 20, M = 400,
                            B = 0.025, carryover_weight = 0.6,
                            init_elo = 1500) {

  message("Starting to process data")
  # Start progress bar
  if(type == "Historical") pb <- progress_estimated(nrow(data))

  # Initialise a data frame
  # First, Add new columns to data
  data <- data %>%
    mutate(      Home.ELO = NA,
                 Away.ELO = NA,
                 Home.ELO_post = NA,
                 Away.ELO_post = NA,
                 exp_margin = NA,
                 exp_outcome = NA)

  # Step through each game
  for (i in seq_along(data$Game)) {
    if(type == "Historical") pb$tick()$print() # update the progress bar (tick())


    # get game details
    game <- data[i, ]

    # Get current elo
    home_elo <- team_elo$ELO[(team_elo$Team == data$Home.Team[i])]
    away_elo <- team_elo$ELO[(team_elo$Team == data$Away.Team[i])]

    if (data$Round.Number[i] == 1) {
      home_elo <- calculate_season_carryover(home_elo, initial_team = init_elo, weight = carryover_weight)
      away_elo <- calculate_season_carryover(away_elo, initial_team = init_elo, weight = carryover_weight)
    }

    # Calculate ELO Diff
    elo_diff <- home_elo + HGA - away_elo

    # Find expected outcome based on elo
    exp_margin <- find_expected_margin(elo_diff, M = M, B = B)
    exp_outcome <- find_expected_outcome(elo_diff, M = M)

    if (type == "Simulation") {
      # sample from rnorm of mean marg and historical SD
      data$margin[i] <- round(rnorm(1, exp_margin, sd = stdev))
    }

    # Find MOV multiplier
    MOV <- calculate_MOV(elo_diff, data$Margin[i])

    # Calculate ELO change
    elo_change <- update_elo(
      data$Margin[i], elo_diff, MOV = MOV,
      k = k, M = M, B = B
    )

    new_home_elo <- home_elo + elo_change
    new_away_elo <- away_elo - elo_change
    team_elo$ELO[(team_elo$Team == data$Home.Team[i])] <- new_home_elo
    team_elo$ELO[(team_elo$Team == data$Away.Team[i])] <- new_away_elo

    # Add new data to df
      data$Home.ELO[i] = home_elo
      data$Away.ELO[i] = away_elo
      data$Home.ELO_post[i] = new_home_elo
      data$Away.ELO_post[i] = new_away_elo
      data$exp_margin[i] = exp_margin
      data$exp_outcome[i] = exp_outcome
  }

  message("Finished Processing Data")
  return(data)
}
