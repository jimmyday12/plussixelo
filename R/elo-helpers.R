#' Find the expected outcome given an ELO difference.
#'
#' \code{find_expected_outcome} returns the expected outcome of winning based on an ELO points difference.
#'
#' This is a generic ELO function in the form of prob = 1/1+10^(elo/M)
#' where elo is the elo difference between two teams and
#' M is the scaling factor for the elo difference (EXPAND)
#'
#' @param elo_difference Difference in elo ratings between two teams. Can be a positive or negative number.
#' @param M Test
#' @return A numeric value between 0 and 1, where values >0.5 indicate a win
#' while values of <0.5 indicate a loss.
#' A value of 0.5 indicates a draw
#'
#' @examples
#' find_expected_outcome(100, M = 400)
#' find_expected_outcome(0)
#'
#' \dontrun{
#' find_expected_outcome("a")
#' }
#' @export

find_expected_outcome <- function(elo_difference, M = 400) {
  # Error checks
  if (!is.numeric(elo_difference)) stop("elo_difference must be numeric")

  # Use traditional elo calculation
  expected_outcome <- 1 / (1 + (10 ^ (-elo_difference / M)))
  return(expected_outcome)
}


#' Helper function to process elo data into long format
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
find_expected_margin <- function(elo_difference, M = 400, B = 0.025) {
  # Traditinoal ELO equation for expected outcome
  # format is expected_outcome = 1/ 1+ 10^(elo_difference/M)
  # X is ELO Diff
  # M is scaling factor
  expected_outcome <- find_expected_outcome(elo_difference, M = M)

  # Now run existing map_margin_to_prob to outcome convert to margin
  # Find expected (predicted) Margin
  points <- -200:200
  points_norm <- map_margin_to_outcome(points, B = B) # create vector of results
  expected_margin <- points[which.min(abs(points_norm - expected_outcome))]
  return(expected_margin)
}

#' Find new season ELO score by applying carryover factor.
#'
#' \code{calculate_season_carryover} returns an ELO rating that is scaled towards the mean based on a carryover weight.
#'
#' INSERT DESCRIPTION
#'
#' @param elo An ELO rating, typically taken as the end of season value.
#' @param initial_team Rating given to the intial team. All values are scaled towards this value
#' @param weight A weighting for how much to regress the score towards `initial_team`.
#' A value of 1 would not regress the ELO rating at all
#' while a value of 0 would regress all the way to `initial_team`
#' @return A numeric value indicating the new ELO rating
#'
#' @examples
#' calculate_season_carryover(1600, initial_team = 1500, weight = 0.3)
#' calculate_season_carryover(1400, initial_team = 1550, weight = 0.5)
#'
#' \dontrun{
#' calculate_season_carryover(1650, weight = -10)
#' }
#' @export
calculate_season_carryover <- function(elo, initial_team = 1500, weight = 0.5) {
  # error checks
  if (!is.numeric(elo)) stop("elo must be numeric")
  if (!is.numeric(weight)) stop("weight must be numeric")
  if (!is.numeric(initial_team)) stop("initial_team must be numeric")
  if (weight < 0) stop("carryover_weight must be positive")
  if (weight > 1) stop("carryover_weight must be between 0 and 1, inclusive")



  new_elo <- (weight * elo) + (initial_team * (1 - weight))
  new_elo <- as.integer(floor(new_elo))
  return(new_elo)
}

#' Helper function to process elo data into long format
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
map_margin_to_outcome <- function(margin, A = 0, K = 1, B = 0.05, v = 1, Q = 1, C = 1) {
  # Generalised logistic function is in format
  # Y <- A + ((K-A) / ((C + (Q*exp(-B * X)))^(1/v)))
  numer <- K - A # create numerator
  denom <- C + (Q * exp(-B * margin)) # create denomenator
  divis <- numer / denom ^ (1 / v) # perform division
  actOut <- A + divis # add to A
  return(actOut)
}

#' Helper function to process elo data into long format
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
calculate_MOV <- function(elo_diff, margin, J = 2.2) {
  # performs a Margin of Victory Multiplier
  # this allows for scaling of new result depending on if fav won or not

  # First find if Fav won if so, make ELO_Fav positive
  if ((elo_diff * margin) > 0) {
    # then both are same sign, meaning fav won
    elo_fav <- elo_diff
  } else # otherwise, they are different so underdog won
  {
    elo_fav <- -elo_diff
  }
  mult <- J / ((0.001 * elo_diff) + J) # find multiplier
  MOV <- log(abs(margin) + 1) * mult # multiply by nat log of abs margin
  return(MOV)
}


#' Helper function to process elo data into long format
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
update_elo <- function(margin, elo_diff, MOV = 1, k = 20,
                       M = 400, B = 0.025) {

  # Error checks

  # Find the expected outcome
  expected_outcome <- find_expected_outcome(elo_diff, M = M)

  # First normalises actual Outcome between 0 and 1, slightly squashed so that
  # there are diminishing gains at higher levels.
  actual_outcome <- map_margin_to_outcome(margin)

  # Expected outcome is for home team. Away team is the negative of it, since
  # ELO is zero sum
  elo_change <- round((k * MOV * (actual_outcome - expected_outcome)))

  return(elo_change)
}

#' Helper function to process elo data into long format
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
convert_elo_results <- function(results) {

  # Convert results to wide format
  results_long <- results %>%
    mutate(Home.ELO = as.numeric(Home.ELO_post),
           Away.ELO = as.numeric(Away.ELO_post)) %>%
    select(Game:Away.Points, Home.ELO:Away.ELO, everything())  %>%
    gather(variable, value, Home.Team:Away.ELO) %>%
    separate(variable, into = c("Status", "variable")) %>%
    spread(variable, value) %>%
    arrange(Game) %>%
    mutate(Margin = ifelse(Status == "Home", Margin, Margin * -1)) %>%
    select(Game, Date, Season, Round, Round.Type, Round.Number, Venue, Team, Status, Goals, Behinds, Points, Margin, ELO)

  return(results_long)
}



