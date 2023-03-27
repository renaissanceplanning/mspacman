
# Productivity (derivative of complete trips curve) ============================

#' Calculate trip productivity
#'
#' "Productivity" is defined as the first derivative of the complete trips
#' curve at time \eqn{t}
#'
#' @param times A vector of times specified in minutes (and therefore between
#' 0 and 1440, inclusive)
#' @param complete_trips_curve The complete trips curve on which to assess
#' productivity
#'
#' @return A vector of productivities for each time in `times`
#'
#' @export
productivity = function(times, complete_trips_curve){
  # Check times, and convert to 12:00pm-12:00pm to guarantee continuity
  if(any(times < 0 | times > 1440)){
    stop("at least one time in `times` is improperly specified")
  }
  times[times < 720] = times[times < 720] + 720
  # Take productivity as the derivative of the complete trips curve
  pdt = complete_trips_curve(times, deriv=1)
  return(pdt)
}

# Efficiency (expected observed completed trips in interval) ===================

#' Calculate trip efficiency
#'
#' "Efficiency" is defined as the expected number of completed trips observed
#' (or share of completed trips observed) at a random point in a specified
#' interval. That is to say, for a random time \eqn{t: t1 <= t <= t2}, how many
#' completed trips (or what share of total trips completed) would we expect to
#' have observed?
#'
#' @param time_lowers Lower bound for time intervals; should align with indices
#' of `time_uppers`.
#' @param time_uppers Upper bound for time intervals; should align with indices
#' of `time_lowers`
#' @param complete_trips_curve The complete trips curve on which to assess
#' efficiency
#'
#' @return A vector of efficiencies for each interval implied by `time_lowers`
#' and `time_uppers`
#'
#' @export
efficiency = function(time_lowers, time_uppers, complete_trips_curve){
  # Take efficiency as expected value of completed trips curve over the interval
  # (treating the initial value as "0" so we get newly completed trips)
  full_integral = purrr::map2(time_lowers, time_uppers, function(l,u){
    integrate(complete_trips_curve, l, u)$value
  }) %>% unlist()
  interval_width = time_uppers - time_lowers
  zero_reset = complete_trips_curve(time_lowers) * interval_width
  eff = (full_integral - zero_reset) / interval_width
  return(eff)
}
