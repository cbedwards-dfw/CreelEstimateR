#' Calculate degrees of freedom based on number of creel survey days
#'
#' @param angler_hours_daily_mean ??
#'
#' @return ??
#' @export
#'
prep_inputs_pe_df <- function(
    angler_hours_daily_mean
){
  angler_hours_daily_mean |>  #KB addition
    dplyr::count(.data$section_num, .data$period, .data$day_type, .data$angler_final, name = "n_days_samp") |>
    dplyr::group_by(.data$section_num, .data$angler_final) |>
    dplyr::mutate(
      df = (min(.data$n_days_samp - 1) + sum(.data$n_days_samp))/2
      ) |>
    dplyr::ungroup()
}
