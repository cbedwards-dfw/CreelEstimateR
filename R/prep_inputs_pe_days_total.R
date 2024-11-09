#' Calculate total days the fishery was open per strata (strata = period, day_type, and section_num)
#'
#' @param days tibble of dates and corresponding fields (e.g., year, month, period, open/close fishery by section)
#'
#' @return ?
#' @export
#'
prep_inputs_pe_days_total <- function(
    days
){

  days |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("open_section"),
      names_to = "section_temp",
      values_to = "is_open") |>
    dplyr::filter(.data$is_open) |>
    dplyr::mutate(section_num = as.numeric(gsub("^.*_", "", .data$section_temp))) |>
    dplyr::count(.data$period, .data$day_type, .data$section_num, name = "N_days_open")
}
