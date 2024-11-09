#' Calculate daily cpue by section, date (period, day_type), angler_final, and catch group (aka est_cg) using the Ratio-of-Means (rom) calculation
#'
#' @param days tibble with time strata and closure fields
#' @param dwg_summarized list with shared interview, index and census tibbles
#' @param angler_hours_daily_mean tibble of daily mean angler hours by event_date, angler_type, and section_num
#'
#' NOTE: the Ratio-of-Mean cpue calculation inherently weights angler groups with longer fishing times as opposed to Mean-of-Ratio (mor)
#' calculation which gives equal weight to every interview
#'
#' @return ??
#' @export
#'
prep_inputs_pe_daily_cpue_catch_est <- function(
    days,
    dwg_summarized,
    angler_hours_daily_mean
){

  dplyr::left_join(
    dwg_summarized$interview,
    days |> dplyr::select("event_date", "day_type", "period", "day_length"),
    by=c("event_date")
  ) |>
    dplyr::group_by(.data$section_num, .data$period, .data$day_type, .data$event_date, .data$angler_final, .data$est_cg) |>
    dplyr::summarise(
      n_obs = dplyr::n(),
      total_catch = sum(.data$fish_count),
      total_hours = sum(.data$fishing_time_total),
      cpue_rom_daily = .data$total_catch / .data$total_hours,
      .groups = "drop"
    ) |>
    dplyr::full_join(
      angler_hours_daily_mean,
      by = c("section_num", "period", "day_type", "event_date", "angler_final")
    ) |>
    tidyr::drop_na(.data$ang_hrs_daily_mean_TI_expan) |>
    dplyr::mutate(
      catch_estimate = round(.data$cpue_rom_daily * .data$ang_hrs_daily_mean_TI_expan, 3)
    ) |>
    dplyr::arrange(.data$section_num, .data$event_date, .data$angler_final, .data$est_cg)

}
