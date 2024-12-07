#' Estimate catch with PE method
#'
#'Performs point estimate calculation of catch after prep_dwg_ and prep_inputs_pe_ functions.
#'
#' @param days  tibble of dates and corresponding fields (e.g., year, month, period, open/close fishery by section)
#' @param pe_inputs_list Help!
#' @param params description
#' @param dwg description
#'
#' @return Catch estimates from PE method
#' @export
#'
est_pe_catch <- function(
    days,
    pe_inputs_list,
    params,
    dwg
){
  est_catch <-
    dplyr::left_join(
      #dates expanded to sections * angler_final * opendays
      days |>
        dplyr::select("period", "day_type", "event_date", dplyr::starts_with("open_section")) |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("open_section"),
          names_to = "section_num",
          values_to = "is_open"
        ) |>
        dplyr::filter(.data$is_open) |>
        dplyr::mutate(
          section_num = as.numeric(gsub("^.*_", "", .data$section_num)),
          is_open = NULL,
          angler_final = list(unique(pe_inputs_list$ang_hrs_daily_mean$angler_final))
        ) |>
        tidyr::unnest("angler_final")
      ,
      pe_inputs_list$daily_cpue_catch_est |>
        dplyr::select("section_num", "event_date", "angler_final", "est_cg", "catch_estimate")
      ,
      by = c("section_num", "event_date", "angler_final")
    ) |>
    dplyr::group_by(.data$section_num, .data$period, .data$day_type, .data$angler_final, .data$est_cg) |>
    dplyr::summarize(
      n_obs = sum(!is.na(.data$catch_estimate)),
      dplyr::across(
        .cols = c("catch_estimate"),
        .fns = list(
          mean = ~mean(.x, na.rm = T),
          var = ~var(.x, na.rm = T)
        ), #sd = ~sd(.x, na.rm = T), med = ~median(.x, na.rm=T),
        .names = "catch_est_{.fn}"
      ),
      .groups = "drop"
    ) |>
    dplyr::right_join(
      pe_inputs_list$days_total
      ,
      by = c("section_num", "period", "day_type")
    ) |>
    #!!not sure this is correct - could/should recalc df for within-week/month?
    dplyr::left_join(
      pe_inputs_list$df |>
        dplyr::distinct(.data$section_num, .data$angler_final, .data$df)
      ,
      by = c("section_num", "angler_final")
    ) |>
    dplyr::mutate(
      project_name = params$project_name, # add back metadata to estimates
      fishery_name = params$fishery_name, # add back metadata to estimates
      catch_est_var = tidyr::replace_na(.data$catch_est_var, 0),
      est = .data$N_days_open * .data$catch_est_mean,
      var = dplyr::if_else(
        .data$n_obs < .data$N_days_open,
        (.data$N_days_open^2) * (.data$catch_est_var / .data$n_obs) * (1-(.data$n_obs/.data$N_days_open)),
        (.data$N_days_open^2) * (.data$catch_est_var / .data$n_obs)
      ),
      l95 = .data$est - stats::qt(1-(0.05/2), .data$df)*(.data$var^0.5),
      u95 = .data$est + stats::qt(1-(0.05/2), .data$df)*(.data$var^0.5)
    ) |>
    dplyr::left_join( # add back matching date information for stratum estimates
      dwg$days |>
        dplyr::select(tidyselect::all_of(c("event_date", "period", "year"))) |>
        dplyr::group_by(.data$period) |>
        dplyr::summarise(
          min_event_date = min(.data$event_date),
          max_event_date = max(.data$event_date)),
      by = "period") |>
    tidyr::drop_na("est_cg") |>
    dplyr::relocate("project_name", "fishery_name") |>
    dplyr::relocate(c("min_event_date", "max_event_date"), .before = "period")

  return(est_catch)
}
