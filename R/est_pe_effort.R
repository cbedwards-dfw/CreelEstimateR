#' Estimate effort with PE method
#'
#' Performs point estimate calculation of effort after prep functions. Calculate mean and variance for section_num-period-day_type-angler_final. This is the finest stratification above individual days.
#'
#' @details sample size is inherently small for a day_type within week stratification
#' -the var() and sd() functions return NA when passed a length-1 vector (single obs)
#' -variance has limited meaning even when n=3, e.g. if sampling Fri & Sat & Sun
#' BUT sample design itself (and first principles) stratify on day_type
#' such that pooling over weekend/weekday is counter to data collection protocol/design
#' could pool over weeks (and perhaps angler_final) if a better variance is desired over the fishery duration
#'
#' @param days tibble of dates and corresponding fields (e.g., year, month, period, open/close fishery by section)
#' @param pe_inputs_list ??
#' @param sections ??
#' @param params description
#'
#' @return ??
#' @export

est_pe_effort <- function(days, pe_inputs_list, sections, params) {
  est_effort <-
    dplyr::left_join(
      #dates expanded to sections * angler_final * opendays
      days |>
        dplyr::select(
          "period",
          "day_type",
          "event_date",
          dplyr::starts_with("open_section")
        ) |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("open_section"),
          names_to = "section_num",
          values_to = "is_open"
        ) |>
        dplyr::filter(.data$is_open) |>
        dplyr::mutate(
          section_num = as.numeric(gsub("^.*_", "", .data$section_num)),
          is_open = NULL,
          angler_final = list(unique(
            pe_inputs_list$ang_hrs_daily_mean$angler_final
          ))
        ) |>
        tidyr::unnest("angler_final")
      ,
      #estimates of fishing_time possible to calculate for sampled dates-sections-angler_final
      pe_inputs_list$ang_hrs_daily_mean |>
        dplyr::select(
          "section_num",
          "event_date",
          "angler_final",
          "ang_hrs_daily_mean_TI_expan"
        )
      ,
      by = c("section_num", "event_date", "angler_final")
    ) |>
    dplyr::group_by(.data$section_num, .data$period, .data$day_type, .data$angler_final) |>
    dplyr::summarize(
      n_obs = sum(!is.na(.data$ang_hrs_daily_mean_TI_expan)),
      dplyr::across(
        .cols = c("ang_hrs_daily_mean_TI_expan"),
        .fns = list(
          mean = ~ mean(.x, na.rm = T),
          var = ~ var(.x, na.rm = T)
        ),
        #sd = ~sd(.x, na.rm = T), med = ~median(.x, na.rm=T),
        .names = "ang_hrs_{.fn}"
      ),
      .groups = "drop"
    ) |>
    dplyr::right_join(pe_inputs_list$days_total
                      ,
                      by = c("section_num", "period", "day_type")) |>
    #!!not sure this is correct - could/should recalc df for within-week/month?
    dplyr::left_join(
      pe_inputs_list$df |>
        dplyr::distinct(.data$section_num, .data$angler_final, .data$df),
      by = c("section_num", "angler_final")
    ) |>
    #!!this carries forward the orig variance eqns but not sure if
    #!!1) eqns are correctly implemented or
    #!!2) eqns are meaningful relative to the ang_hrs_var already present from base var()
    #!! i.e., the 3rd term "adjustment coef" in the first case
    #!! acts to reduce the first 2 terms' computed "variance", and is asymptotic to 0 for complete sampling
    #!! such that case logic prevents 0 total_effort_var at n_obs==N_days_open
    dplyr::mutate(
      project_name = params$project_name,
      # add back metadata to estimates
      fishery_name = params$fishery_name,
      # add back metadata to estimates
      ang_hrs_var = tidyr::replace_na(.data$ang_hrs_var, 0),
      est = .data$N_days_open * .data$ang_hrs_mean,
      var = dplyr::if_else(
        .data$n_obs < .data$N_days_open,
        (.data$N_days_open ^ 2) * (.data$ang_hrs_var / .data$n_obs) * (1 -
                                                                         (.data$n_obs / .data$N_days_open)),
        (.data$N_days_open ^ 2) * (.data$ang_hrs_var / .data$n_obs)
      ),
      l95 = .data$est - stats::qt(1 - (0.05 / 2), .data$df) * (.data$var ^
                                                                 0.5),
      u95 = .data$est + stats::qt(1 - (0.05 / 2), .data$df) * (.data$var ^
                                                                 0.5)
    ) |>
    dplyr::left_join(
      # add back matching date information for stratum estimates
      days |>
        dplyr::select("event_date", "period", "year") |>
        dplyr::group_by(.data$period) |>
        dplyr::summarise(
          min_event_date = min(.data$event_date),
          max_event_date = max(.data$event_date)
        ),
      by = "period"
    ) |>
    dplyr::relocate("project_name", "fishery_name") |>
    dplyr::relocate(c("min_event_date", "max_event_date"), .before = "period")

  return(est_effort)
}
