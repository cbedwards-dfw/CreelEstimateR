#
# aggregating over day_type to simplify plot

#' Plot cpue by period, section_num, angler_final, and catch group
#'
#' aggregating over day_type to simplify plot
#'
#' @param days ??
#' @param dwg_summarized ??
#' @param daily_cpue_catch_est ??
#' @param est_catch_group ??
#' @param period_pe ??
#'
#' @return ??
#' @export
#'
plot_inputs_pe_cpue_period <- function(
    days, #tibble with time strata and closure fields
    dwg_summarized, #list with shared interview, index and census tibbles
    daily_cpue_catch_est,
    est_catch_group,
    period_pe
){

  cpue_period <-
    dplyr::left_join(
      dwg_summarized$interview,
      days |> dplyr::select("event_date", "day_type", "period"),
      by=c("event_date")
    ) |>
    dplyr::filter(.data$est_cg == est_catch_group) |>
    dplyr::group_by("section_num", "period", "day_type", "angler_final", "est_cg") |> #day_type dropped to simplify plot
    dplyr::summarise(
      n_obs = dplyr::n(),
      total_catch = sum(.data$fish_count),
      total_hours = sum(.data$fishing_time_total),
      cpue_rom_period = .data$total_catch / .data$total_hours,
      .groups = "drop"
    ) |>
    dplyr::left_join( # add back matching date information for stratum estimates
      dwg$days |>
        dplyr::select("event_date", "period", "year") |>
        dplyr::group_by("period") |>
        dplyr::summarise(
          min_event_date = min(.data$event_date),
          max_event_date = max(.data$event_date)),
      by = "period"
    )

  if(period_pe == "week"){
    cpue_period |>
      ggplot2::ggplot(ggplot2::aes(.data$min_event_date, .data$cpue_rom_period, fill = interaction(.data$day_type, .data$angler_final))) +
      ggplot2::geom_point(ggplot2::aes(fill = interaction(.data$day_type, .data$angler_final)), color = "black", pch = 21, size = 3.25) +
      ggplot2::scale_x_date(date_breaks = "1 week", labels = scales::date_format("%W"),
                   sec.axis = ggplot2::dup_axis(name = "", breaks = ggplot2::waiver(), labels = scales::date_format("%b"))) +
      ggplot2::ylab("Catch per unit effort (fish/hr)") +
      ggplot2::xlab("Date (week)") +
      ggplot2::labs(title = est_catch_group, fill = "Angler and day type groups") +
      ggplot2::scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      # geom_text(aes(label = n_obs), nudge_y = 0.02, color = "black", check_overlap = TRUE, size = 2.5) + #option to see sample size
      ggplot2::facet_wrap(~.data$section_num, scales = "fixed", ncol = 2, labeller = ggplot2::label_wrap_gen(multi_line = F))
  }
  else if(period_pe == "month"){

    cpue_period |>
      ggplot2::ggplot(ggplot2::aes(.data$min_event_date, .data$cpue_rom_period, fill = interaction(.data$day_type, .data$angler_final))) +
      ggplot2::geom_point(ggplot2::aes(fill = interaction(.data$day_type, .data$angler_final)), color = "black", pch = 21, size = 3.25) +
      ggplot2::scale_x_date(date_breaks = "1 month", labels = scales::date_format("%b")) +
      ggplot2::ylab("Catch per unit effort (fish/hr)") +
      ggplot2::xlab("Date (month)") +
      ggplot2::labs(title = est_catch_group, fill = "Angler and day type groups") +
      ggplot2::scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      # geom_text(aes(label = n_obs), nudge_y = 0.02, color = "black", check_overlap = TRUE, size = 2.5) + #option to see sample size
      ggplot2::facet_wrap(~.data$section_num, scales = "fixed", ncol = 2, labeller = ggplot2::label_wrap_gen(multi_line = F))

  }

  else if(period_pe == "duration"){
    cpue_period |>
      ggplot2::ggplot(ggplot2::aes(.data$min_event_date, .data$cpue_rom_period, fill = interaction(.data$day_type, .data$angler_final))) +
      ggplot2::geom_point(ggplot2::aes(fill = interaction(.data$day_type, .data$angler_final)), color = "black", pch = 21, size = 3.25) +
      ggplot2::scale_x_date() +
      ggplot2::ylab("Catch per unit effort (fish/hr)") +
      ggplot2::xlab("Date") +
      ggplot2::labs(title = est_catch_group, fill = "Angler and day type groups") +
      ggplot2::scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      # geom_text(aes(label = n_obs), nudge_y = 0.02, color = "black", check_overlap = TRUE, size = 2.5) + #option to see sample size
      ggplot2::facet_wrap(~.data$section_num, scales = "fixed", ncol = 2, labeller = ggplot2::label_wrap_gen(multi_line = F))
  }
}
