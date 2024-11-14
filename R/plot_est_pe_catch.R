#' plot PE catch estimates by catch group (est_cg), period (time stratum) and section
#'
#' Dev note: is estimates_pe$catch supposed to be estimates_pe_catch in all the code below? Seems like it should be.
#'
#' @param estimates_pe_catch ??
#' @param est_catch_group ??
#' @param period_pe ??
#' @param estimates_pe ??
#'
#' @return ??
#' @export
#'
plot_est_pe_catch <- function(
    estimates_pe_catch,
    est_catch_group,
    period_pe,
    estimates_pe
)

{
  if(period_pe == "week"){
    estimates_pe$catch |>
      dplyr::filter(.data$est_cg == est_catch_group) |>
      ggplot2::ggplot(ggplot2::aes(.data$min_event_date, .data$est, fill = interaction(.data$day_type, .data$angler_final))) +
      ggplot2::scale_x_date(date_breaks = "1 week", labels = scales::date_format("%W"),
                   sec.axis = ggplot2::dup_axis(name = "", breaks = ggplot2::waiver(), labels = scales::date_format("%b"))) +
      ggplot2::ylab("Catch") +
      ggplot2::xlab("Date (week)") +
      ggplot2::labs(fill = "Angler and day type groups") +
      ggplot2::geom_col(position = ggplot2::position_stack(), color = "black", width = 3.5) +
      ggplot2::scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      ggplot2::labs(title = est_catch_group, fill = "Angler and day type groups") +
      ggplot2::facet_wrap(~.data$section_num, scales = "fixed", labeller = ggplot2::label_wrap_gen(multi_line = F), ncol = 2)
  }
  else if(period_pe == "month"){
    estimates_pe$catch |>
      dplyr::filter(.data$est_cg == .data$est_catch_group) |>
      ggplot2::ggplot(ggplot2::aes(.data$min_event_date, .data$est, fill = interaction(.data$day_type, .data$angler_final))) +
      ggplot2::scale_x_date(date_breaks = "1 month", labels = scales::date_format("%b")) +
      ggplot2::ylab("Catch") +
      ggplot2::xlab("Date (month)") +
      ggplot2::labs(fill = "Angler and day type groups") +
      ggplot2::geom_col(position = ggplot2::position_stack(), color = "black", width = 12) +
      ggplot2::scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      ggplot2::labs(title = est_catch_group, fill = "Angler and day type groups") +
      ggplot2::facet_wrap(~.data$section_num, scales = "fixed", labeller = ggplot2::label_wrap_gen(multi_line = F), ncol = 2)

  }

  else if(period_pe == "duration"){
    estimates_pe$catch |>
      dplyr::filter(.data$est_cg == .data$est_catch_group) |>
      ggplot2::ggplot(ggplot2::aes(.data$min_event_date, .data$est, fill = interaction(.data$day_type, .data$angler_final))) +
      ggplot2::scale_x_date() +
      ggplot2::ylab("Catch") +
      ggplot2::xlab("Date") +
      ggplot2::labs(fill = "Angler and day type groups") +
      ggplot2::geom_col(position = ggplot2::position_stack(), color = "black", width = 12) +
      ggplot2::scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
      ggplot2::labs(title = est_catch_group, fill = "Angler and day type groups") +
      ggplot2::facet_wrap(~.data$section_num, scales = "fixed", labeller = ggplot2::label_wrap_gen(multi_line = F), ncol = 2)
  }
}
