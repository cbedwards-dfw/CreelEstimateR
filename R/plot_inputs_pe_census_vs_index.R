
#' Plot paired season-long counts from census and index angler effort count surveys
#'
#' @param census_TI_expan ??
#'
#' @return ??
#' @export
#'
plot_inputs_pe_census_vs_index <- function(
    census_TI_expan
){
  census_TI_expan |>
    ggplot2::ggplot(ggplot2::aes(.data$count_index, .data$count_census, fill = .data$angler_final)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::scale_x_continuous(limits = c(0, NA)) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::geom_point(color = "black", pch = 21, size = 3.25) +
    ggplot2::labs(fill = "Angler type") +
    ggplot2::scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(linetype = 0))) +
    ggplot2::facet_wrap(~paste("Section:",.data$section_num), labeller = ggplot2::label_wrap_gen(multi_line = F))
}



