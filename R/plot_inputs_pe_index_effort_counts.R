#' Plot paired season-long counts from census and index angler effort count surveys
#'
#' Dev note: initial version started pipe with `dwg_summ$effort_index`, did not use argument.
#' I'm assuming argument is supposed to be used instead. May be wrong.
#'
#' @param effort_index ??
#'
#' @return ??
#' @export
#'
plot_inputs_pe_index_effort_counts <- function(
    effort_index
){
  effort_index |>
    dplyr::mutate(count_sequence = factor(.data$count_sequence)) |>
    ggplot2::ggplot(ggplot2::aes(.data$event_date, .data$count_index, fill = .data$count_sequence)) +
    #geom_point() + geom_text(aes(label = count_index), nudge_y = 1, check_overlap = T) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7)) +
    ggplot2::scale_x_date("", date_breaks = "7 days", date_labels =  "%m-%d") +
    ggplot2::scale_y_continuous("") +
    ggplot2::scale_color_brewer(palette = "Blues", aesthetics = c("fill")) +
    ggplot2::facet_wrap(~.data$section_num + .data$angler_final, scales = "fixed", ncol = 1, labeller = ggplot2::label_wrap_gen(multi_line = F))
}
