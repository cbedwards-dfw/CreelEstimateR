#' Get bss effort
#'
#' Dev note: needs dwg as argument
#'
#' @param bss_fit ??
#' @param ecg ??
#'
#' @return ??
#' @export
#'
get_bss_effort_daily <- function(bss_fit, ecg){
  bss_fit |>
    summary(pars = c("E")) |>
    purrr::pluck("summary") |> #only want the combined-chains version
    as.data.frame() |>
    tibble::rownames_to_column("estimate_index") |>
    tibble::as_tibble() |>
    dplyr::mutate(indices = stringr::str_sub(.data$estimate_index, 3, 20) |>
                    stringr::str_remove("\\]")) |>
    tidyr::separate(
      col = "indices",
      into = c("section_num", "day_index", "angler_final")
    ) |>
    dplyr::mutate(
      dplyr::across(c("section_num", "day_index"), as.integer),
      angler_final = dplyr::if_else(.data$angler_final == "1", "bank", "boat"),
      est_cg = ecg,
      estimate = "E_daily"
    ) |>
    dplyr::left_join(dwg$days |>
                dplyr::select("event_date", "day_index", "week", "month"),
              by = "day_index") |>
    dplyr::relocate("estimate", "estimate_index", "est_cg", "day_index", "event_date", "week", "month", "section_num", "angler_final") |>
    dplyr::arrange(.data$event_date)
}
