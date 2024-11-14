#' creates summary table of census expansion factors based on values in data base
#'
#' NOTE: "p_census is the same as "p_TI" in the BSS model, which represent a known or assumed proportion of each section covered by tie-in counts for each final angler-type; this parameter is used to directly expand/modify the census count data prior to the estimate of the bias parameter/expansion factor
#' NOTE: values of "p_census/p_TI" are entered and stored in creel db
#' NOTE: currently, values of "p_census/p_TI" are limited to two "angler_final" groupings (bank, boat)
#'
#' @param eff data from dwg filtered using start & end dates passed from params
#'
#' @return ??
#' @export
#'
prep_dwg_census_expan <- function(eff){
  eff |>
    dplyr::filter(.data$location_type == "Section") |>
    dplyr::distinct(.data$section_num, .data$p_census_bank, .data$p_census_boat) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("p_census_"),
      names_prefix = "p_census_",
      names_to = "angler_final",
      values_to = "p_census"
    ) |>
    dplyr::mutate(
      p_census = tidyr::replace_na(.data$p_census, 1)
    ) |>
    dplyr::arrange(.data$angler_final, .data$section_num)

}
