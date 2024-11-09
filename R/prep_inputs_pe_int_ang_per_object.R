#' calculate the number of objects (e.g., trailers, vehicles, boats) each angler group brought to the fishery based on study_design used
#'
#' Devnote: we use summarize in the below. I assume it should be dplyr:: summarize?
#'
#' @param dwg_summarized list with shared interview, index and census tibbles
#' @param study_design parameter specifying study design and which if/else loop gets called below
#' @param ...
#'
#' @return ??
#' @export
#'
prep_inputs_pe_int_ang_per_object <- function(
    dwg_summarized,
    study_design
    ){

if(stringr::str_detect(study_design, "tandard" )){

  dplyr::bind_rows(
    dwg_summarized$interview |>
      dplyr::summarize(
        angler_final = "total",
        count_type = "vehicle",
        person_count_total = sum(.data$person_count_final),
        object_count_total = sum(.data$vehicle_count),
        ang_per_object = sum(.data$person_count_final) / sum(.data$vehicle_count), .groups = "drop"
      )
    ,
    dwg_summarized$interview |>
      dplyr::filter(.data$angler_final == "boat") |>
      dplyr::summarize(
        angler_final = "boat",
        count_type = "trailer",
        person_count_total = sum(.data$person_count_final),
        object_count_total = sum(.data$trailer_count),
        ang_per_object = sum(.data$person_count_final) / sum(.data$trailer_count), .groups = "drop"
      )
  )

}else if(study_design == "Drano"){

# List of "angler_final" that are in effort_index dataset but missing from interview dataset (in theory, this should not happen but hypothetically could)
  missing_angler_final_int<-
    dplyr::anti_join(
      dwg_summarized$effort_index |>
        dplyr::distinct(.data$angler_final)
      ,
      dwg_summarized$interview |>
        dplyr::distinct(.data$angler_final)
      , by = "angler_final"
    ) |>
    dplyr::pull()

  #KB NOTE: study design did not require a "boat_count" (number of boats for a given interview group); so here I'm simply dividing the person count by the number of interviews which implicitly assumes 1 boat per interviewed group
  dplyr::bind_rows(
    dwg_summarized$interview |>
    dplyr::group_by(.data$angler_final) |>
    dplyr::summarize(
      count_type = "boats",
      person_count_total = sum(.data$person_count_final),
      groups_interviewed = dplyr::n(),
      ang_per_object = sum(.data$person_count_final) / dplyr::n(), .groups = "drop"
    )
  )
}
}
