#' Aggregates index effort counts over locations within count_seq & section based on study_design and user input values for boat_type_collapse, fish_location_determines_type, angler_type_kayak_pontoon
#'
#'  Note dplyr::summarize() does not account for missed locations with date-section-sequence
#'
#' @param eff  effort data from dwg filtered using start & end dates passed from params
#' @param study_design  string passed from params denoting which study design was followed during data collection
#' @param boat_type_collapse string passed from params that controls whether all (potential) boat types (e.g., motor_boat, drift_boat) are collapsed (i.e., boat_type_collapse: "Yes") into a single boat type or kept separate (boat_type_collapse: "No").
#' @param fish_location_determines_type string passed from params that controls whether the observed fishing location for a given angler group during an effort count determines their angler type.
#' @param angler_type_kayak_pontoon  string passed from params that controls whether a boat designated as a kayak, pontoon, or kick during an effort count or angler group interview should be designated as a boat or bank angler.
#'
#' @return ?
#' @export
#'
prep_dwg_effort_index <- function(
    eff,
    study_design,
    boat_type_collapse = NA,
    fish_location_determines_type = NA,
    angler_type_kayak_pontoon = NA){

  #create intermediate object index_angler_groups that converts count_type objects to angler_final
  if(stringr::str_detect(study_design, "tandard" )){

    index_angler_groups <-
      eff |>
      dplyr::filter(
        .data$tie_in_indicator == 0,
        is.na(.data$no_count_reason),
        !is.na(.data$count_type)
      ) |>
      dplyr::mutate(
        angler_final =
          dplyr::case_when(
            .data$count_type == "Trailers Only" ~ "boat",
            .data$count_type == "Vehicle Only" ~ "total",
            TRUE ~ "fail"
          )
      )

  }else if(study_design == "Drano"){

    index_angler_groups<-
      eff |>
      dplyr::filter(
        .data$tie_in_indicator == 0,
        is.na(.data$no_count_reason),
        !is.na(.data$count_type)
      ) |>
      dplyr::mutate(
        angler_final =
          dplyr::case_when(

            stringr::str_detect(.data$count_type, "Shore") ~ "bank",

            stringr::str_detect(.data$count_type, "Motor") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
            stringr::str_detect(.data$count_type, "Motor") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_motor",

            stringr::str_detect(.data$count_type, "Skiff|Pram") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
            stringr::str_detect(.data$count_type, "Skiff|Pram") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_skiff",

            stringr::str_detect(.data$count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
            stringr::str_detect(.data$count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_single",
            stringr::str_detect(.data$count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "ank")  ~ "bank",
            TRUE ~ "fail"
          )
      )
  }
  # create final object output of interest index_angler_final that summarizes index count data by section_num, event_date, count_sequence, & angler_final
  index_angler_final<-
    index_angler_groups |>
    dplyr::group_by(.data$section_num, .data$event_date, .data$count_sequence, .data$angler_final) |> #
    dplyr::summarise(count_index = sum(.data$count_quantity), .groups = "drop") |>
    dplyr::arrange(.data$section_num, .data$event_date, .data$count_sequence) |>
    dplyr::mutate(
      fishery_name = params$fishery_name # add back fishery_name
      , angler_final_int = as.integer(factor(.data$angler_final))
    ) |>
    dplyr::relocate("fishery_name")

  return(list(index_angler_groups = index_angler_groups,
              index_angler_final = index_angler_final))

}
