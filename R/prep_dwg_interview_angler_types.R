#' Create angler_final field based on study_design designation and user input values for boat_type_collapse, fish_location_determines_type, angler_type_kayak_pontoon
#'
#'  Dev note: is study_design option supposed to be "standard" or "tandard"? Or are we using str_detect to support multiple capitalizations?
#'
#' @param interview_fishing_time output from preceding function that calculates fishing time of angler groups
#' @param study_design string passed from params denoting which study design was followed during data collection
#' @param boat_type_collapse string passed from params that controls whether all (potential) boat types (e.g., motor_boat, drift_boat) are collapsed (i.e., boat_type_collapse: "Yes") into a single boat type or kept separate (boat_type_collapse: "No").
#' @param fish_location_determines_type string passed from params that controls whether the observed fishing location for a given angler group during an effort count determines their angler type.
#' @param angler_type_kayak_pontoon string passed from params that controls whether a boat designated as a kayak, pontoon, or kick during an effort count or angler group interview should be designated as a boat or bank angler.
#'
#' @return ??
#' @export
#'
prep_dwg_interview_angler_types <- function(
    interview_fishing_time,
    study_design,
    boat_type_collapse = NA,
    fish_location_determines_type = NA,
    angler_type_kayak_pontoon = NA
){

  if(stringr::str_detect(study_design, "tandard" )){

    interview_angler_types <-
      interview_fishing_time |>
      dplyr::mutate(
        angler_final =
          dplyr::case_when(
            .data$boat_used == "No" ~ "bank", #bank: if anglers did not have/use a boat

            .data$boat_used == "Yes" & is.na(.data$fish_from_boat) & !stringr::str_detect(.data$boat_type, "ontoon|ayak") ~ "boat", # boat: if anglers had a boat, were NOT asked where they primarily fished (boat vs. bank), and not a "ontoon|ayak"

            .data$boat_used == "Yes" & is.na(.data$fish_from_boat) & stringr::str_detect(.data$boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") ~ "boat", # boat: if anglers had a boat, were NOT asked where they primarily fished (boat vs. bank), were in a "ontoon|ayak" AND "ontoon|ayak" considered a boat
            .data$boat_used == "Yes" & is.na(.data$fish_from_boat) & stringr::str_detect(.data$boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "ank") ~ "bank", # bank: if anglers had a boat, were NOT asked where they primarily fished (boat vs. bank), were in a "ontoon|ayak" AND "ontoon|ayak" considered a bank

            .data$boat_used == "Yes" & !is.na(.data$fish_from_boat) & stringr::str_detect(fish_location_determines_type, "Y") & stringr::str_detect(.data$fish_from_boat, "Y|oat")  ~ "boat", # boat: if anglers had a boat, were asked where they primarily fished (boat vs. bank) AND the fishing location determines the angler type, and primarily fish from a boat
            .data$boat_used == "Yes" & !is.na(.data$fish_from_boat) & stringr::str_detect(fish_location_determines_type, "Y") & stringr::str_detect(.data$fish_from_boat, "N|ank")  ~ "bank", # bank: if anglers had a boat, were asked where they primarily fished (boat vs. bank) AND the fishing location determines the angler type, and primarily fish from a bank

            .data$boat_used == "Yes" & !is.na(.data$fish_from_boat) & stringr::str_detect(fish_location_determines_type, "N") & !stringr::str_detect(.data$boat_type, "ontoon|ayak") ~ "boat", # boat: if anglers had a boat, were asked where they primarily fished (boat vs. bank) BUT the fishing location does NOT determine the angler type, and were NOT in a "ontoon|ayak"

            .data$boat_used == "Yes" & !is.na(.data$fish_from_boat) & stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(.data$boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") ~ "boat", # boat: if anglers had a boat, were asked where they primarily fished (boat vs. bank) BUT the fishing location does NOT determine the angler type, were in a "ontoon|ayak" AND "ontoon|ayak" considered a boat
            .data$boat_used == "Yes" & !is.na(.data$fish_from_boat) & stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(.data$boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "ank") ~ "bank", # bank: if anglers had a boat, were asked where they primarily fished (boat vs. bank) BUT the fishing location does NOT determine the angler type, were in a "ontoon|ayak" AND "ontoon|ayak" considered a bank
            TRUE ~ "fail"
          )
        , angler_final_int = as.integer(factor(.data$angler_final))
      )

  }else if(study_design == "Drano"){

    interview_angler_types <-
      interview_fishing_time |>
      dplyr::mutate(
        angler_final =
          dplyr::case_when(
            .data$boat_used == "No" ~ "bank",

            .data$boat_used == "Yes" & stringr::str_detect(.data$boat_type, "railered|Motorized") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
            .data$boat_used == "Yes" & stringr::str_detect(.data$boat_type, "railered|Motorized") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_motor",

            .data$boat_used == "Yes" & stringr::str_detect(.data$boat_type, "Skiff|Pram") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
            .data$boat_used == "Yes" & stringr::str_detect(.data$boat_type, "Skiff|Pram") & stringr::str_detect(boat_type_collapse, "N") ~  "boat_skiff",
.data$
            .data$boat_used == "Yes" & stringr::str_detect(.data$boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
            .data$boat_used == "Yes" & stringr::str_detect(.data$boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_single",
            .data$boat_used == "Yes" & stringr::str_detect(.data$boat_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "ank")  ~ "bank",
            TRUE ~ "fail"
          )
        , angler_final_int = as.integer(factor(.data$angler_final))
      )
  }
  return(interview_angler_types)
}
