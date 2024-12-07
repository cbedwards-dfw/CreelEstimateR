#' Aggregate census (tie in) effort counts, associating to closest-in-time index count.
#'
#' @param eff effort data from dwg filtered using start & end dates passed from params
#' @param study_design string passed from params denoting which study design was followed during data collection
#' @param boat_type_collapse string passed from params that controls whether all (potential) boat types (e.g., motor_boat, drift_boat) are collapsed (i.e., boat_type_collapse: "Yes") into a single boat type or kept separate (boat_type_collapse: "No").
#' @param fish_location_determines_type string passed from params that controls whether the observed fishing location for a given angler group during an effort count determines their angler type.
#' @param angler_type_kayak_pontoon string passed from params that controls whether a boat designated as a kayak, pontoon, or kick during an effort count or angler group interview should be designated as a boat or bank angler.
#' @param params ??
#'
#' @return ??
#' @export
#'
prep_dwg_effort_census <- function(
    eff,
    study_design,
    boat_type_collapse = NA,
    fish_location_determines_type = NA,
    angler_type_kayak_pontoon = NA,
    params
){

  eff_cen <- dplyr::filter(eff, .data$tie_in_indicator == 1) #Filter for effort census (aka tie-in) data
  eff_ind <- dplyr::filter(eff, .data$tie_in_indicator == 0) #Filter for effort index data

  if(nrow(eff_cen) == 0){cat("ATTENTION: No effort census data collected and/or entered into the creel database \n")}

  if(stringr::str_detect(study_design, "tandard" )){
    #create intermediate object census_angler_groups that i.) pairs closest index and census counts and ii.) then converts count_type objects to angler_final based on study design & user defined arguments (in YAML)
    census_angler_groups<-
      dplyr::left_join(
        #census values of interest...
        dplyr::select("eff_cen", "section_num", "event_date", "tie_in_indicator", "count_type", "count_quantity"),
        #...get reassigned count_seq from closest index
        #this nested join is typically expanding, as multiple index counts may match each census section & date
        #then the slice_min & distinct cut back down to a single value to reassign to above
        dplyr::left_join(
          dplyr::distinct(.data$eff_cen, .data$section_num, .data$event_date,
          .data$tie_in_indicator, .data$effort_start_time, .data$count_sequence),
          dplyr::distinct(.data$eff_ind, .data$section_num, .data$event_date,
                          .data$tie_in_indicator, .data$effort_start_time, .data$count_sequence),
          by = c("section_num", "event_date"),
          suffix = c("_cen", "_ind")
        ) |>
          dplyr::group_by(.data$section_num, .data$event_date) |>
          dplyr::slice_min(abs(.data$effort_start_time_cen - .data$effort_start_time_ind), n = 1) |>
          dplyr::ungroup() |>
          dplyr::distinct(.data$section_num, .data$event_date, count_sequence = .data$count_sequence_ind)
        ,
        by = c("section_num", "event_date")
      ) |>
      dplyr::mutate(
        angler_final =
          dplyr::case_when(
            .data$count_type %in% c("Boat", "Boat Anglers") ~ "boat",
            .data$count_type %in% c("Bank", "Bank Anglers") ~ "bank",

            stringr::str_detect(fish_location_determines_type, "Y") & stringr::str_detect(.data$count_type, "Shore") ~ "bank",
            stringr::str_detect(fish_location_determines_type, "Y") & stringr::str_detect(.data$count_type, "Boat - D|Boat - M|Boat - P") ~ "boat",

            stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(.data$count_type, "Motor|Large") ~ "boat",
            stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(.data$count_type, "Drift|Raft")  ~ "boat",

            stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(.data$count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") ~ "boat",
            stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(.data$count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "ank") ~ "bank",

            stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(.data$count_type, "No Boat") ~ "bank",
            TRUE ~ "fail"

          ),
        angler_final_int = as.integer(factor(.data$angler_final))
      )

    census_angler_final<-
      census_angler_groups |>
      dplyr::filter(.data$angler_final != "fail") |>
      dplyr::group_by(.data$section_num,.data$ event_date, .data$tie_in_indicator,
                      .data$count_sequence, .data$angler_final, .data$angler_final_int) |>
      dplyr::summarize(count_census = sum(.data$count_quantity), .groups = "drop") |>
      dplyr::arrange(.data$section_num, .data$event_date, .data$count_sequence) |>
      dplyr::mutate(
        fishery_name = params$fishery_name # add back fishery_name
      ) |>
      dplyr::relocate("fishery_name")



  }else if(study_design == "Drano"){

    cat("NOTE: Per Drano study design, daily effort counts were assumed to be census counts of banks anglers and boat vessels.
        Effort count data were recorded and stored in the data base as index counts but in reality they are census counts with regards to their spatial coverage.
        To fit the Drano Lake creel study design within the larger creel analysis, the function 'prep_dwg_effort_index' was duplicated within
        the 'prep_dwg_effort_census' function to create the object 'census_angler_final' where the field count_index was duplicated and named as 'count_census'.
        Overall, the effort index data set was duplicated to generate an effort census data set." )

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

    index_angler_final<-
      index_angler_groups |>
      dplyr::group_by(.data$section_num, .data$event_date, .data$count_sequence, .data$angler_final) |>
      dplyr::summarise(count_index = sum(.data$count_quantity), .groups = "drop") |>
      dplyr::arrange(.data$section_num, .data$event_date, .data$count_sequence) |>
      dplyr::mutate(
        fishery_name = params$fishery_name, # add back fishery_name
        angler_final_int = as.integer(factor(.data$angler_final))
      ) |>
      dplyr::relocate("fishery_name")

    census_angler_groups<-index_angler_groups
    census_angler_final<-index_angler_final |>
      dplyr::rename(count_census = "count_index") |>
      dplyr::relocate("count_census", .after = "angler_final_int")

  }

  return(list(census_angler_groups = census_angler_groups,
              census_angler_final = census_angler_final))

}
