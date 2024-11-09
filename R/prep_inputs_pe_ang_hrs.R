#' Title
#'
#' @param days tibble with time strata and closure fields
#' @param dwg_summarized list with shared interview, effort index and effort census tibbles
#' @param interview_ang_per_object tibble (from list) that has summarized anglers per count_type object by angler_final
#' @param paired_census_index_counts tibble (from list) that has summarized tie-in (aka census) count expansion factors by section & angler_final
#' @param study_design parameter specifying study design and which if/else loop gets called below
#'
#' @return ?
#' @export
#'
prep_inputs_pe_ang_hrs <- function(
    days,
    dwg_summarized,
    interview_ang_per_object,
    paired_census_index_counts,
    study_design
){

  if(stringr::str_detect(study_design, "tandard" )){

    effort_index_daily_mean <-
      dwg_summarized$effort_index |>
      dplyr::group_by(.data$section_num, .data$event_date, .data$angler_final) |>
      dplyr::summarise(count_index_mean = mean(.data$count_index, na.rm = TRUE), .groups = "drop")|>
      dplyr::arrange(.data$event_date, .data$section_num) # |>

    angler_hours_daily_mean <-
      effort_index_daily_mean |>
      dplyr::left_join(days |>
                         dplyr::select("event_date", "day_type", "period", "day_length"),
                       by=c("event_date")) |>
      dplyr::left_join(interview_ang_per_object |>
                         dplyr::select("angler_final", "ang_per_object"), by=c("angler_final"))|>
      dplyr::ungroup() |>
      dplyr::mutate(
        angler_hours_daily_mean = .data$ang_per_object * .data$count_index_mean * .data$day_length
      ) |>
      dplyr::select(-"day_length", -"ang_per_object", -"count_index_mean") |>
      tidyr::drop_na(.data$angler_hours_daily_mean) |>
      dplyr::arrange(.data$section_num, .data$event_date)

    #now coerce back to angler_final bank/boat (unexpanded)
    if(any(angler_hours_daily_mean$angler_final=="boat")){
      angler_hours_daily_mean <-
        angler_hours_daily_mean |>
        tidyr::pivot_wider(
          names_from = "angler_final",
          values_from = "angler_hours_daily_mean",
        ) |>
        dplyr::mutate(
          boat = tidyr::replace_na(.data$boat, 0),
          bank = .data$total - .data$boat,
          total = NULL
        ) |>
        tidyr::pivot_longer(
          cols = c("boat", "bank"),
          names_to = "angler_final",
          values_to = "angler_hours_daily_mean"
        ) |>
        dplyr::mutate(
          angler_hours_daily_mean = ifelse(is.na(.data$angler_hours_daily_mean) | .data$angler_hours_daily_mean<0,
                                           0,
                                           angler_hours_daily_mean) #KB addition (instead of filtering out NAs and negatives, I turned them to zero)
        )

    } else { #if no boat/trailer then total==bank
      angler_hours_daily_mean <-
        angler_hours_daily_mean |>
        dplyr::mutate(angler_final = "bank")
    }

  }else if(study_design == "Drano"){ #KB addition

    effort_index_daily_mean <-
      dwg_summarized$effort_index |>
      dplyr::group_by(.data$section_num, .data$event_date, .data$angler_final) |>
      dplyr::summarise(count_index_mean = mean(.data$count_index, na.rm = TRUE), .groups = "drop")|>
      dplyr::arrange(.data$event_date, .data$section_num) # |>

    angler_hours_daily_mean <-
      effort_index_daily_mean |>
      dplyr::left_join(days |>
                         dplyr::select("event_date", "day_type", "period", "day_length"), by=c("event_date")) |>
      dplyr::left_join(interview_ang_per_object |>
                         dplyr::select("angler_final", "ang_per_object"), by=c("angler_final"))|>
      dplyr::ungroup() |>
      dplyr::mutate(
        angler_hours_daily_mean = dplyr::if_else(.data$angler_final == "bank",
                                                 .data$count_index_mean *.data$day_length,
                                                 .data$ang_per_object * .data$count_index_mean * .data$day_length)
      ) |>
      dplyr::select(-"day_length", -"ang_per_object", -"count_index_mean") |>
      tidyr::drop_na(.data$angler_hours_daily_mean) |>
      dplyr::arrange(.data$section_num, .data$event_date)

  }

  #now multiply mean daily effort in fishing_time by tie-in ratio bias term
  #aiming for event_date, section_num, angler_final [total, boat, bank (as total-boat)]
  angler_hours_daily_mean_TI_expan <-
    dplyr::left_join(
      angler_hours_daily_mean,
      paired_census_index_counts |>
        dplyr::select("section_num", "angler_final", "TI_expan_final"),
      # census_TI_expan |> dplyr::select(section_num, angler_final, TI_expan_final)
      by = c("section_num", "angler_final")
    ) |>
    dplyr::mutate(
      ang_hrs_daily_mean_TI_expan = .data$angler_hours_daily_mean * .data$TI_expan_final
    )

  #return(angler_hours_daily_mean)
  return(angler_hours_daily_mean_TI_expan)
}
