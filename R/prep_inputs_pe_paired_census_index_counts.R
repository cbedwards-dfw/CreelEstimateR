#' Summarize paired census and index angler effort counts
#'
#' Devnote: uncertain about replace_na(1) -- is it doing as intended?
#'
#' @param days tibble with time strata and closure fields
#' @param dwg_summarized list with shared interview, index and census tibbles
#' @param interview_ang_per_object tibble of interview-based values to translate vehicle/trailer counts to boat/bank
#' @param census_expan tibble summarizing p_census by angler_final and section_num
#' where p_census is a hard coded value in the database specifying the proportion of
#'  a section that is covered during a census count; values less than 1 with result
#'  in census counts being expanded (e.g., census count divided by p_census)
#' @param study_design parameter specifying study design and which if/else loop gets called below
#'
#' @return ?
#' @export
#'
prep_inputs_pe_paired_census_index_counts <- function(
    days,
    dwg_summarized,
    interview_ang_per_object,
    census_expan,
    study_design
){

  if(stringr::str_detect(study_design, "tandard" )){

    if(nrow(dwg_summarized$effort_census) == 0) {
      census_TI_expan <-
        tidyr::expand_grid(
          section_num = dwg_summarized$effort_index |>
            dplyr::distinct(.data$section_num) |>
            dplyr::pull(),
          angler_final = dwg_summarized$effort_index |>
            dplyr::distinct(.data$angler_final)|>
            dplyr::pull(),
          TI_expan_final = 1 # If no census counts counted, this hard codes the spatial expansion to be 1 (i.e., assumes bias parameter is 1); should revisit later
        )
    } else {
      #begin census expansion values object by joining census and index in terms of total & boat
      census_TI_expan <-
        dplyr::left_join(
          #census already grouped & summed by event_date, section_num, tie_in_indicator, count_sequence, and angler_final
          #but as for interview above, first split and collapse to reassign angler_final as total & boat
          dplyr::bind_rows(
            dwg_summarized$effort_census |>
              dplyr::group_by(.data$section_num, .data$event_date, .data$count_sequence) |>
              dplyr::summarize(angler_final = "total", count_census = sum(.data$count_census),  .groups = "drop"),
            dwg_summarized$effort_census |>
              dplyr::filter(.data$angler_final == "boat") |>
              dplyr::group_by(.data$section_num, .data$event_date, .data$count_sequence) |>
              dplyr::summarize(angler_final = "boat", count_census = sum(.data$count_census), .groups = "drop")
          ),
          dwg_summarized$effort_index |>
            dplyr::select(-"fishery_name", -"angler_final_int")
          ,
          by = c("section_num", "event_date", "count_sequence", "angler_final")
        ) |>
        tidyr::drop_na(.data$count_index) |>
        dplyr::left_join(interview_ang_per_object |>
                           dplyr::select("angler_final", "ang_per_object"),
                         by=c("angler_final"))|>
        dplyr::ungroup() |>
        dplyr::mutate(
          count_index_expand = .data$ang_per_object * .data$count_index
        ) |>
        dplyr::select(-"count_index", -"ang_per_object")  |>
        dplyr::rename("count_index" = "count_index_expand")

      #now overwrite, coercing angler_final back to bank/boat as above for pe_estimates$angler_hours_daily_mean
      #again dropping NAs and negatives as invalid for inferring estimates
      if(any(census_TI_expan$angler_final=="boat")){
        census_TI_expan <-
          census_TI_expan |>
          tidyr::pivot_longer(
            cols = c("count_census", "count_index"),
            names_to = "count_type",
            values_to = "count"
          ) |>
          tidyr::pivot_wider(
            names_from = "angler_final",
            values_from = "count"
          ) |>
          dplyr::mutate(
            boat = tidyr::replace_na(.data$boat, 0), # NA's here are implicit 0's due to lack of boat anglers in data
            bank = .data$total - .data$boat,
            total = NULL
          ) |>
          tidyr::pivot_longer(
            cols = c("boat", "bank"),
            names_to = "angler_final",
            values_to = "count"
          ) |>
          tidyr::pivot_wider(
            names_from = "count_type",
            values_from = "count"
          ) |>
          dplyr::mutate(
            count_index = dplyr::if_else(.data$count_index < 0 , 0, .data$count_index)
            # need derived negative count_index estimates of bank anglers to pass through subsequent filter,
            # if count_index doesn't pass filter then TI_expan is NA. Should only be an issue in very data limited situations
            # if not addressed, then effort from index counts with no TI_expan value get dropped from final estimates
          ) |>
          dplyr::filter(
            !is.na(.data$count_census),
            !is.na(.data$count_index),
            .data$count_census >= 0,
            .data$count_index >= 0
          )
      } else {
        census_TI_expan <- census_TI_expan |>
          dplyr::mutate(angler_final = "bank")
      }

      census_TI_expan <-
        census_TI_expan |>
        dplyr::group_by(.data$section_num, .data$angler_final) |>
        dplyr::summarise(
          dplyr::across(c("count_census", "count_index"), sum),
          .groups = "drop"
        ) |>
        dplyr::left_join(census_expan, by = c("section_num", "angler_final")) |>
        dplyr::mutate(
          TI_expan_weighted = .data$count_census / .data$count_index,
          TI_expan_weighted = dplyr::if_else(
            is.infinite(.data$TI_expan_weighted) | .data$TI_expan_weighted == 0,
            1,
            .data$TI_expan_weighted
          ) |> tidyr::replace_na(1), ##CBE: is this doing as intended?
          TI_expan_final = .data$TI_expan_weighted / .data$p_census
        )

    }
  }else if(study_design == "Drano"){
    # NOTE: Drano study design was set up such that effort was censused during each count thus pairing index and census counts not applicable;
    # NOTE: assumed each effort count was a census (i.e., TI_expan_final = 1 for all angler_types and sections)

    census_TI_expan <-
      tidyr::expand_grid(
        section_num = dwg_summarized$effort_index |> dplyr::distinct(.data$section_num) |> dplyr::pull(),
        angler_final = dwg_summarized$effort_index |> dplyr::distinct(.data$angler_final)|> dplyr::pull(),
        TI_expan_final = 1
      )

  }
  return(census_TI_expan)
}
