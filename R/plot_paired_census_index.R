#' plot paired census index
#'
#' Pared down version of prep_inputs_pe_ang_hrs_vhcl_trlr.R function to return table with season-long
#' sum of angler counts from paired census and index count surveys and scatter plot showing ratio of census
#'  to index counts relative to a 1:1 line in a scatterplot
#'
#'  Dev notes: where is angler_hours_daily_mean defined?
#'
#' @param days ??
#' @param dwg_summarized ??
#' @param interview_ang_per_vehic ??
#' @param census_expan ??
#' @param ... ??
#'
#' @return List. `$plot` is ggplot object, `$census_TI_expan` is ??
#' @export
#'
plot_paired_census_index <- function(
    days,
    dwg_summarized,
    interview_ang_per_vehic,
    census_expan,
    ...
){

  eff_ind <- dplyr::left_join(
    dwg_summarized$effort_index,
    days |>
      dplyr::select("event_date", "day_type", "period", "day_length"),
    by=c("event_date")
  ) |>
    dplyr::mutate(
      angler_final = dplyr::case_when(
        .data$count_type == "Trailers Only" ~ "boat",
        .data$count_type == "Vehicle Only" ~ "total"
      )
    )

  if(nrow(dwg_summarized$effort_census) == 0) {
    census_TI_expan <- expand_grid(
      section_num = unique(angler_hours_daily_mean$section_num),
      angler_final = unique(angler_hours_daily_mean$angler_final),
      TI_expan_final = 1)
  } else {
    #begin census expansion values object by joining census and index in terms of total & boat
    census_TI_expan <- dplyr::left_join(
      #census already grouped & summed by event_date, section_num, tie_in_indicator, count_sequence, and angler_final [bank, boat]
      #but as for interview above, first split and collapse to reassign angler_final as total & boat
      bind_rows(
        dwg_summarized$effort_census |>
          dplyr::group_by("section_num", "event_date", "count_sequence") |>
          dplyr::summarize(angler_final = "total", count_census = sum(.data$count_census),  .groups = "drop")
        ,
        dwg_summarized$effort_census |>
          dplyr::filter(angler_final == "boat") |>
          dplyr::group_by("section_num", "event_date", "count_sequence") |>
          dplyr::summarize(angler_final = "boat", count_census = sum(.data$count_census), .groups = "drop")
      ),
      #index counts via interviews for angler-per-vehic; angler_final already total & boat
      #this is very similar to above pe_estimates$angler_hours_daily_mean
      #but all count_seqs rather than summarized to daily mean
      #as above, applies mean ang-per-vehic within section_num-day_type-angtype
      #where interview missing an ang-type or no interviews on that date
      #prevents loss of use of census info b/c of a single day missing interviews...
      dplyr::full_join(
        interview_ang_per_vehic,
        eff_ind,
        by = c("angler_final")
      ) |>
        dplyr::group_by("section_num", "day_type", "angler_final") |>
        dplyr::mutate(
          ang_per_vhcl_trlr = dplyr::if_else(
            is.na(.data$ang_per_vhcl_trlr),
            mean(.data$ang_per_vhcl_trlr, na.rm=T),
            .data$ang_per_vhcl_trlr)) |>
        dplyr::ungroup() |>
        dplyr::mutate(count_index = .data$ang_per_vhcl_trlr * .data$count_index) |>
        dplyr::select("section_num", "event_date", "count_sequence", "angler_final", "count_index")
      ,
      by = c("section_num", "event_date", "count_sequence", "angler_final")
    ) |>
      tidyr::drop_na(count_index)

    #now overwrite, coercing angler_final back to bank/boat as above for pe_estimates$angler_hours_daily_mean
    #again dropping NAs and negatives as invalid for inferring estimates
    if(any(census_TI_expan$angler_final=="boat")){
      census_TI_expan <- census_TI_expan |>
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
          cols = c(.data$boat, .data$bank),
          names_to = "angler_final",
          values_to = "count"
        ) |>
        tidyr::pivot_wider(
          names_from = "count_type",
          values_from = "count"
        ) |>
        dplyr::mutate(
          count_index = tidyr::if_else(.data$count_index < 0 , 0, .data$count_index)
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

    census_TI_expan <- census_TI_expan |>
      dplyr::group_by("section_num", "angler_final") |>
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
          .data$TI_expan_weighted) |>
          tidyr::replace_na(1),
        TI_expan_final = dplyr::if_else(
          .data$cen_exp_meth == "Direct",
          .data$TI_expan_weighted / .data$p_census,
          .data$census_indir)
      )

  }
  # return(census_TI_expan)

  if(params$census_expansion == "Direct") {

    plot <-  census_TI_expan |>
      ggplot(aes(.data$count_index, .data$count_census, color = .data$angler_final)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2) +
      scale_x_continuous(limits = c(0, NA)) +
      scale_y_continuous(limits = c(0, NA)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~paste("Section:",.data$section_num), labeller = label_wrap_gen(multi_line = F))

    return(list(plot,census_TI_expan))
  }

}
