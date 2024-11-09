#' Title
#'
#' @param analysis_lut ??
#' @param estimates_pe ??
#'
#' @return ??
#' @export
process_estimates_pe <- function(analysis_lut, estimates_pe) {

  transformed_pe_data <- list(
    pe_effort = data.frame(),
    pe_catch = data.frame(),
    pe_stratum_effort = data.frame(),
    pe_stratum_catch = data.frame(),
    pe_summarized_effort = data.frame(),
    pe_summarized_catch = data.frame()
  )

  # Incorporate model_type-specific outputs
  #PE
  transformed_pe_data$pe_effort <- estimates_pe$effort
  transformed_pe_data$pe_catch <- estimates_pe$catch

  # Add UUID and model_type columns
  #PE effort
  transformed_pe_data$pe_effort <- transformed_pe_data$pe_effort |>
    dplyr::mutate(analysis_id = analysis_lut$analysis_id,
           model_type = "PE") |>
    dplyr::relocate("analysis_id")

  #PE catch
  transformed_pe_data$pe_catch <- transformed_pe_data$pe_catch |>
    dplyr::mutate(analysis_id = analysis_lut$analysis_id,
           model_type = "PE") |>
    dplyr::relocate("analysis_id")
  ############################ Stratified table ##################################

  #create tables for stratified table intermediates
  transformed_pe_data$pe_stratum_effort <- transformed_pe_data$pe_effort
  transformed_pe_data$pe_stratum_catch <- transformed_pe_data$pe_catch

  # Effort transformation
  transformed_pe_data$pe_stratum_effort <- transformed_pe_data$pe_stratum_effort |>
    dplyr::select(-c("var", "l95","u95")) |>
    tidyr::pivot_longer(cols = c("n_obs":"est"),
                 names_to = "estimate_type",
                 values_to = "value")

  #tidy output
  transformed_pe_data$pe_stratum_effort <- transformed_pe_data$pe_stratum_effort |>
    tidyr::drop_na() |>
    dplyr::ungroup() |>
    dplyr::mutate(estimate_category = "effort") |>
    dplyr::relocate("estimate_category", .after = "model_type")

  # Catch transformation
  transformed_pe_data$pe_stratum_catch <- transformed_pe_data$pe_stratum_catch |>
    dplyr::select(-c("var","l95","u95")) |>
    tidyr::pivot_longer(cols = c("n_obs":"est"),
                 names_to = "estimate_type",
                 values_to = "value")

  #tidy output
  transformed_pe_data$pe_stratum_catch <- transformed_pe_data$pe_stratum_catch |>
    tidyr::drop_na() |>
    dplyr::ungroup() |>
    dplyr::mutate(estimate_category = "catch") |>
    dplyr::relocate("estimate_category", .after = "model_type")


  ############################# Rolled up table ##################################

  #create tables for rolled up table intermediates
  transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_effort
  transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_catch

  # ### handling NaN values in PE effort estimates
  # if(any(is.nan(estimates_pe$effort$est))) {
  #   #check PE inputs
  #   #group by section_num, period, day_type, angler_final
  #   get_period <- dwg$days |> dplyr::select(event_date, period)
  #   effort_est <- inputs_pe$ang_hrs_daily_mean
  #   effort_est <- dplyr::left_join(effort_est, get_period, by = "event_date")
  #
  #   effort_est <- effort_est |>
  #     dplyr::group_by(section_num, period, day_type, angler_final)
  #   #evaluate numerator and denominator for 0 or NA
  #   effort_est_summary <- effort_est |>
  #     dplyr::summarise(
  #         numerator = sum(effort_est$ang_hrs_daily_mean_TI_expan),
  #         denominator = dplyr::n(),
  #         .groups = "keep" #still need n & d to be group-level values
  #     )
  # }

  # Effort transformation
  #checking for NaN values
  rows_pre <- nrow(transformed_pe_data$pe_summarized_effort)

  filtered_data <- transformed_pe_data$pe_summarized_effort |>
    dplyr::filter(!is.nan(.data$ang_hrs_mean) & !is.nan(.data$ang_hrs_var) &
             !is.nan(.data$est) & !is.nan(.data$var) & !is.nan(.data$l95) & !is.nan(.data$u95))

  rows_post <- nrow(filtered_data)
  n_nan <- rows_pre - rows_post

  message(paste("\n", n_nan, "instances of NaN values detected in the PE effort estimates. Values of NaN are filtered and removed from the final data uploaded to the database."))

  #transformation from filtered data
  transformed_pe_data$pe_summarized_effort <- filtered_data |>
    dplyr::group_by(.data$analysis_id, .data$project_name, .data$fishery_name, .data$model_type) |>
    dplyr::summarise(est_sum = sum(.data$est), .groups = "keep") |>
    tidyr::pivot_longer(cols = c("est_sum"),
                 names_to = "estimate_type",
                 values_to = "value") |>
    #set min date as start of monitoring period
    dplyr::mutate(min_event_date = as.Date(params$est_date_start),
           #set max_event_date as sys.date if in-season, set as est_end_date if out of monitoring period
           max_event_date = as.Date(
             ifelse(
               Sys.Date() <= params$est_date_end,
               Sys.Date(),
               params$est_date_end
             ))
    )

  #tidy output
  transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_summarized_effort |>
    dplyr::ungroup() |>
    dplyr::mutate(estimate_category = "effort") |>
    dplyr::relocate("estimate_category", .after = "model_type")

  # Catch
  #calculate days open and days surveyed
  totaldaysopen_totaldayssurveyed <-transformed_pe_data$pe_summarized_catch |>
    dplyr::distinct(.data$period, .data$day_type, .data$est_cg, .data$n_obs, .data$N_days_open) |>
    dplyr::group_by(.data$est_cg) |>
    dplyr::summarise(
      totaldaysopen = sum(.data$N_days_open),
      totalobs = sum(.data$n_obs),
      .groups = 'drop'
    )

  #catch transformation
  transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_summarized_catch |>
    dplyr::group_by(.data$analysis_id, .data$project_name, .data$fishery_name, .data$model_type, .data$est_cg) |> #includes catch group col
    dplyr::summarise(
      # n_obs_sum = sum(n_obs),
      # N_days_open_sum = sum(N_days_open),
              est_sum = {
                #error handling for NA values in the estimate column
                if (any(is.na(.data$est))) {
                  stop("NA values found in the 'est' column of the PE catch estimates. Please review before proceeding.")

                } #if there are no NA values, sum
                sum(.data$est)
              }, .groups = "keep"
    ) |>
    tidyr::pivot_longer(cols = c("est_sum"),
                 names_to = "estimate_type",
                 values_to = "value") |>
    #set min date as start of monitoring period
    dplyr::mutate(min_event_date = as.Date(params$est_date_start),
    #set max_event_date as sys.date if in-season, set as est_end_date if out of monitoring period
           max_event_date = as.Date(
             ifelse(
               Sys.Date() <= params$est_date_end,
               Sys.Date(),
               params$est_date_end
             ))
    )

  #prep totaldays object for joining back with pe_summarized_catch
  totaldaysopen_totaldayssurveyed <- totaldaysopen_totaldayssurveyed |>
    tidyr::pivot_longer(cols = c("totaldaysopen", "totalobs"),
                 names_to = "estimate_type",
                 values_to = "value") |>
    dplyr::mutate(
      analysis_id = unique(transformed_pe_data$pe_summarized_catch$analysis_id),
      project_name = unique(transformed_pe_data$pe_summarized_catch$project_name),
      fishery_name = unique(transformed_pe_data$pe_summarized_catch$fishery_name),
      model_type = unique(transformed_pe_data$pe_summarized_catch$model_type),
      #same date consideration as above
      min_event_date = as.Date(params$est_date_start),
      max_event_date = as.Date(
        ifelse(
          Sys.Date() <= params$est_date_end,
          Sys.Date(),
          params$est_date_end
        ))
    )

  transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_summarized_catch |>
    dplyr::bind_rows(totaldaysopen_totaldayssurveyed)

  #tidy output
  transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_summarized_catch |>
    dplyr::ungroup() |>
    dplyr::mutate(estimate_category = "catch") |>
    dplyr::relocate("estimate_category", .after = "model_type")

  #Get PE and BSS dataframes to match before binding rows

  #table 1, stratum_catch
  transformed_pe_data$pe_stratum_catch <- transformed_pe_data$pe_stratum_catch

  #table 2, stratum_effort
  transformed_pe_data$pe_stratum_effort <- transformed_pe_data$pe_stratum_effort |>
    dplyr::mutate(est_cg = NA)

  #table 3, summarized_catch
  transformed_pe_data$pe_summarized_catch <- transformed_pe_data$pe_summarized_catch

  #table 4, summarized_effort
  transformed_pe_data$pe_summarized_effort <- transformed_pe_data$pe_summarized_effort |>
    dplyr::mutate(est_cg = NA)

  cat("\nPE standarization transformation complete.")

  return(transformed_pe_data)
}
