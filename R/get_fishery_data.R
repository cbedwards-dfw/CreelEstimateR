#' Obtain and format interview, catch, and effort data for sets of fishery-years
#'
#' Based on code written for descriptive_statistics.R in the `creelreview` package. Streamlines
#' the process of getting creel data in useful format.
#'
#' @param fishery_name Character string of fishery name. If a vector of character strings is provided instead, will use all combinations of fishery_name by years.
#' @param years Integer or vector of integers identifying years of data to pull
#'
#' @return List with three dataframes: `$interview`, `$catch`, and `$effort`
#' @export
#'
#' @examples
#' \dontrun{
#' temp <- get_fishery_data(
#'   fishery = "Nisqually salmon",
#'   year = 2021:2023
#' )
#' }
get_fishery_data <- function(fishery_name, years) {
  fisheries <- as.character(interaction(fishery_name, years, sep = " "))

  # fisheries <- c("Skagit fall salmon 2021", "Skagit fall salmon 2022", "Skagit fall salmon 2023", "Skagit fall salmon 2024")

  # download the data from data.wa.gov
  all_data <- rlang::set_names(fisheries) |>
    purrr::map(~ fetch_dwg(fishery_name = .x))

  # select and bind the interview data
  # functionalize this


  interview <- all_data |>
    purrr::map(~ purrr::keep(.x, names(.x) |> stringr::str_detect("interview"))) |> # Filter for "interview" named objects
    # map(~map(.x, ~mutate(.x, zip_code = as.numeric(zip_code)))) |> # issue binding zip code due to data mismatch, likely when zipcode is.na across an entire fishery dataset
    purrr::map_dfr(dplyr::bind_rows) |>
    dplyr::mutate(
      month = lubridate::month(.data$event_date),
      year = lubridate::year(.data$event_date),
      week = lubridate::week(.data$event_date),
      fishing_location = dplyr::if_else(is.na(.data$fishing_location), .data$interview_location, .data$fishing_location)
    )


  # interview <- all_data |>
  #   map(~keep(.x, names(.x) |>  str_detect("interview"))) |>
  #   map_dfr(bind_rows) |>
  #   mutate(
  #     month = lubridate::month(event_date),
  #     year = lubridate::year(event_date),
  #     week = lubridate::week(event_date),
  #     fishing_location = if_else(is.na(fishing_location), interview_location, fishing_location))
  # )



  # select and bind the catch data
  # functionalize this
  catch <- all_data |>
    purrr::map(~ purrr::keep(.x, names(.x) |> stringr::str_detect("catch"))) |> # Filter for "catch" named objects
    purrr::map_dfr(dplyr::bind_rows)

  # select and bind the effort data
  # functionalize this
  effort <- all_data |>
    purrr::map(~ purrr::keep(.x, names(.x) |> stringr::str_detect("effort"))) |> # Filter for "catch" named objects
    purrr::map_dfr(dplyr::bind_rows) |>
    dplyr::mutate(
      month = lubridate::month(.data$event_date),
      year = lubridate::year(.data$event_date),
      week = lubridate::week(.data$event_date)
    )

  return(list(interview = interview, catch = catch, effort = effort))
}
