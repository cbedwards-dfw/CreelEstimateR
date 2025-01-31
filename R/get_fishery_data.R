#' Obtain and format interview, catch, and effort data for sets of fishery-years
#'
#' Based on code written for descriptive_statistics.R in the `creelreview` package. Streamlines
#' the process of getting creel data in useful format.
#'
#' @param fishery_names Character string of fishery name or vector of character strings. If `years` is provided, `fishery_names` is combined with `year` to create fishery names (see example). If `years` is not provided, uses this character string or vector of character strings as the exact fishery names. Try `search_fishery_name()` to see available options.
#' @param years Integer or vector of integers identifying years of data to pull. Optional argument, defaults to NULL.
#'
#' @return List with three dataframes: `$interview`, `$catch`, `$effort`, `$locations`, `$ll`, `$closures`
#' @export
#'
#' @examples
#' \dontrun{
#' temp <- get_fishery_data(
#'   fishery_names = "Nisqually salmon",
#'   years = 2021:2023)
#'   ## exact definition
#'   temp <- get_fishery_data(
#'   fishery_names = c("Skagit fall salmon 2021", "Skagit summer gamefish 2022"))
#'   ## Using the search_fishery_name() function to choose fisheries
#'   temp = get_fishery_data(fishery_names = search_fishery_name("cascade winter"))
#' }
get_fishery_data <- function(fishery_names, years = NULL) {

  if(!is.null(years)){
    fisheries <- as.character(interaction(fishery_names, years, sep = " "))
  } else{
    fisheries = fishery_names
  }

  ## in current formating of fishery_name variable in database will need an option to also specify the full name of fisheries
  ## current approach doesn't work with pattern of using a "return year", e.g., Hoh winter steelhead 2024-25

  # fisheries <- c("Skagit fall salmon 2021", "Skagit fall salmon 2022", "Skagit fall salmon 2023", "Skagit fall salmon 2024")

  # download the data from data.wa.gov
  all_data <- rlang::set_names(fisheries) |>
    purrr::map(~ fetch_dwg(fishery_name = .x)) |>
    purrr::map(~ purrr::keep(.x, ~nrow(.x) > 0)) |>  # this and line below clean out empty lists for fishery-year combos that do not exist in the database; Keep non-empty dataframes
    purrr::keep(~length(.x) > 0) #Keep lists with at least one non-empty dataframe

  # potential enhancement - print out the years for which there were no records?

  # select and bind the interview data
  # functionalize this


  interview <- all_data |>
    purrr::map(~ purrr::keep(.x, names(.x) |> stringr::str_detect("interview"))) |> # Filter for "interview" named objects
    purrr::map(~purrr::map(.x, ~ dplyr::mutate(.x, zip_code = as.numeric(zip_code)))) |> # issue binding zip code due to data mismatch, likely when zipcode is.na across an entire fishery dataset
    purrr::map_dfr(dplyr::bind_rows) |>
    dplyr::mutate(
      month = lubridate::month(.data$event_date),
      year = lubridate::year(.data$event_date),
      week = lubridate::week(.data$event_date),
      fishing_location = dplyr::if_else(is.na(.data$fishing_location), .data$interview_location, .data$fishing_location) # issue when field for interview location varies across for a fishery
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
      week = lubridate::week(.data$event_date))

  # fishery_location lookup tables, contain the sites and sections used for a fishery
  locations <- all_data |>
    purrr::map(~ purrr::keep(.x, names(.x) |> stringr::str_detect("fishery_manager"))) |> # Filter for "catch" named objects
    purrr::map_dfr(dplyr::bind_rows)

  # longitude/latitude lookup tables, contain approximate XY coordinates for waterbodies within a fishery
  ll <- all_data |>
    purrr::map(~ purrr::keep(.x, names(.x) |> stringr::str_detect("ll"))) |> # Filter for "catch" named objects
    purrr::map_dfr(dplyr::bind_rows)

  # fishery_location lookup tables, contain the sites and sections used for a fishery
  closures <- all_data |>
    purrr::map(~ purrr::keep(.x, names(.x) |> stringr::str_detect("closures"))) |> # Filter for "catch" named objects
    purrr::map_dfr(dplyr::bind_rows)

  #)

  return(list(interview = interview, catch = catch, effort = effort, locations = locations, ll = ll, closures = closures))
}
