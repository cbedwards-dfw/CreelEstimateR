#' Download creel dataset from data.wa.gov
#'
#' Downloads a set of electronically collected observations from the statewide freshwater creel database.
#' These consist of counts at index sites and along census survey sections,
#' as well as angler interviews and associated catch information.
#'
#' @param fishery_name Character string with water body area, focal species, year / year-group used to fetch a subset of data for analysis
#'
#' @return ??
#' @export
#'
fetch_dwg <- function(fishery_name){
  fishery_options = fetch_fishery_names()
  rlang::arg_match(fishery_name, fishery_options)

  dwg_base <- list(
    #event = "https://data.wa.gov/resource/ui95-axtn.csv",
    effort = "https://data.wa.gov/resource/h9a6-g38s.csv",
    interview = "https://data.wa.gov/resource/rpax-ahqm.csv",
    catch = "https://data.wa.gov/resource/6y4e-8ftk.csv",
    water_bodies = "https://data.wa.gov/resource/nbd2-vdmz.csv",
    closures = "https://data.wa.gov/resource/6zm6-iep6.csv",
    gear = "https://data.wa.gov/resource/d2ks-afhz.csv", #currently unused?
    fishery_manager = "https://data.wa.gov/resource/vkjc-s5u8.csv"
  )

  dwg <- list()

  dwg$effort <- paste0(
    dwg_base$effort,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F) |>
    tidyr::drop_na(.data$count_type) |>
    dplyr::select(-.data$created_datetime, -.data$modified_datetime)

  dwg$ll <- paste0(
    dwg_base$water_bodies,
    "?$where=water_body_desc in('",
    paste0(unique(dwg$effort$water_body), collapse = "','"),
    "')&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F)

  dwg$interview <- paste0(
    dwg_base$interview,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F) |>
    dplyr::select(
      -.data$created_datetime, -.data$modified_datetime)

  dwg$catch <- paste0(
    dwg_base$catch,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F) |>
    dplyr::select("interview_id", "catch_id", "species", "run", "life_stage", "fin_mark", "sex", "fork_length_cm", "fate", "fish_count") |>
    dplyr::mutate(
      catch_group = paste(.data$species, .data$life_stage, .data$fin_mark, .data$fate, sep = "_") # fish catch groups to estimate catch of
    )

  dwg$closures <- paste0(
    dwg_base$closures,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F) |>
    dplyr::select("fishery_name", "section_num", "event_date")

  dwg$fishery_manager <- paste0(
    dwg_base$fishery_manager,
    "?$where=fishery_name='",
    fishery_name,
    "'&$limit=100000"
  ) |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F)

  return(dwg)
}

# #tests
# fetch_dwg("Naselle winter steelhead 2022")
# fetch_dwg("Skykomish summer Chinook 2022")
