#' List all available fishery names
#'
#' @return Character vector
#' @export
#'
#' @examples
#' head(fetch_fishery_names())
fetch_fishery_names = function(){
  dat <- readr::read_csv(utils::URLencode("https://data.wa.gov/resource/vkjc-s5u8.csv?$limit=200000"),
                  show_col_types = F)
  fisheries <- unique(dat[["fishery_name"]]) |>
    as.character() |>
    stats::na.omit() |>
    sort()
  return(fisheries)
}

#' Find fishery names from a partial names
#'
#' Helps identify options for `fetch_dwg()`.
#'
#' @param fishery_partial Partial fishery name. Can take regular expressions, ignores capitalization.
#'
#' @return character vector.
#' @export
#'
#' @examples
#' search_fishery_name("gamefish")
#'
search_fishery_name <- function(fishery_partial){
  grep(fishery_partial, fetch_fishery_names(), ignore.case = T, value = TRUE)
}

#
# temp = readr::read_csv(utils::URLencode("https://data.wa.gov/resource/6y4e-8ftk.csv?$limit=150000"),
#                        show_col_types = F)


#
# readr::read_csv(utils::URLencode("https://data.wa.gov/resource/6y4e-8ftk.csv?$limit=150000"),
#                 show_col_types = F)
# dwg_base$catch,
# "?$where=fishery_name='",
# fishery_name,
# "'&$limit=100000"
#
#
# "https://data.wa.gov/resource/6y4e-8ftk.csv?$where=fishery_name='Nisqually salmon 2021'&$limit=100000"
# "https://data.wa.gov/resource/6y4e-8ftk.csv?where=fishery_name='Nisqually salmon 2021'&$limit=100000"
#
# url.use = "https://data.wa.gov/resource/6y4e-8ftk.csv"
# fishery.use = "Nisqually salmon 2021"
#
#
# paste0(
#   url.use,
#   "?$where=fishery_name='",
#   fishery.use,
#   "'&$limit=100000"
# ) |>
#   utils::URLencode() |>
#   readr::read_csv(show_col_types = F) |>
#   dplyr::select("interview_id", "catch_id", "species", "run", "life_stage", "fin_mark", "sex", "fork_length_cm", "fate", "fish_count") |>
#   dplyr::mutate(
#     catch_group = paste(.data$species, .data$life_stage, .data$fin_mark, .data$fate, sep = "_") # fish catch groups to estimate catch of
#   )
