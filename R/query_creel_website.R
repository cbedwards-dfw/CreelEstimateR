#' Query the creel website for specific combinations of data
#'
#' Queries the website storing the creel data, which uses the Socrata API (e.g., https://dev.socrata.com/docs/queries/where)
#'
#' The query is focused on filtering, and is designed to be written as an R list, with any number of conditions in it. For example, to return only data in which `fishery_name` is "Baker summer sockeye 2022", the query list should include `fishery_name = "Baker summer sockeye 2022"`. And to return only data for species "Kokanee" or "Rainbow Trout", the query list should also include `species = c("Kokanee", "Rainbow Trout")`.
#'
#' @param table_name Name of table (e.g., catch, interview, effort)
#' @param query_list Basically `filter()` statements as a list of named items. Item name matches the column name of the table, and then item should either be an atomic to be matched exactly, or a vector for matching multiple options. See details.
#' @param limit maximum number of rows to return. Numeric, defaults to 100,000
#'
#' @return tibble of creel data
#' @export
#'
#' @examples
#' table_name = "catch" ## name of table
#' query_list = list( ## each item in list should be named for a column in the table, and should be
#'   fishery_name = "Baker summer sockeye 2022",
#'   fin_mark = "UM",
#'   species = c("Kokanee", "Rainbow Trout")
#' )
#' dat = query_creel_website(table_name, query_list)
#' head(dat)
query_creel_website <- function(table_name,  #name of table to pull from: effort, interview, catch, etc.
                                query_list,
                                limit = 100000){ #named list of conditions to meet. See example. Names must match){

  ## dev notes: should add some checking of query_list names against options
  ## Might want option to add in numerical conditions as well, but that's going to involve
  ## a different code structure

  table_lut = c(effort = "https://data.wa.gov/resource/h9a6-g38s.csv",
                interview = "https://data.wa.gov/resource/rpax-ahqm.csv",
                catch = "https://data.wa.gov/resource/6y4e-8ftk.csv",
                water_bodies = "https://data.wa.gov/resource/nbd2-vdmz.csv",
                closures = "https://data.wa.gov/resource/6zm6-iep6.csv",
                #,gear = "https://data.wa.gov/resource/d2ks-afhz.csv" #currently unused?
                fishery_manager = "https://data.wa.gov/resource/vkjc-s5u8.csv"
  )

  table_name = rlang::arg_match(table_name, names(table_lut))

  query_text = table_lut[[table_name]]
  for(i in 1:length(query_list)){
    if(length(query_list[[i]]) == 1){
      contents = glue::glue("='{query_list[[i]]}'")
    } else {
      contents = paste0(" in('", paste0(query_list[[i]], collapse = "','"), "')")
    }
    if(i == 1){
      prefix = "?$where="
    } else {
      prefix = " AND "
    }
    query_text = paste0(query_text, glue::glue("{prefix}{names(query_list)[i]}{contents}"))
  }
  # for(i in 1:length(numerical_queries)){
  #   query_text = paste0(query_text, glue::glue("AND {}"
  # }

  query_text = paste0(query_text, "&$limit=100000")

  res = query_text |>
    utils::URLencode() |>
    readr::read_csv(show_col_types = F)
  return(res)
}

table_name = "catch" ## name of table
query_list = list( ## each item in list should be named for a column in the table, and should be
  ## either an atomic to be matched exactly, or vector to be %in%'d. Not sure how a vector of numerics will work.
  fishery_name = "Baker summer sockeye 2022",
  fin_mark = "UM",
  species = c("Kokanee", "Rainbow Trout")
)

query_creel_website(table_name, query_list)
