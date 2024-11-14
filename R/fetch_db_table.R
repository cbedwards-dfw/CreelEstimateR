#' Query database tables
#'
#' Helper function to streamline queries of the WDFW Postgres database.
#'
#' @param con Connection to WDFW PostgreSQL database made with DBI-compliant RPostgres package. 'con' created by establish_db_con.R function.
#' @param schema query schema
#' @param table table in database
#' @param filter filter to apply during query
#'
#' @return tibble of query results
#' @export
#'
#' @examples
#' \dontrun{
#' a <- fetch_db_table(con, "creel", "fishery_location_lut")
#' b <- fetch_db_table(con, "creel", "fishery_location_lut", filter = "survey_type == 'Index'")
#' c <- fetch_db_table(con, "creel", "fishery_location_lut",
#' filter = c("survey_type == 'Index'", "section_num == '1'"))
#' }

fetch_db_table <- function(con, schema, table, filter = NULL) {

  if(!DBI::dbIsValid(con)) {
    stop("No database connection provided.")
  }

  #built query
  query <- dplyr::tbl(con, dbplyr::in_schema(schema, table))

  #apply filters to query
  if (!is.null(filter)) {
    combined_filter <- paste(filter, collapse = " & ")
    query <- query |> dplyr::filter(!!rlang::parse_expr(combined_filter))
  }

  #execute query
  result <- query |> dplyr::collect()

  return(result)
}
