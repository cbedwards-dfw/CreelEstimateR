#' Write stratum
#'
#' @param con ??
#' @param creel_estimates_db ??
#'
#' @export
#'
write_stratum <- function(con, creel_estimates_db) {
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id(schema = "creel", table = "model_estimates_stratum"),
    value = creel_estimates_db$stratum,
    row.names = FALSE,
    overwrite = FALSE,
    append = TRUE)
}
