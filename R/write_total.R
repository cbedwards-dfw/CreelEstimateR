#' Write Total
#'
#' @param con ??
#' @param creel_estimates_db ??
#'
#' @export
#'
write_total <- function(con, creel_estimates_db) {
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id(schema = "creel", table = "model_estimates_total"),
    value = creel_estimates_db$total,
    row.names = FALSE,
    overwrite = FALSE,
    append = TRUE)
}
