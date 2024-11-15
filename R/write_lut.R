#' Write look-up table
#'
#' @param con ??
#' @param analysis_lut ??
#' @param max_retries maximum number of times to try to write; numeric, defaults to 5.
#'
#' @export
#'
write_lut <- function(con, analysis_lut, max_retries = 5) {

  #define function to check for NOT NULL constraints in analysis_lut
  validate_lut <- function(data) {
    required_columns <- c("analysis_id", "analysis_name", "r_session_json", "analysis_json", "repo_version")
    missing_values <- sapply(required_columns, function(col) any(is.na(data[[col]])))

    if (any(missing_values)) {
      stop("The following columns contain NA values: ", paste(names(missing_values)[missing_values], collapse = ", "))
    }
    return(data)
  }

  #ensure data adheres to NOT NULL constraints
  analysis_lut <- validate_lut(analysis_lut)

  attempt <- 1
  success <- FALSE

  #attempt to write table, retrying if any NOT NULL constraints are violated
  while (attempt <= max_retries && !success) {
    tryCatch({
      DBI::dbWriteTable(
        conn = con,
        name = DBI::Id(schema = "creel", table = "model_analysis_lut"),
        value = analysis_lut,
        row.names = FALSE,
        overwrite = FALSE,
        append = TRUE
      )

      success <- TRUE
      message("Data written successfully on attempt ", attempt)

    }, error = function(e) {
      message("Attempt ", attempt, " failed: ", e$message)
      attempt <- attempt + 1

      if (attempt > max_retries) {
        stop("Failed to write data after ", max_retries, " attempts.")
      }
    })
  }
}
