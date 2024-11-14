#' Confirm database upload
#'
#'Confirms upload of model estimates by querying model_analysis_lut for session analysis_id.
#'
#' @param con Connection to WDFW PostgreSQL database made with DBI-compliant RPostgres package. 'con' created by establish_db_con.R function.
#' @param analysis_lut Data frame containing session-specific analysis_id and associated metadata created by generate_analysis_lut.R
#'
#' @return nothing returned.
#' @export
#'
confirm_db_upload <- function(con, analysis_lut) {

  #query database for records that match the estimates that were just written
  verification_table <- fetch_db_table(con, "creel", "model_analysis_lut") |> dplyr::select("analysis_id", "analysis_name")

  if (analysis_lut$analysis_id %in% verification_table$analysis_id) {

    DBI::dbDisconnect(con)
    cat(paste("\nData sucessfully exported.", "\u2713"))
    cat("\nDisconnecting from database.")

  } else {
    #what to do if analysis_id is not in analysis_lut (partial/failed export)
    message("\nUnable to confirm upload by checking database for session analysis_id.")

    message(paste("\nWriting",crayon::red$bgYellow("FAILED_UPLOAD_LOG_analysis_lut.csv") , "to CreelEstimates folder so that analysis_id for partial data upload can be investigated."))

    readr::write_csv(analysis_lut, file = paste0("FAILED_UPLOAD_LOG_","analysis_lut.csv"), append = TRUE)

    DBI::dbDisconnect(con)
    stop("\nDisconnecting from database.")
  }
}
