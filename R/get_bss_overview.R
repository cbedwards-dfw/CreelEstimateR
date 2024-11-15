#' get bss overview
#'
#' @param bss_fit ??
#' @param ecg ??
#'
#' @return ??
#' @export
#'
get_bss_overview <- function(bss_fit, ecg){
  bss_fit |>
    summary(pars = c("E_sum", "C_sum")) |>
    purrr::pluck("summary") |>
    as.data.frame() |>
    tibble::rownames_to_column("estimate") |>
    tibble::as_tibble() |>
    dplyr::bind_cols(
      bss_fit |>
        rstan::get_sampler_params(inc_warmup = FALSE) |> #list of matrices rows-iterations by 6 measures
        rlang::set_names(~paste0("n_div_",1:length(bss_fit@stan_args))) |>
        purrr::map_dbl(~.x[, "divergent__"] |> sum()) |> #n-chains cols of n-divergent transitions per chain
        sum() |>
        tibble::as_tibble_col(column_name = "n_div")
    ) |>
    dplyr::mutate(est_cg = ecg) |>
    dplyr::relocate("estimate", "est_cg")
}
