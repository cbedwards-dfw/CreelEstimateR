#' Title
#'
#' @param date_begin ??
#' @param date_end ??
#' @param weekends ??
#' @param holidays ??
#' @param lat ??
#' @param long ??
#' @param period_pe ??
#' @param sections ??
#' @param closures ??
#' @param day_length ??
#' @param day_length_inputs ??
#'
#' @return ??
#' @export
#'
prep_days <- function(
    date_begin,
    date_end,
    weekends = c("Saturday", "Sunday"),
    holidays, #date/char vector of YYYY-MM-DD dates to categorize as "weekend" strata
    lat, long,
    period_pe,
    sections, #numeric vector of all possible sections to estimate
    closures, #tibble of fishery_name, section number and date of closures
    day_length,
    day_length_inputs){

  date_begin <- as.Date(date_begin, format="%Y-%m-%d")
  date_end <- as.Date(date_end, format="%Y-%m-%d")
  holidays <- as.Date(holidays, format="%Y-%m-%d")

  # create tibble with dates and time period strata

  days <- tibble::tibble(
    event_date = seq.Date(date_begin, date_end, by = "day"),
    day = weekdays(.data$event_date),
    day_type = dplyr::if_else(.data$day %in% weekends | .data$event_date %in% holidays, "weekend", "weekday"),
    day_type_num = as.integer(c("weekend" = 1, "weekday" = 0)[.data$day_type]),  #if_else(str_detect(day_type, "end"), 1, 0),
    #Monday to Sunday weeks, see ?strptime
    week = as.numeric(format(.data$event_date, "%W")),
    month = as.numeric(format(.data$event_date, "%m")),
    year = as.numeric(format(.data$event_date, "%Y")),
    period = dplyr::case_when(
      period_pe == "week" ~ .data$week,
      period_pe == "month" ~ .data$month,
      period_pe == "duration" ~ double(1)
    ),
    day_index = as.integer(seq_along(.data$event_date)),
    week_index = as.integer(factor(.data$week, levels = unique(.data$week))),
    month_index = as.integer(factor(.data$month, levels = unique(.data$month)))
  )

  # section calculating day length, using param option day_length_expansion
  # the manual options allows the user to select fixed times or sunrise / sunset with offsets for either start or end times
  # the alternative options are "night closure", "dawn/dusk", "sunrise/sunset"
  if(day_length == "manual"){

    # If manual start and/or end times enter, specify "ui_start_time" and "ui_end_time" in case they were left blank
    # if( day_length_inputs$start_time == "manual"){day_length_inputs$start_time<-c("sunrise")}
    # if( day_length_inputs$end_time == "manual")  {day_length_inputs$end_time<-c("sunset")}

    # Day length based on sunrise/sunset

    day_length_values <-
      tibble::tibble(
        sunset::getSunlightTimes(
          keep=c("sunrise", "sunset"),
          date=days$event_date,
          lat = lat,
          lon = long,
          tz = "America/Los_Angeles"
        )
      ) |>
      dplyr::select(-tidyselect::any_of("lat", "lon"))

    # Start times using sunrise + offset
    if(day_length_inputs$start_time != "manual"){
      day_length_values <-
        day_length_values |>
        dplyr::mutate(
          start_date_time = .data$sunrise - (60*60*day_length_inputs$start_adj)
        )
    }else{ # or manually specified time
      day_length_values <-
        day_length_values |>
        dplyr::mutate(
          start_date_time = as.POSIXct(paste(day_length_values$date, day_length_inputs$start_manual), format = "%Y-%m-%d %H:%M:%S")
        )
    }

    # End times using sunset + offset
    if(day_length_inputs$end_time != "manual"){
      day_length_values <-
        day_length_values |>
        dplyr::mutate(
          end_date_time = .data$sunset + (60*60*day_length_inputs$end_adj)
        )
    }else{ # or manually specified time
      day_length_values <-
        day_length_values |>
        dplyr::mutate(
          end_date_time = as.POSIXct(paste(day_length_values$date, day_length_inputs$end_manual), format = "%Y-%m-%d %H:%M:%S")
        )
    }

    # calculate day length from start and end times from above
    day_length_values <- day_length_values |>
      dplyr::mutate(
        day_length = as.numeric(.data$end_date_time - .data$start_date_time)
      ) |>
      dplyr::select(event_date = .data$date, "day_length")


    #
  }else{
    day_length_values <- suncalc::getSunlightTimes(
      date = days$event_date,
      tz = "America/Los_Angeles",
      lat = lat, lon = long,
      keep=c("sunrise", "sunset", "dawn", "dusk")
    ) |>
      dplyr::select(event_date = .data$date, "sunrise", "sunset", "dawn", "dusk") |>
      dplyr::mutate(
        day_length_dawn_dusk = as.numeric((.data$dusk) - (.data$dawn)),
        day_length_sunrise_sunset = as.numeric((.data$sunset) - (.data$sunrise)),
        day_length_night_closure = as.numeric((.data$sunset + 3600) - (.data$sunrise - 3600)),
      ) |>
      dplyr::mutate(
        day_length = dplyr::case_when(
          .data$day_length == "dawn/dusk" ~ .data$day_length_dawn_dusk,
          .data$day_length == "sunrise/sunset" ~ .data$day_length_sunrise_sunset,
          .data$day_length == "night closure" ~ .data$day_length_night_closure
        )
      ) |>
      dplyr::select("event_date", "day_length")
  }

  # join day_length to days tibble

  days <- days |>
    dplyr::left_join(day_length_values, by = "event_date")

  # expanding join to incorporate closure dates

  days <- dplyr::left_join(
    days,
    dplyr::rows_update(
      tidyr::expand_grid(event_date = days$event_date,
                         section_num = sections,
                         open = TRUE),
      closures |>
        dplyr::mutate(
          event_date = as.Date(.data$event_date, format="%Y-%m-%d"),
          section_num = as.double(.data$section_num)
        ) |>
        dplyr::filter(dplyr::between(.data$event_date, date_begin, date_end)) |>
        dplyr::select("section_num", "event_date") |>
        dplyr::mutate(open = FALSE)
      ,
      by = c("section_num", "event_date")
    ) |>
      dplyr::arrange(.data$section_num, .data$event_date) |>
      dplyr::mutate(section_num = paste0("open_section_", .data$section_num)) |>
      tidyr::pivot_wider(names_from = .data$section_num, values_from = .data$open),
    by = "event_date"
  ) |>
    dplyr::mutate(
      fishery_name = params$fishery_name
    ) |>
    dplyr::relocate("fishery_name")

  return(days)
}
