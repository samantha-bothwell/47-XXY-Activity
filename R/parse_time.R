#### Helper function to parse time
parse_time <- function(x) {
  if (is.numeric(x)) {
    # Excel serial: convert to POSIXct
    return(janitor::excel_numeric_to_date(x, include_time = TRUE))
  }
  if (inherits(x, c("POSIXct", "POSIXt", "Date"))) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  # Try common formats (quietly). Add more orders if needed.
  t <- suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE))
  if (all(is.na(t))) t <- suppressWarnings(lubridate::mdy_hms(x, quiet = TRUE))
  if (all(is.na(t))) t <- suppressWarnings(lubridate::dmy_hms(x, quiet = TRUE))
  return(t)
}
