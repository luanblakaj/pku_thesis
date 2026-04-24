load_controls <- function() {
  df_unemp <- readxl::read_excel("data/control_unemployment.xlsx") %>%
    dplyr::rename(date_raw = Date, control_unemployment = France) %>%
    dplyr::mutate(
      date = as.Date(paste0(date_raw, "-01")),
      control_unemployment = as.numeric(control_unemployment)
    ) %>%
    dplyr::select(date, control_unemployment)

  df_asylum <- readxl::read_excel("data/control_asylum.xlsx") %>%
    dplyr::rename(date_raw = Date, control_asylum = France) %>%
    dplyr::mutate(
      date = as.Date(paste0(date_raw, "-01")),
      control_asylum = as.numeric(control_asylum)
    ) %>%
    dplyr::select(date, control_asylum)

  df_monthly <- dplyr::full_join(df_unemp, df_asylum, by = "date") %>%
    dplyr::arrange(date)

  df_quarterly <- df_monthly %>%
    dplyr::mutate(
      date = as.Date(paste0(lubridate::year(date), "-", sprintf("%02d", 3 * lubridate::quarter(date) - 2), "-01"))
    ) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      control_unemployment = mean(control_unemployment, na.rm = TRUE),
      control_asylum = sum(control_asylum, na.rm = TRUE),
      .groups = "drop"
    )

  df_annual <- df_monthly %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      control_unemployment = mean(control_unemployment, na.rm = TRUE),
      control_asylum = sum(control_asylum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(date = as.Date(paste0(year, "-07-01"))) %>%
    dplyr::select(date, control_unemployment, control_asylum)

  list(monthly = df_monthly, quarterly = df_quarterly, annual = df_annual)
}

add_controls <- function(df, level = c("monthly", "quarterly", "annual"), date_col = "date") {
  level <- match.arg(level)
  controls <- load_controls()[[level]]
  dplyr::left_join(df, controls, by = stats::setNames("date", date_col))
}
