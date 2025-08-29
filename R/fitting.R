library(magrittr)
library(rlang)


normalize <- function(data, value = 0.01) {
  data %>%
    dplyr::mutate(
      dplyr::across(.cols = -Time, .fns = ~ (value - .[Time == 0]) + .)
    )
}

get_pred_exp <- function(fit) {
  fit_obs <- growthrates::obs(fit)
  coeffs <- growthrates::coef(fit)

  fit@FUN(
    fit_obs$time,
    c(y0 = unname(coeffs["y0_lm"]), mumax = unname(coeffs["mumax"]))
  ) %>%
    as.data.frame()
}

get_pred_nonlinear <- function(fit) {
  fit_obs <- growthrates::obs(fit)
  coeffs <- growthrates::coef(fit)

  fit@FUN(fit_obs$time, coeffs) %>% as.data.frame()
}

all_pred_exp <- function(fits, x = "time", y = "y") {
  fits@fits %>%
    purrr::imap(~ {
      get_pred_exp(.x) %>% dplyr::mutate(grouping = .y)
    }) %>%
    dplyr::bind_rows() %>%
    tidyr::separate_wider_delim(
      grouping,
      delim = ":", names = fits@grouping
    ) %>%
    dplyr::rename({{ x }} := "time", {{ y }} := "y")
}

all_pred_nonlinear <- function(fits, x = "time", y = "y") {
  fits@fits %>%
    purrr::imap(~ {
      get_pred_nonlinear(.x) %>% dplyr::mutate(grouping = .y)
    }) %>%
    dplyr::bind_rows() %>%
    tidyr::separate_wider_delim(
      grouping,
      delim = ":", names = fits@grouping
    ) %>%
    dplyr::rename({{ x }} := "time", {{ y }} := "y")
}
