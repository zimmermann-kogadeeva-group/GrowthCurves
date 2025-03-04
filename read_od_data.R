library(magrittr)

convert_to_num_mins <- function(vec, digits = -1) {
  if (is.character(vec)) {
    new_vec <- vec %>%
      lubridate::hms() %>%
      as.numeric() / 60
  } else {
    new_vec <- difftime(vec, vec[1], units = "mins") %>% as.numeric()
  }
  return(round(new_vec, digits))
}


get_metadata_txt <- function(path) {
  enc <- readr::guess_encoding(path)[[1, 1]]
  f <- readr::read_lines(path, locale = readr::locale(encoding = enc))

  # Time is present in metadata and in table
  # so we skip all Time entries corresponding to metadata
  start_idxs <- which(grepl("Time", f))[c(FALSE, TRUE)]
  end_idxs <- which(grepl("Results", f))
  test_line <- f[start_idxs[1] + 1]
  decimal_mark <- ifelse(stringr::str_detect(test_line, ","), ",", ".")

  list(
    start = start_idxs,
    end = end_idxs,
    decimal_mark = decimal_mark,
    encoding = enc
  )
}

read_od_txt <- function(path, sheets = NULL) {
  meta <- get_metadata_txt(path)

  if (any(is.na(meta$start_idxs))) {
    stop("Could not find 'Time' column in the file")
  }

  if (is.na(meta$decimal_mark)) {
    stop("Could not determine `decimal_mark` within the file")
  }

  if (is.null(sheets)) {
    sheets <- seq(1, length(meta$start))
  }
  sheets <- as.numeric(sheets)

  purrr::map(
    sheets,
    ~ readr::read_tsv(
      path,
      skip = meta$start[.x] - 1,
      n_max = meta$end[.x] - meta$start[.x] - 2,
      locale = readr::locale(
        encoding = meta$encoding,
        decimal_mark = meta$decimal_mark
      ),
      show_col_types = FALSE
    )
  )
}

read_od_xlsx <- function(path, sheets = NULL) {
  if (is.null(sheets)) {
    sheets <- readxl::excel_sheets(path)
  }
  df <- readxl::read_excel(path, sheets[1])

  time_idxs <- which(df[, 1] == "Time")
  alt_time_idxs <- which(df[, 2] == "Time")
  if (length(time_idxs) == 2) {
    skip <- as.integer(time_idxs[2])
  } else if (length(alt_time_idxs) > 0) {
    skip <- as.integer(alt_time_idxs[1])
  } else {
    stop("Could not find 'Time' column in the table")
  }
  n_max <- which(df[, 1] == "Results") - skip - 2

  # names(sheets) <- sheets

  purrr::imap(
    sheets,
    ~ readxl::read_excel(path, .x, skip = skip, n_max = n_max)
  )
}

process_data <- function(data, time_digits = -1, ...) {
  dots <- dplyr::enquos(...)

  # read the data, convert time column to time elapsed since beginning of
  # taking OD measurements. Also pivot to long format.
  data %>%
    tidyr::drop_na() %>%
    dplyr::mutate(time_elapsed_min = convert_to_num_mins(Time, time_digits)) %>%
    # 2 is to remove temp col (with diff names)
    dplyr::select(-c("Time", 2)) %>%
    tidyr::pivot_longer(
      cols = -time_elapsed_min,
      names_to = "well",
      values_to = "OD"
    ) %>%
    tidyr::separate_wider_position(
      cols = well,
      widths = c(row = 1, col = 2),
      too_few = "align_start",
      cols_remove = F
    ) %>%
    dplyr::mutate(
      col = factor(
        col,
        levels = seq(1, 12) %>% as.character(),
        labels = seq(1, 12) %>% as.character()
      ),
      !!!dots
    ) %>%
    return()
}

read_od_data <- function(
    path,
    sheets = NULL,
    blank_wells = NULL,
    normalize_over = "time_elapsed_min",
    time_digits = -1,
    ...) {
  # Make sure that ellipsis is converted properly
  dots <- dplyr::enquos(...)

  file_ext <- tools::file_ext(path)
  if (file_ext == "txt") {
    list_sheets <- read_od_txt(path, sheets)
  } else if (file_ext == "xlsx") {
    list_sheets <- read_od_xlsx(path, sheets)
  } else {
    stop(paste("Unknown format:", file_ext))
  }

  # Loop over all the specified sheets
  data <- list_sheets %>%
    purrr::imap(~ process_data(
      .x,
      time_digits = time_digits,
      plate = .y,
      !!!dots
    )) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(plate = as.factor(plate))

  # Normalize based on user specified columns
  blank_wells <- dplyr::enquo(blank_wells)

  if (!rlang::quo_is_null(blank_wells)) {
    data <- data %>%
      dplyr::filter(!!blank_wells) %>% # select the blank wells
      dplyr::group_by(across(all_of(normalize_over))) %>%
      dplyr::summarize(mean_OD_blank = mean(OD)) %>%
      dplyr::ungroup() %>%
      dplyr::full_join(data, by = normalize_over) %>%
      dplyr::mutate(norm_OD = OD - mean_OD_blank)
  }

  return(data)
}

conv_to_factors <- function(data, ...) {
  col_names <- rlang::ensyms(...)
  for (name in col_names) {
    col_vals <- data %>%
      dplyr::pull(!!name) %>%
      unique() %>%
      sort()
    data <- data %>% dplyr::mutate(across(all_of(name), as.factor))
  }
  data
}

add_metadata <- function(data, metadata_path, sheets = NULL) {
  if (is.null(sheets)) {
    sheets <- readxl::excel_sheets(metadata_path)
  }

  data <- data %>%
    dplyr::mutate(plate = as.numeric(plate), col = as.numeric(col))

  for (sheet in sheets) {
    data <- dplyr::left_join(data, readxl::read_excel(metadata_path, sheet))
  }

  data <- data %>% conv_to_factors(plate, col)

  return(data)
}

add_mean_and_sd <- function(data, mean_over = c("time_elapsed_min", "well")) {
  data %>%
    dplyr::group_by(across(all_of(mean_over))) %>%
    dplyr::summarize(mean_norm_OD = mean(norm_OD), sd_norm_OD = sd(norm_OD)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      lb = mean_norm_OD - sd_norm_OD,
      ub = mean_norm_OD + sd_norm_OD
    ) %>%
    dplyr::full_join(data, by = mean_over)
}
