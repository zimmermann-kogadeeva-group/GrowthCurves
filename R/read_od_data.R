library(magrittr)


possible_wells <- expand.grid(col = LETTERS[1:8], row = seq(1, 12)) %>%
  dplyr::mutate(well = paste0(col, row)) %>%
  dplyr::pull(well)


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
  suppressMessages(
    df <- readxl::read_excel(path, sheets[1])
  )

  time_idxs <- df[, 1] %>%
    dplyr::pull() %>%
    stringr::str_detect("Time") %>%
    which()

  alt_time_idxs <- df[, 2] %>%
    dplyr::pull() %>%
    stringr::str_detect("Time") %>%
    which()

  if (length(time_idxs) == 2) {
    skip <- as.integer(time_idxs[2])
  } else if (length(alt_time_idxs) > 0) {
    skip <- as.integer(alt_time_idxs[1])
  } else {
    stop("Could not find 'Time' column in the table")
  }

  n_max <- df[, 1] %>%
    dplyr::pull() %>%
    stringr::str_detect("Results") %>%
    which() - skip - 2

  # Either 'Results' or 'End Time' indicates end of data
  if (length(n_max) == 0 || n_max < 1) {
    n_max <- df[, 1] %>%
      dplyr::pull() %>%
      stringr::str_detect("End Time") %>%
      which() - skip - 2
  }

  # names(sheets) <- sheets

  purrr::imap(
    sheets,
    ~ {
      readxl::read_excel(path, .x, skip = skip, n_max = n_max) %>%
        dplyr::rename_with(~"Time", starts_with("Time"))
    }
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
    dplyr::select(dplyr::all_of(c("time_elapsed_min", possible_wells))) %>%
    tidyr::pivot_longer(
      cols = -time_elapsed_min,
      names_to = "well",
      values_to = "OD"
    ) %>%
    tidyr::separate_wider_position(
      cols = well,
      widths = c(row = 1, col = 2),
      too_few = "align_start",
      cols_remove = FALSE
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

normalize_od_data <- function(
    data,
    blank_wells = NULL,
    normalize_over = NULL) {
  # Normalize based on user specified columns
  blank_wells <- dplyr::enquo(blank_wells)

  if (!rlang::quo_is_null(blank_wells)) {
    if (is.null(normalize_over)) {
      # If normalize_over arg is NULL, then take a mean over
      # all the blank wells over all time points
      blank_val <- data %>%
        dplyr::filter(!!blank_wells) %>% # select the blank wells
        dplyr::pull(OD) %>%
        mean()

      data <- data %>% dplyr::mutate(mean_OD_blank = blank_val)
    } else {
      # Otherwise take a mean over blank wells per user defined col(s)
      data <- data %>%
        dplyr::filter(!!blank_wells) %>% # select the blank wells
        dplyr::group_by(dplyr::across(dplyr::all_of(normalize_over))) %>%
        dplyr::summarize(mean_OD_blank = mean(OD)) %>%
        dplyr::ungroup() %>%
        dplyr::full_join(data, by = normalize_over)
    }
    data <- data %>%
      dplyr::mutate(norm_OD = OD - mean_OD_blank) %>%
      dplyr::select(-mean_OD_blank)
  }
  return(data)
}

read_od_data <- function(
    path,
    sheets = NULL,
    blank_wells = NULL,
    normalize_over = NULL,
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

  blank_wells <- dplyr::enquo(blank_wells)

  # Loop over all the specified sheets
  data <- list_sheets %>%
    purrr::imap(~ process_data(
      .x,
      time_digits = time_digits,
      plate = .y,
      !!!dots
    )) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(plate = as.factor(plate)) %>%
    normalize_od_data(!!blank_wells, normalize_over)

  return(data)
}

conv_to_factors <- function(data, ...) {
  col_names <- rlang::ensyms(...)
  for (name in col_names) {
    col_vals <- data %>%
      dplyr::pull(!!name) %>%
      unique() %>%
      sort()

    data <- data %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(name), as.factor))
  }
  data
}

add_metadata <- function(data, metadata_path, sheets = NULL) {
  if (is.null(sheets)) {
    sheets <- readxl::excel_sheets(metadata_path)
  }

  data <- data %>%
    dplyr::mutate(plate = as.numeric(plate), col = as.numeric(col))

  suppressMessages(
    for (sheet in sheets) {
      data <- dplyr::left_join(data, readxl::read_excel(metadata_path, sheet))
    }
  )

  data <- data %>% conv_to_factors(plate, col)

  return(data)
}

add_mean_and_sd <- function(
    data, col = "norm_OD",
    mean_over = c("time_elapsed_min", "well")) {
  data %>%
    dplyr::group_by(across(all_of(mean_over))) %>%
    dplyr::summarize(
      mean_norm_OD = mean(!!rlang::sym(col)),
      sd_norm_OD = sd(!!rlang::sym(col))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      lb = mean_norm_OD - sd_norm_OD,
      ub = mean_norm_OD + sd_norm_OD
    ) %>%
    dplyr::full_join(data, by = mean_over)
}
