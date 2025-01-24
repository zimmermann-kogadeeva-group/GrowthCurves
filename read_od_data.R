library(magrittr)

convert_to_num_mins <- function(vec) {
    if (is.character(vec)) {
        new_vec <- vec %>% lubridate::hms() %>% as.numeric() / 60
    } else { 
        new_vec <- difftime(vec, vec[1], units="mins") %>% as.numeric()
    }
    return(new_vec)
}

read_data <- function(path, sheet, skip=25, n_max=97, ...) {
    dots <- dplyr::enquos(...)

    # read the data, convert time column to time elapsed since beginning of 
    # taking OD measurements. Also pivot to long format.
    readxl::read_excel(path, skip = skip, n_max = n_max, sheet = sheet) %>%
    dplyr::mutate(time_elapsed_min=convert_to_num_mins(Time))  %>%
    # 2 is to remove temp col (with diff names)
    dplyr::select(-c("Time", 2)) %>%  
    tidyr::pivot_longer(
        cols = -time_elapsed_min, 
        names_to = "well", 
        values_to = "OD"
    ) %>%
    tidyr::separate_wider_position(
        cols=well, 
        widths=c(row=1, col=2), 
        too_few="align_start", 
        cols_remove = F
    ) %>%
    dplyr::mutate(
        col = factor(
            col, 
            levels = seq(1, 12) %>% as.character(), 
            labels = seq(1, 12) %>% as.character()),
        !!!dots
    ) %>%
    return()
}

read_od_data <- function(
    path, 
    sheet, 
    blank_wells=NULL,
    normalize_over=NULL, 
    ...
) {
    # Check where the actual table is based on the location of Time column name
    # Note that Time is also in the metadata at the top of the spreadsheet
    df <- readxl::read_excel(path, sheet[1])
    time_idxs <- which(df[, 1] == "Time")
    alt_time_idxs <- which(df[, 2] == "Time")
    if (length(time_idxs) == 2) {
        skip <- as.integer(time_idxs[2])
    } else if (length(alt_time_idxs) > 0) {
        skip <- as.integer(alt_time_idxs[1])
    } else {
        stop("Could not find 'Time' column in the table")
    }
    n_max <- which(df[,1] == "Results") - skip - 2

    # Make sure that ellipsis is converted properly
    dots <- dplyr::enquos(...)

    # Loop over all the specified sheets
    data <- sheet %>%
    purrr::imap(~read_data(
        path = path, 
        sheet = .x, 
        skip=skip,
        n_max=n_max,
        plate=.y,
        !!!dots
    )) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(plate=as.factor(plate))

    # Normalize based on user specified columns
    normalize_over <- c("time_elapsed_min", normalize_over)
    blank_wells <- dplyr::enquo(blank_wells)

    if (!rlang::quo_is_null(blank_wells)) {
        data <- data %>%
        dplyr::filter(!!blank_wells) %>% # select the blank wells
        dplyr::group_by(across(all_of(normalize_over))) %>%  # TODO: add groupby args as func args
        dplyr::summarize(mean_OD_blank = mean(OD)) %>%
        dplyr::ungroup() %>%
        dplyr::full_join(data, by=normalize_over) %>%
        dplyr::mutate(norm_OD = OD - mean_OD_blank)
    }

    return(data)
}

