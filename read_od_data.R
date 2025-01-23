library(magrittr)


read_data <- function(path, sheet, skip=25, n_max=97, ...) {
    dots <- dplyr::enquos(...)

    # read the data, convert time column to time elapsed since beginning of 
    # taking OD measurements. Also pivot to long format.
    readxl::read_excel(path, skip = skip, n_max = n_max, sheet = sheet) %>%
    dplyr::mutate(
        time_elapsed_min=difftime(Time, Time[[1]], units="mins") %>% as.double()
    ) %>%
    dplyr::select(-c("T° 578", "Time")) %>%
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
    skip=25,
    n_max=97,
    blank_wells=NULL,
    normalize_over=NULL, 
    ...
) {
    dots <- dplyr::enquos(...)

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
    dplyr::mutate(plate=as.factor(plate)) %>%
    return()

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

