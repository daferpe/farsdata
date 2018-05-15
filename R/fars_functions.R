#' Read file
#'
#' This is a simple function that reads data from a .csv file. That file is coming from the US
#' National Highway Traffic Safety Administration's Fatality Analysis Reporting System, which is
#' a nationwide census providing the American public yearly data regarding fatal injuries suffered
#' in motor vehicle traffic crashes.
#'
#' @param filename A character string giving the file name which will be reading by the function
#'
#' @return This function returns a dataframe with the information in the file. If the file does not
#' exist, it will return an error.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' filename_param <- system.file('data', 'accident_2013.csv.bz2',
#'                              package = 'farsdata')
#' fars_read(filename = filename_param)
#'
#' @export
fars_read <- function(filename) {
  # filepath <- paste0("inst/",filename)
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make file name
#'
#' This is a simple function that makes a file name.
#'
#' @param year A character string or an integer giving the year
#'
#' @return This function returns a character string with the file name.
#'
#' @examples
#' make_filename("2013")
#' make_filename("2015")
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        filename <- sprintf("accident_%d.csv.bz2", year)
        filename <- system.file('data', filename, package = 'farsdata')
        filename
}

#' Read FARS years
#'
#' This is a function that reads the months with data by year.
#'
#' @param years A vector with a list of years
#'
#' @return This function returns a dataframe with the months with data by year. If any of the years do not
#' exist, it will return NULL and a warning.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#'
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(c(2013,2014))
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize FARS data by year and month
#'
#' This is a function that summarizes data from FARS by year and month. That file is coming from the US
#'
#' @param years A vector with a list of years
#'
#' @return This function returns a dataframe with the accidents by year and month.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @seealso \link{fars_read}
#'
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013,2014))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Map with the accidents by state for a year
#'
#' This is a function that shows a map with the accidents by state for a year
#'
#' @param state.num An integer with the State Code
#' @param year A character string or an integer giving the year
#'
#' @return a map. If the \code{state.num} is invalid, it will return an error
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#'
#' @examples
#' fars_map_state(31,2013)
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
