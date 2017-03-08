#' Load a CSV file as data frame tbl
#'
#' Loads a CSV file defined by \code{filename} argument and returns the data
#' as a data frame tibble. Ends with an error if filename is not valid
#'
#' @param filename Path to the CSV file (\code{character}) 
#'
#' @return The function returns a tibble (data.frame) based on the CSV file.
#'
#' @details For the \code{filename} provided, the function checks if the file
#' exists. If not, it stops with an error message. Otherwise the file is read
#' in using \code{read_csv} and returned as a data frame tibble.
#'
#' @examples
#' \dontrun{
#' data_2010 <- fars_read("./data/accident_2010.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make a filename from the years provided
#'
#' The function creates a filename for a .csv.bz2 file based on the \code{year}
#' argument in a form "accident_<year>.csv.bz2". The input is coerced to an integer.
#' If coercion is not possible, it shows a warning message
#'
#' @param year A vector or list indicating the years for which filenames
#' are to be returned. Coerced to \code{integer}
#'
#' @return Returns a \code{character} vector in a format "accident_<year>.csv.bz2" that
#' can be used as file names
#'
#' @details Works for all input that can be coerced to integers. \code{NA}s are
#' introduced with a warning if input can't be coerced. See examples for usage.
#'
#' @examples
#' \dontrun{
#' make_filename(2006)
#' make_filename(2007:2010)
#'
#' year_list <- list(2000, "2001", '2002')
#' make_filename(year_list)
#'
#' # NAs introduced
#' make_filename("some_input")
#' }
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read data for years into a list of data frame tables
#'
#' This function reads data for the years provided and it returns
#' a list containing the data frame tables. The output contains the data
#' for all the months of the specified years.
#'
#' @param years A \code{numeric} vector of years for which data is to be read
#'
#' @return A list of data frame tables. For invalid inputs, a warning message is printed
#' and returns \code{NULL}.
#'
#' @examples \dontrun{
#' fars_read_years(2010:2012)
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
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

#' Summarize FARS data for years provided
#'
#' This function summarizes the list of data frames obtained using (\code{fars_read_years}).
#' The information about the number of cases per year every month is returned.
#'
#' @param years A \code{numeric} vector of years for which data is to be summarized
#'
#' @return Summarized data in a data frame table(\code{tbl_df})
#'
#' @examples \dontrun{
#' fars_summarize_years(2010:2012)
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plots the locations of accidents on a map
#'
#' The function takes a state number and year as input and plots the locations
#' of the accidents on a map. An invalid state number or year results in an error.
#'
#' @param state.num The number of a state in the US as per the FARS data
#' sets. Coerced to \code{integer}
#' @param year The year for which data is to be plotted. Coerced to \code{integer}
#'
#' @return A plot of the accidents for the \code{state.num} and
#' \code{year} provided. Returns \code{NULL} for invalid input
#'
#' @examples
#' \dontrun{
#' fars_map_state(44, 2010)
#' fars_map_state("15", 2010)
#'
#' # Error
#' fars_map_state(45, 2030)
#' fars_map_state(-1, 2010)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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
