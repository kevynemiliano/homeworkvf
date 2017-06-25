#' Reading a file
#'
#' This is a simple function that, validate the existence of a file, if the file does
#' not exist you will see a warning. The file will be read with read_csv function. Then
#' with the file alreay loaded i create a data frame.
#'
#' @importFrom readr read_csv
#'
#' @importFrom dplyr tbl_df
#'
#' @param filename The name of the file i want to read
#'
#' @return This function return the estructure of the file loaded as data frame
#'
#' @examples
#' fars_read("accident_2014.csv.bz2")
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


#' File name creator
#'
#' This is a simple function that give us the file name with the folowing structure:
#' "accident_year.csv.bz2".
#'
#' @param year This is the year of the file i want to generate
#'
#' @return This function returns the file name
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#' List month-year creator
#'
#' This is a function that give us a list with the year that our selection(2013,2014 y
#' 2015) we implement the make_filename and fars_read function to get the data.
#' If we select an unexpected year we'll see a warning message as well we use the
#' tryCatch function to anticipate the possibility of warning or errors and handle it.
#'
#' @importFrom dplyr mutate select
#'
#' @param years This is the year wich i want the month-year information list
#'
#' @return This function returns a list with month-year information
#'
#' @examples
#' fars_read_years(2014)
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



#' A summary year-month infomation
#'
#' This is a function that give us a summary number of accidents of each month of a
#' specific year.
#'
#' @importFrom tidyr spread
#' @import dplyr
#' @import magrittr
#'
#' @param years This is the year wich i want the  each month accidents information
#'
#' @return This function returns a table with twelve observations (number of accidents)
#' for each month
#'
#' @examples
#' fars_summarize_years(2014)
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}



#' A map of accidents
#'
#' This is a function that give us a map where indicates an accidents locations.
#' If i select an invalid state num it generate an error and if select a sate num which
#' does not have any accident a message "no accidents to plot" will appear.
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
#'
#' @param years This is the year wich i want to recopilate the information and plot
#' @param state.num This is the state wich i want to plot
#'
#' @return This function returns a map that indicates the places where happened an
#' accident
#'
#' @examples
#' fars_map_state(1,2014)
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
