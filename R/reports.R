#' Download data for various EIA reports
#'
#' These functions download data for various EIA reports found on the EIA website but not necessarily available through the EIA API.
#'
#' The wrapper function and the individual report functions do not make API calls and do not require an API key.
#'
#' @param id character, the report ID. See details for the list of available reports.
#' @param ... arguments passed to individual report data functions.
#'
#' @return a list, typically a list of data frames
#' @export
#'
#' @examples
#' \dontrun{
#' x <- eia_report("drilling productivity")
#' }
eia_report <- function(id, ...){
  f <- switch(
    id,
    "drilling productivity" = report_drilling_productivity
  )
  f(...)
}

#' @importFrom httr GET write_disk
#' @export
#' @rdname eia_report
report_drilling_productivity <- function(){
  url <- paste0("https://www.eia.gov/petroleum/drilling/xls/dpr-data.xlsx")
  file <- file.path(tempdir(), "dpr-data.xlsx")
  x <- httr::RETRY(
    verb = "GET"
    , url = url
    , httr::write_disk(file)
  )
  x <- tryCatch(
    purrr::map(1:7, ~{
      readxl::read_xlsx(file, .x, skip = 1, .name_repair = "minimal")
    }),
    error = function(e) NULL
  )
  y <- tryCatch(
    readxl::read_xlsx(file, 8, .name_repair = "minimal"),
    error = function(e) NULL
  )
  if(is.null(x)){
    unlink(file, recursive = TRUE, force = TRUE)
    message("Report not found.")
    return(invisible())
  }
  names(x) <- readxl::excel_sheets(file)[1:7]
  unlink(file, recursive = TRUE, force = TRUE)
  list(
    data = dplyr::bind_rows(x, .id = "Region") %>%
      dplyr::mutate(Region = factor(.data[["Region"]])),
    counties = y
  )
}
