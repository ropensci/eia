#' EIA data
#'
#' Obtain data from the EIA.
#'
#' By default, `data`, `facets`, and `freq` are set to `NULL`. To obtain valid input
#' values for each of these arguments, one must use the specific ID labels as
#' provided by `eia_metadata()`.
#'
#' By default, additional processing is done to return a list containing tibble data frames.
#' Set `tidy = FALSE` to return only the initial list result of `jsonlite::fromJSON`.
#' Set `tidy = NA` to return the original JSON as a character string.
#'
#' Set to `cache = FALSE` to force a new API call for updated data.
#' Using `FALSE` always makes a new API call and returns the result from the server.
#' `TRUE` uses memoization on a per R session basis, caching the result of the
#' function call in memory for the duration of the R session.
#' You can reset the entire cache by calling `eia_clear_cache`.
#'
#' @param dir character, directory path.
#' @param data character or `NULL`, see details.
#' @param facets character list or `NULL`, see details.
#' @param freq character or `NULL`, if char, then one of: "yearly", "monthly", "daily", "hourly".
#' @param start character or `NULL`, must match format of either default or supplied `freq`;
#' i.e. if `freq = "yearly"`, then format of `start` must be `YYYY`.
#' @param end character or `NULL`, must match format of either default or supplied `freq`;
#' i.e. if `freq = "yearly"`, then format of `start` must be `YYYY`.
#' @param sort character list, list containing two character objects `"columns"` and `"direction"*`.
#'    * `"columns"` may be of arbitrary length and must contain the precise names of the columns
#'    by which to sort.
#'    * `"direction"` must be of length 1 and contain either `"asc"` or `"desc"` for ascending
#'    or descending order, respectively.
#' @param length numeric or `NULL`, specify number of rows to return.
#' @param offset numeric or `NULL`, specify number of rows to skip before return.
#' @param tidy logical or `NULL`, return a tidier result. See details.
#' @param cache logical, cache result for duration of R session using memoization. See details.
#' @param key API key: character if set explicitly; not needed if key is set globally. See `eia_set_key()`.
#'
#' @return data.frame/tibble
#' @export
#'
#' @examples
#' \dontrun{
#' eia_data(
#'   dir = "electricity/retail-sales",
#'   data = "price",
#'   facets = list(sectorid = c("COM", "RES"), stateid = "OH")
#' )
#' }
eia_data <- function(dir, data = NULL, facets = NULL,
                     freq = NULL, start = NULL, end = NULL,
                     sort = NULL, length = NULL, offset = NULL,
                     tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  # if(cache){
  # .eia_data_memoized(dir, data, facets, freq, start, end, tidy, key)
  # } else {
  # .eia_data(dir, data, facets, freq, start, end, tidy, key)
  # }
  print(.eia_data_url(dir, data, facets, freq, start, end, sort, length, offset, key))
  .eia_data(dir, data, facets, freq, start, end, sort, length, offset, tidy, key)
}

.eia_data <- function(dir, data, facets, freq, start, end, sort, length, offset, tidy, key){
  r <- .eia_get(.eia_data_url(dir, data, facets, freq, start, end, sort, length, offset, key))
  if(is.na(tidy)) return(r)
  r <- jsonlite::fromJSON(r)
  if(!tidy) return(r)
  if (!is.null(r$response$warnings) & is.null(length)){
    wrngs <- paste0(r$response$warnings[[1]], "\n", r$response$warnings[[2]])
    ttlrs <- r$response$total
    warning(wrngs, "\nTotal available rows: ", ttlrs)
  } else {
    rtrnd <- nrow(r$response$data)
    ttlrs <- r$response$total
    if (rtrnd != ttlrs) message("Rows returned: ", rtrnd, "\nRows available: ", ttlrs)
  }
  tibble::as_tibble(r$response$data)
}

.eia_data_memoized <- memoise::memoise(.eia_data)

.eia_data_url <- function(dir, data, facets, freq, start, end, sort, length, offset, key){
  dir <- .eia_url(path = paste0(dir, "/data/?api_key=", key))
  dat_spcs <- if(!is.null(data)) .data_specs(data)
  fct_spcs <- if(!is.null(facets)) .facet_specs(facets)
  frq_spcs <- if(!is.null(freq)) .freq_specs(freq)
  str_spcs <- if(!is.null(start)) .start_specs(start)
  end_spcs <- if(!is.null(end)) .end_specs(end)
  srt_spcs <- if(!is.null(sort)) .sort_specs(sort)
  lng_spcs <- if(!is.null(length)) .lng_specs(length)
  ofs_spcs <- if(!is.null(offset)) .ofs_specs(offset)
  paste0(dir, dat_spcs, fct_spcs, frq_spcs, str_spcs, end_spcs, srt_spcs, lng_spcs, ofs_spcs)
}

.data_specs <- function(data){
  paste0("&data[]=", data, collapse = "")
}

.facet_specs <- function(facets){
  paste0(unlist(lapply(
    1:length(facets),
    function(x){
      paste0("&facets[", names(facets[x]), "][]=", unlist(facets[x]), collapse = "")
  })), collapse = "")
}

.freq_specs <- function(freq){
  if (!is.character(freq) | length(freq) > 1)
    stop("frequency must be a character value of length 1.")
  freqs <- c("annual", "yearly", "quarterly", "monthly", "daily", "hourly")
  if (!freq %in% freqs)
    stop("frequency must be one of: 'yearly', 'monthly', 'daily', or 'hourly'.")
  paste0("&frequency=", freq)
}

.start_specs <- function(start, freq){
  if (!is.character(start))
    stop("'start' must be a character matching the required frequency format.")
  paste0("&start=", start)
}

.end_specs <- function(end, freq){
  if (!is.character(end))
    stop("'end' must be a character matching the required frequency format.")
  paste0("&end=", end)
}

.sort_specs <- function(sort){
  if (length(sort) != 2 || !all(names(sort) %in% c("columns", "direction")))
    stop(paste0(
      "'sort' must be a list of length 2 containing objects:",
      "'columns' and 'direction' of arbitrary length and of length 1, respectively."
    ))
  cols <- sort$columns
  dirctn <- sort$direction
  sort_cols <- paste0(unlist(lapply(
    1:length(cols),
    function(x){
      paste0("&sort[0][column]=", unlist(cols[x]), collapse = "")
    })), collapse = "")
  sort_dirs <- if (length(dirctn) > 1) {
    stop("must provide a single value for 'direction': 'asc' or 'desc'.")
  } else if (!dirctn %in% c("asc", "desc")){
    stop("'direction' must be one of 'asc' or 'desc'.")
  } else {
    paste0("&sort[0][direction]=", dirctn)
  }
  paste0(sort_cols, sort_dirs)
}

.lng_specs <- function(length){
  if (!is.numeric(length) | length > 5000)
    stop("'length' must be a numeric value between 0 and 5000.")
  paste0("&length=", length)
}

.ofs_specs <- function(offset){
  if (!is.numeric(offset) | offset < 0)
    stop("'offset' must be a numeric value greater than 0.")
  paste0("&offset=", offset)
}
