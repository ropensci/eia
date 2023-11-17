#' EIA data
#'
#' Obtain data from the EIA.
#'
#' By default, `data`, `facets`, and `freq` are set to `NULL`. To obtain valid
#' input values for each of these arguments, use the specific ID labels
#' as provided by `eia_metadata()`.
#'
#' The use of `start` and `end` require some input to `freq`.
#' By default (`check_metadata = FALSE`), the resulting data will match the
#' temporal resolution provided to `freq`, however, `check_metadata = TRUE` applies
#' further restrictions such that the format of values provided to `start`/`end` must match
#' that of `freq`. Furthermore, regardless of the input format provided to `start`/`end`,
#' the resulting data will always match the specification of `freq`. And lastly,
#' regardless of chosen format, `end` must be strictly greater than `start` to return data.
#'
#' By default, additional processing is done to return a list containing tibble data frames.
#' Set `tidy = FALSE` to return only the initial list result of `jsonlite::fromJSON`.
#' Set `tidy = NA` to return the original JSON as a character string.
#'
#' Set to `cache = FALSE` to force a new API call for updated data.
#' Using `FALSE` always makes a new API call and returns the result from the server.
#' `TRUE` uses memoization on a per R session basis, caching the result of the
#' function call in memory for the duration of the R session.
#' You can reset the entire cache by calling `eia_clear_cache()`.
#'
#' @param dir character, directory path.
#' @param data character or `NULL`, see details.
#' @param facets character list or `NULL`, see details.
#' @param freq character or `NULL`, see details.
#' @param start,end character or `NULL`, see details.
#' @param sort named list of two.
#'   * `cols`: list column names on which to sort.
#'   * `order`: `"asc"` or `"desc"` for ascending or descending, respectively.
#' @param length numeric or `NULL`, number of rows to return.
#' @param offset numeric or `NULL`, number of rows to skip before return.
#' @param tidy logical or `NULL`, return a tidier result. See details.
#' @param check_metadata logical, if `TRUE` checks input values against metadata endpoint.
#' @param cache logical, cache result for duration of R session using memoization.
#' See details.
#' @param key API key: character if set explicitly; not needed if key is set
#' globally. See `eia_set_key()`.
#'
#' @return data frame
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
eia_data <- function(dir,
                     data = NULL, facets = NULL,
                     freq = NULL, start = NULL, end = NULL,
                     sort = NULL, length = NULL, offset = NULL,
                     tidy = TRUE, check_metadata = FALSE, cache = TRUE,
                     key = eia_get_key()){
  .key_check(key)
  if (check_metadata)
    .eia_metadata_check(dir, data, facets, freq, start, end, key)
  if (cache){
    .eia_data_memoized(dir, data, facets, freq, start, end, sort, length, offset, tidy, key)
   } else {
    .eia_data(dir, data, facets, freq, start, end, sort, length, offset, tidy, key)
  }
}

.eia_data <- function(dir, data, facets, freq, start, end, sort, length, offset, tidy, key){
  r <- .eia_get(.eia_data_url(dir, data, facets, freq, start, end, sort, length, offset, key))
  if(is.na(tidy)) return(r)
  r <- jsonlite::fromJSON(r)
  if(!tidy) return(r)
  if (!is.null(r$response$warnings) & is.null(length)){
    wrngs <- paste0(r$response$warnings[[1]], "\n", r$response$warnings[[2]])
    warning(wrngs, "\nTotal available rows: ", r$response$total, call. = FALSE)
  } else {
    if (r$response$total == 0)
      stop("No data available - check inputs.", call. = FALSE)
    if (nrow(r$response$data) != r$response$total)
      warning("Rows returned: ", nrow(r$response$data), "\nRows available: ", r$response$total, call. = FALSE)
  }
  tibble::as_tibble(r$response$data)
}

.eia_data_memoized <- memoise::memoise(.eia_data)

.eia_data_url <- function(dir, data, facets, freq, start, end, sort, length, offset, key){
  paste0(
    .eia_url(path = paste0(dir, "/data/?api_key=", key)),
    .data_specs(data),
    .facet_specs(facets),
    .freq_specs(freq),
    .start_specs(start, freq),
    .end_specs(end, freq),
    .sort_specs(sort),
    .lng_specs(length),
    .ofs_specs(offset)
  )
}

.eia_metadata_check <- function(dir, data, facets, freq, start, end, key){
  md <- eia_metadata(dir, TRUE, TRUE, key)
  .eia_md_check(md, dir, data, facets, freq, start, end)
}

.eia_md_check <- function(md, dir, data, facets, freq, start, end){
  .data_check(data, md$Data$id)
  .facet_check(facets, md$Facets$id)
  .freq_check(freq, md$Frequency$id)
  .start_check(start, freq, md$Frequency, md$Period$start, md$Period$end)
  .end_check(end, freq, md$Frequency, md$Period$end, md$Period$start)
}

# Data input formatting and validation
.data_specs <- function(data){
  if (!is.null(data)) paste0("&data[]=", data, collapse = "")
}

.data_check <- function(data, dat_ids){
  if (!is.null(data) && !all(data %in% dat_ids))
    stop("Invalid 'data' provided. Options are: '", paste(dat_ids, collapse = "', '"), "'",
         call. = FALSE)
}

# Facets input formatting and validation
.facet_specs <- function(facets){
  if(!is.null(facets))
    paste0(unlist(lapply(1:length(facets),
      function(x){
        paste0("&facets[", names(facets[x]), "][]=", unlist(facets[x]), collapse = "")
      })), collapse = "")
}

.facet_check <- function(facets, fct_ids){
  if(!is.null(facets)){
    nms <- names(facets)
    if (!all(nms %in% fct_ids))
      stop("Invalid 'facets' provided. Options are: '", paste(fct_ids, collapse = "', '"), "'",
           call. = FALSE)
  }
}

# Frequency input formatting and validation
.freq_specs <- function(freq){
  if (!is.null(freq))
    paste0("&frequency=", freq[1])
}

.freq_check <- function(freq, frq_ids){
  if (!is.null(freq)){
    if (!is.character(freq) | length(freq) > 1 || !(freq %in% frq_ids))
      stop("Invalid 'freq' provided. Must be one of: '", paste(frq_ids, collapse = "', '"), "'",
           call. = FALSE)
  }
}

# Start input formatting and validation
.start_specs <- function(start, freq){
  if(!is.null(start)){
    if(!is.character(start))
      stop("'start' must be a character string of length 1.", call. = FALSE)
    if(is.null(freq))
      stop("'start' requires 'freq' be non-NULL.", call. = FALSE)
    paste0("&start=", start)
  }
}

.start_check <- function(start, freq, md_frq_tbl, mds, mde){
  if(!is.null(start)){
    if(!is.character(start))
      stop("'start' must be a character string of length 1.", call. = FALSE)
    if (is.null(freq))
      stop("'start' requires 'freq' be non-NULL.", call. = FALSE)
    fmt <- md_frq_tbl[md_frq_tbl$id == freq, ]$format
    if (nchar(start) != nchar(fmt))
      stop("'start' must be a character string of format: ", fmt, call. = FALSE)
    if (start > mde)
      stop("'start' is beyond the end of available data.", call. = FALSE)
    if (start < mds)
      warning("'start' is beyond available history. Earliest available: ", mds, call. = FALSE)
  }
}

# End input formatting and validation
.end_specs <- function(end, freq){
  if (!is.null(end)){
    if(!is.character(end))
      stop("'end' must be a character string of length 1.", call. = FALSE)
    if(is.null(freq))
      stop("'end' requires 'freq' be non-NULL.", call. = FALSE)
    paste0("&end=", end)
  }
}

.end_check <- function(end, freq, md_frq_tbl, mde, mds){
  if (!is.null(end)){
    if(!is.character(end))
      stop("'end' must be a character string of length 1.", call. = FALSE)
    if (is.null(freq))
      stop("'end' requires 'freq' be non-NULL.", call. = FALSE)
    fmt <- md_frq_tbl[md_frq_tbl$id == freq, ]$format
    if (nchar(end) != nchar(fmt))
      stop("'end' must be a character string of format: ", fmt, call. = FALSE)
    if (end < mds)
      stop("'end' is before the start of available data.", call. = FALSE)
    if (end > mde)
      warning("'end' is beyond available history. Latest available: ", mde, call. = FALSE)
  }
}

# Sort input formatting and validation
.sort_specs <- function(sort){
  if (!is.null(sort)){
    if (length(sort) != 2 || !all(names(sort) %in% c("cols", "order")))
      stop("'sort' must be a named list of length 2 containing the following:\n",
           "'cols' and 'order' of arbitrary length and of length 1, respectively.",
           call. = FALSE)
    cols <- sort$cols
    order <- sort$order
    if (length(cols) < length(order))
      order <- order[1:length(cols)]
    if (length(cols) > length(order))
      order <- rep(order[length(order)], length(cols))
    sort_cols <- lapply(
      1:length(cols),
      function(x){paste0("&sort[", x, "][column]=", unlist(cols[x]), collapse = "")}
    )
    sort_order <- lapply(
      1:length(order),
      function(x) {paste0("&sort[", x, "][direction]=", unlist(order[x]), collapse = "")}
    )
    paste0(sort_cols, sort_order, collapse = "")
  }
}

# Length input formatting and validation
.lng_specs <- function(length){
  if (!is.null(length)){
    if (!is.numeric(length) | length > 5000 | length < 0)
      stop("'length' must be a numeric value between 0 and 5000.", call. = FALSE)
    paste0("&length=", length)
  }
}

# Offset input formatting and validation
.ofs_specs <- function(offset){
  if (!is.null(offset)){
    if (!is.numeric(offset) | offset < 0)
      stop("'offset' must be a numeric value greater than 0.", call. = FALSE)
    paste0("&offset=", offset)
  }
}
