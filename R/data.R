#' EIA data
#'
#' Obtain data from the EIA.
#'
#' By default, `data`, `facets`, and `freq` are set to `NULL`. To obtain valid
#' input values for each of these arguments, one must use the specific ID labels
#' as provided by `eia_metadata()`.
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
#' @param freq character or `NULL`, if char, then one of: "yearly", "monthly",
#' "daily", "hourly".
#' @param start,end character or `NULL`, must match format of default or supplied
#' `freq`; i.e. if `freq = "yearly"`, then format of `start` must be `YYYY`.
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
eia_data <- function(dir, data = NULL, facets = NULL,
                     freq = NULL, start = NULL, end = NULL,
                     sort = NULL, length = NULL, offset = NULL,
                     tidy = TRUE, check_metadata = FALSE, cache = TRUE,
                     key = eia_get_key()){
  .key_check(key)
  if (check_metadata)
    .eia_metadata_check(dir, data, facets, freq, start, end, sort, length, offset, key)
  if (cache){
    .eia_data_memoized(dir, data, facets, freq, start, end, sort, length, offset, tidy, key)
   }else {
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
    ttlrs <- r$response$total
    warning(wrngs, "\nTotal available rows: ", ttlrs, call. = FALSE)
  } else {
    rtrnd <- nrow(r$response$data)
    ttlrs <- r$response$total
    if (rtrnd != ttlrs)
      warning("Rows returned: ", rtrnd, "\nRows available: ", ttlrs, call. = FALSE)
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
    .start_specs(start),
    .end_specs(end),
    .sort_specs(sort),
    .lng_specs(length),
    .ofs_specs(offset)
  )
}

.eia_metadata_check <- function(dir, data, facets, freq, start, end, sort, length, offset, key){
  md <- eia_metadata(dir, TRUE, TRUE, key)
  .eia_check_call(md, dir, data, facets, freq, start, end, sort, length, offset)
}

.eia_check_call <- function(md, dir, data, facets, freq, start, end, sort, length, offset){
  .data_check(data, md$Data$id)
  .facet_check(facets, md$Facets$id)
  .freq_check(freq, md$Frequency$id)
  md_start <- md$Period$start; md_end <- md$Period$end
  .start_check(start, freq, md$Frequency, md_start, md_end)
  .end_check(end, freq, md$Frequency, md_end, md_start)
  .sort_check(sort)
  .lng_check(length)
  .ofs_check(offset)
}

.data_specs <- function(data){
  if (!is.null(data)) paste0("&data[]=", data, collapse = "")
}

.data_check <- function(data, ids){
  if (!is.null(data) && !all(data %in% ids))
    stop("'data' must be some combination of: ", paste(ids, collapse = ", "),
         call. = FALSE)
}

.facet_specs <- function(facets){
  if(!is.null(facets))
    paste0(unlist(lapply(1:length(facets),
      function(x){
        paste0("&facets[", names(facets[x]), "][]=", unlist(facets[x]), collapse = "")
      })), collapse = "")
}

.facet_check <- function(facets, ids){
  if(!is.null(facets)){
    nms <- names(facets)
    if (!all(nms %in% ids)){
      stop("names of the 'facets' list input must be some combination of: ",
           paste(ids, collapse = ", "),
           call. = FALSE)
    }
  }
}

.freq_specs <- function(freq){
  if (!is.null(freq)) paste0("&frequency=", freq)
}

.freq_check <- function(freq, ids){
  if (!is.null(freq)){
    if (!is.character(freq) | length(freq) > 1)
      stop("'freq' must be a character value of length 1.",
           "\n'freq' options are: ", paste(ids, collapse = ", "),
           call. = FALSE)
    if (!(freq %in% ids))
      stop("'freq' must be one of: ", paste(ids, collapse = ", "),
           call. = FALSE)
  }
}

.start_specs <- function(start){
  if(!is.null(start)) paste0("&start=", start)
}

.start_check <- function(start, freq, md_frq_tbl, mds, mde){
  if(!is.null(start)){
    fmt <- md_frq_tbl[md_frq_tbl$id == freq, ]$format
    if (!is.character(start) | nchar(start) != nchar(fmt))
      stop("'start' must be a character string of format: ", fmt,
           call. = FALSE)
    if (start > mde)
      stop("'start' is beyond the end of available data.",
           call. = FALSE)
    if (start < mds)
      warning("'start' is beyond available history. Earliest available: ", mds,
              call. = FALSE)
  }
}

.end_specs <- function(end){
  if (!is.null(end)) paste0("&end=", end)
}

.end_check <- function(end, freq, md_frq_tbl, mde, mds){
  if (!is.null(end)){
    fmt <- md_frq_tbl[md_frq_tbl$id == freq, ]$format
    if (!is.character(end) | nchar(end) != nchar(fmt))
      stop("'end' must be a character string of format: ", fmt,
           call. = FALSE)
    if (end < mds)
      stop("'end' is before the start of available data.",
           call. = FALSE)
    if (end > mde)
      warning("'end' is beyond available history. Latest available: ", mde,
              call. = FALSE)
  }
}

.sort_specs <- function(sort){
  if (!is.null(sort)){
    cols <- sort$cols
    sort_cols <- lapply(1:length(cols),
      function(x){paste0("&sort[", x, "][column]=", unlist(cols[x]), collapse = "")}
    )
    order <- sort$order
    sort_order <- lapply(1:length(cols),
      function(x) {paste0("&sort[", x, "][direction]=", order)}
    )
    paste0(unlist(sort_cols), sort_order, collapse = "")
  }
}

.sort_check <- function(sort){
  if (!is.null(sort)){
    if (length(sort) != 2 || !all(names(sort) %in% c("cols", "order")))
      stop("'sort' must be a named list of length 2 containing the following:\n",
           "'cols' and 'order' of arbitrary length and of length 1, respectively.",
           call.=FALSE)
    cols <- sort$cols
    order <- sort$order
    if (length(order) > 1)
      stop("must provide a single value for 'order': 'asc' or 'desc'.",
           call. = FALSE)
    if (!order %in% c("asc", "desc"))
      stop("'order' must be one of 'asc' or 'desc'.",
           call. = FALSE)
  }
}

.lng_specs <- function(length){
  if (!is.null(length)) paste0("&length=", length)
}

.lng_check <- function(length){
  if (!is.null(length)){
    if (!is.numeric(length) | length > 5000)
      stop("'length' must be a single numeric value between 0 and 5000.",
           call. = FALSE)
  }
}

.ofs_specs <- function(offset){
  if (!is.null(offset)) paste0("&offset=", offset)
}

.ofs_check <- function(offset){
  if (!is.null(offset)){
    if (!is.numeric(offset) | offset < 0)
      stop("'offset' must be a single numeric value greater than 0.",
           call. = FALSE)
  }
}
