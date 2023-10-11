#' EIA data
#'
#' Obtain data from the EIA.
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
#' @param data character vector,
#' @param facets character list,
#' @param frequency character
#' @param start character
#' @param end character
#' @param tidy logical, return a tidier result. See details.
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
#'   facets = list(sectorid = "RES", stateid = "OH")
#' )
#' }
eia_data <- function(dir, data = NULL, facets = NULL, frequency = NULL, start = NULL, end = NULL, tidy = TRUE, cache = TRUE, key = eia_get_key()){
  .key_check(key)
  # if(cache) .eia_data_memoized(dir, data, facets, tidy, key) else .eia_data(dir, data, facets, tidy, key)
  .eia_data(dir, data, facets, tidy, key)
}

.eia_data_url <- function(dir, data, facets, key){
  dir <- .eia_url(path = paste0(dir, "/data/?api_key=", key))
  data_specs <- if(!is.null(data)){
    paste0("&data[]=", data, collapse = "")
  }
  facet_specs <- if(!is.null(facets)){
    l <- lapply(
      1:length(facets),
      function(x) {
        paste0(
          "&facets[", names(facets[x]), "][]=", unlist(facets[x]),
          collapse = ""
        )
      }
    )
    paste0(unlist(l), collapse = "")
  }
  paste0(dir, data_specs, facet_specs)
}

.eia_data <- function(dir, data, facets, tidy, key){
  r <- .eia_data_url(dir, data, facets, key) |> .eia_get()
  if(is.na(tidy)) return(r)
  r <- jsonlite::fromJSON(r)
  if(!tidy) return(r)
  if (!is.null(r$response$warnings)){
    wrngs <- paste0(r$response$warnings[[1]], "\n", r$response$warnings[[2]])
    ttlrs <- r$response$total
    warning(wrngs, "\nTotal available rows: ", ttlrs)
  }
  tibble::as_tibble(r$response$data)
}

.eia_data_memoized <- memoise::memoise(.eia_data)
