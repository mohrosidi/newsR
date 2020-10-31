#' Returns sources from newsapi.org
#'
#' \code{news_sources} returns the news sources currently available on newsapi.org.
#'
#' @param api_key  String with the API key you get from newsapi.org.
#'                Passing it is compulsory. Alternatively, function can be
#'                provided from the global environment.
#'
#' @importFrom httr GET content
#' @importFrom purrr pluck
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @import dplyr
#' @return A tibble
#'
#' @examples
#' \dontrun{
#' news_sources(api_key)
#' }
#'
#' @export

news_sources <- function(api_key = Sys.getenv("NEWS_API_KEY")){

  # error and warning handling ----
  if (nchar(api_key) == 0){
    stop(paste0("You did not correctly specify your API key neither as global",
                " variable nor with the function call. See documentation for",
                " further info."))
  }

  # Access api ----
  resp <- GET(paste0("https://newsapi.org/v2/sources?apiKey=", api_key))

  # Parsed content ----
  parsed <- fromJSON(
    content(
      resp, as = "text", type = "application/json", encoding = "UTF-8"
    ),
    flatten = TRUE
  ) %>% pluck(2) %>% tibble::as_tibble()

  return(parsed)
}
