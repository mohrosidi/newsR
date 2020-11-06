#' Returns news sources from newscatcher api
#'
#' \code{newscatcher_sources} returns news sources from newscatcher api.
#'
#'
#' @param topic The topic to which you want to restrict the articles of your choice. This parameter is experimental. Accepted values are news, sport, tech, world, finance, politics, business, economics, entertainment.
#' @param lang Specifies the language of the search. Allowed values are: af, ar, bg, bn, ca,cn, cs, cy, da, de, el, en, es, et, fa, fi, fr, gu, he, hi, hr, hu, id, it, ja, kn, ko, lt, lv, mk, ml, mr, ne, nl, no, pa, pl, pt, ro, ru, sk, sl, so, sq, sv, sw, ta, te, th, tl, tr,tw, uk, ur, vi. Specifying the language will make your search more relevant
#' @param country The country to which you want to narrow your search. This parameter is experimental. We advise you to use it in conjunction with the lang parameter. Accepts any https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2.
#' @param api_key Character string with the API key you get from newsapi.org.
#'                Passing it is compulsory. Alternatively, a function can be
#'                provided from the global environment.
#'
#' @examples
#' \dontrun{df <- newscatcher_sources(topic = "news", lang = "id")}
#'
#' @importFrom purrr map_lgl pluck
#' @importFrom httr GET parse_url add_headers content
#' @importFrom jsonlite fromJSON
#'
#' @return A character vector
#' @export

newscatcher_sources <- function(topic = NULL,
                                lang = NULL,
                                country = NULL,
                                api_key = Sys.getenv("NEWS_API_KEY")){

  # error and warning -----
  if(all(map_lgl(list(topic,lang, country), is.null))){
    stop(paste("Please provide at lest topic, lang, country"))
  }

  ## Check api_key
  if (nchar(api_key) == 0){
    stop(paste("You did not correctly specify your API key neither as global",
               "variable nor with the function call. See documentation for",
               "further info."))
  }

  # query to api -----
  api_host <- "newscatcher.p.rapidapi.com"

  query_params <- list(topic  = topic,
                       lang   = lang,
                       country   = country)

  url <- parse_url(url =  "https://newscatcher.p.rapidapi.com/v1/sources")
  url$scheme <- "https"
  url$query <- query_params

  ## parse the data
  resp <- GET(url, add_headers("x-rapidapi-host" = api_host, "x-rapidapi-key" = api_key))

  parsed <- fromJSON(
    content(
      resp, as = "text", type = "application/json", encoding = "UTF-8"
    ),
    flatten = TRUE
  )

  return(parsed)
}








