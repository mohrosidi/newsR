#' Returns news headlines from newsapi.org
#'
#' \code{news_headlines} returns headlines news for a country,
#' specific category in a country, single source, or multiple sources. You can
#' also search with keyword.
#'
#'
#' @param keyword Character string that contains the searchterm.
#' @param category Character string with the category you want headlines from.
#' @param country Character string with the country you want headlines from.
#' @param sources Character vector with with IDs of the news outlets
#'                you want to focus on (e.g., c("usa-today", "spiegel-online")).
#' @param page Specifies the page number of your results that is returned. Must
#'             be numeric. Default is first page. If you want to get all results
#'             at once, use \code{get_headlines_all} from 'newsanchor'.
#' @param pageSize The number of articles per page that are returned.
#'                  Maximum is 100 (also default).
#' @param get_all Logical value. If all pages will be collected then gel_all is TRUE
#' @param api_key Character string with the API key you get from newsapi.org.
#'                Passing it is compulsory. Alternatively, a function can be
#'                provided from the global environment.
#'
#' @examples
#' \dontrun{
#' df <- news_headlines(sources = "bbc-news")
#' df <- news_headlines(keyword = "sports", page = 2)
#' df <- news_headlines(category = "business")
#' df <- news_headlines(keyword = "jokowi", country = "id", get_all = TRUE)
#' }
#' @importFrom purrr map_lgl pluck
#' @importFrom httr GET parse_url add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>% bind_rows
#'
#' @return A tibble
#' @export
news_headlines <- function(keyword = NULL,
                               category = NULL,
                               country = NULL,
                               sources = NULL,
                               page = 1,
                               pageSize = 100,
                               get_all = FALSE,
                               api_key = Sys.getenv("NEWS_API_KEY")){

  # error and warning handling ------

  ## if any arguments NULL
  if(all(map_lgl(list(keyword, category, country, sources), is.null))){
    stop(paste("Please provide at least keyword, category, country, and source"))
  }

  ## Check if combination sources, country or category allowed
  if (!is.null(sources) & (!is.null(country) | !is.null(category))){
    stop(paste0("'sources' cannot be used together with ",
                "'country' and/or 'category'."))
  }

  # Check if combination get_all, page, and pageSize valid
  if (!get_all & (!is.null(page) | !is.null(pageSize))){
    stop(paste0("'get_all' cannot be used together with ",
                "'page' and/or 'pageSize'.",
                "if you specify get_all argument to TRUE, then page and pageSize argument must be NULL"))
  }

  ## Check keyword
  if(!is.null(keyword)){
    if(length(keyword) > 1){
      stop("You can only specify one keyword string.")
    }
  }

  ## Check category
  if(!is.null(category)){
    if(length(category) > 1){
      stop("You cannot specify more than one category.")
    }
    if(!category %in% c("business", "entertainment", "general",
                        "health", "science", "sports", "technology")){
      stop(paste(category, "is not valid. Please specify it using business, entertainment, general, health, science, sports, or technology"))
    }

  }

  ## Check country
  if(!is.null(country)){
    if(length(country)>1){
      stop("You cannot specify more than one country")
    }
    if(!country %in% c("ae", "ar", "at", "au", "be", "bg", "br", "ca",
                       "ch", "cn", "co", "cu", "cz", "de", "eg", "fr",
                       "gb", "gr", "hk", "hu", "id", "ie", "il", "in",
                       "it", "jp", "kr", "lt", "lv", "ma", "mx", "my",
                       "ng", "nl", "no", "nz", "ph", "pl", "pt", "ro",
                       "rs", "ru", "sa", "se", "sg", "si", "sk", "th",
                       "tr", "tw", "ua", "us", "ve", "za")){
      stop(paste(country, 'is not valid. Please specify it using "ae", "ar", "at", "au", "be", "bg", "br", "ca"',
                       '"ch", "cn", "co", "cu", "cz", "de", "eg", "fr"',
                       '"gb", "gr", "hk", "hu", "id", "ie", "il", "in"',
                       '"it", "jp", "kr", "lt", "lv", "ma", "mx", "my"',
                       '"ng", "nl", "no", "nz", "ph", "pl", "pt", "ro"',
                       '"rs", "ru", "sa", "se", "sg", "si", "sk", "th"',
                       '"tr", "tw", "ua", "us", "ve", or "za".',
                 "See : https://newsapi.org/sources"))
    }
  }


  ## Check sources
  source_vec <- news_sources(api_key = api_key) %>%
    select(id) %>%
    pull()


  if(!is.null(sources)){
    if(length(sources)>1){
      stop("You cannot specify more than one sources")
    }
    if(!source %in% source_vec){
      stop(paste0(source, "is not valid. Check news_sources(<YOUR API KEY>)"))
    }
  }

  ## Check page
  if(!is.numeric(page)) {
    stop("Page should be a number.")
  }

  ## Check pageSize
  if(!is.numeric(pageSize)) {
    stop("You need to insert numeric values for the number of texts per page.")
  }
  else if(pageSize > 100) {
    stop("Page size cannot not exceed 100 articles per page.")
  }

  ## Check api_key
  if (nchar(api_key) == 0){
    stop(paste("You did not correctly specify your API key neither as global",
                "variable nor with the function call. See documentation for",
                "further info."))
  }

  # Access NEWS API ----
  ## Query
  query_params <- list(category  = category,
                       sources   = sources,
                       country   = country,
                       q         = keyword,
                       pageSize  = pageSize,
                       page      = page)

  url <- parse_url(url = "https://newsapi.org/v2/top-headlines")
  url$scheme <- "https"
  url$query <- query_params

  ## Data parsed
  resp <- GET(url, add_headers("X-Api-Key" = api_key))

  parsed <- fromJSON(
    content(
      resp, as = "text", type = "application/json", encoding = "UTF-8"
    ),
    flatten = TRUE
  )

  tidy_data <- parsed %>% pluck(3) %>% as_tibble()

  tot_result <- parsed %>% pluck(2)

  # Parsed all page ----
  if(get_all){
    if(tot_result > pageSize){

      ## Calculate numb. of page left
      max_no_of_pages <- ceiling(tot_result / pageSize)

      ## iterate through all page left
      for(i in seq.int(2, max_no_of_pages)) {

        ## temp. data
        query_params <- list(category  = category,
                             sources   = sources,
                             country   = country,
                             q         = keyword,
                             pageSize  = pageSize,
                             page      = i)

        url <- parse_url(url = "https://newsapi.org/v2/top-headlines")
        url$scheme <- "https"
        url$query <- query_params

        ## Data parsed
        resp <- GET(url, add_headers("X-Api-Key" = api_key))

        parsed <- fromJSON(
          content(
            resp, as = "text", type = "application/json", encoding = "UTF-8"
          ),
          flatten = TRUE
        )

        temp <- parsed %>% pluck(3) %>% as_tibble()

        status_code <- resp %>% pluck(2)

        ## bind new data
        tidy_data <- bind_rows(tidy_data, temp)

        # check if last status-code != 200
        if (status_code != 200) break
      }

    }
  }

  return(tidy_data)

}

