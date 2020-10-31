#' Get resources of newsapi.org
#'
#' \code{news_everything} returns articles from large and small news
#' sources and blogs. This includes news as well as other regular articles.
#' You can search for multiple \code{sources}, different \code{language},
#' or use your own keywords. Articles can be sorted by the earliest date
#' \code{publishedAt}, \code{relevancy}, or \code{popularity}. \cr\cr
#' Valid languages for \code{language} are provided in the dataset
#' \code{terms_language}.
#'
#' @param keyword Character string that contains the searchterm for the API's
#'                data base. API supports advanced search parameters, see 'details'.
#'                Either keyword or keywordInTitle must be specified.
#' @param keywordInTitle Character string that does the same as above _within the
#'                headline only_. API supports advanced search parameters, see 'details'.
#'                Either keyword or keywordInTitle must be specified.
#' @param sources Character vector with with IDs of the news outlets
#'                you want to focus on (e.g., c("usa-today", "spiegel-online")).
#' @param domains Character vector with domains that you want
#'                to restrict your search to (e.g. c("bbc.com", "nytimes.com")).
#' @param excludeDomains Similar usage as with 'domains'. Will exclude these
#'                        domains from your search.
#' @param from Character string with start date of your search. Needs to conform
#'                 to one of the following lubridate order strings:
#'                 \code{"ymdHMs, ymdHMsz, ymd"}. See help for lubridate::parse_date_time.
#'                 If from is not specified, NewsAPI defaults to the oldest available date
#'                 (depends on your paid/unpaid plan from newsapi.org).
#' @param to Character string that marks the end date of your search. Needs to conform
#'                 to one of the following lubridate order strings:
#'                 \code{"ymdHMs, ymdHMsz, ymd"}. See help for lubridate::parse_date_time.
#'                 If \code{to} is not specified,
#'                 NewsAPI defaults to the most recent article available.
#' @param language Specifies the language of the articles of your search. Must
#'                 be in ISO shortcut format (e.g., "de", "en"). Default
#'                 is all languages.
#' @param sortBy Character string that specifies the sorting variable of your article
#'                results. Accepts three options: "publishedAt", "relevancy",
#'                "popularity". Default is "publishedAt".
#' @param page Specifies the page number of your results that is returned. Must
#'             be numeric. Default is first page.
#' @param pageSize The number of articles per page that are returned.
#'                  Maximum is 100 (also default).
#' @param get_all Logical value. If all pages will be collected then gel_all is TRUE
#' @param api_key Character string with the API key you get from newsapi.org.
#'                Passing it is compulsory. Alternatively, function can be
#'                provided from the global environment (see \code{set_api_key()}).
#'
#' @details Advanced search (see also www.newsapi.org): Surround entire phrases
#'          with quotes (") for exact matches. Prepend words/phrases that must
#'          appear with "+" symbol (e.g., +bitcoin). Prepend words that must not
#'          appear with "-" symbol (e.g., -bitcoin). You can also use AND, OR,
#'          NOT keywords (optionally grouped with parenthesis, e.g., 'crypto AND
#'          (ethereum OR litecoin) NOT bitcoin)'). \cr
#' @examples
#' \dontrun{
#' df <- news_everything(keyword = "jokowi", language = "id")
#' df <- news_everything(keyword = "komodo", from = "2019-01-02 12:00:00", get_all = TRUE)
#' }
#' @importFrom httr content GET build_url parse_url add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_lgl pluck
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>%
#' @importFrom lubridate parse_date_time
#' @return A tibble
#' @export


news_everything <- function(keyword	       = NULL,
                           keywordInTitle  = NULL,
                           sources         = NULL,
                           domains         = NULL,
                           excludeDomains  = NULL,
                           from            = NULL,
                           to              = NULL,
                           language        = NULL,
                           sortBy          = "publishedAt",
                           page            = 1,
                           pageSize        = 100,
                           get_all         = FALSE,
                           api_key         = Sys.getenv("NEWS_API_KEY")) {
  # Initial proceedings ----

  sortings <- c("publishedAt", "relevancy", "popularity")

  # Errors and warnings handling ----

  # Make sure that any search term is passed
  if (missing(keyword) == TRUE & missing(keywordInTitle) == TRUE)
    stop("You need to specify at either keyword or keywordInTitle.")

  # but not both keyword and keywordInTitle (does not make sense)
  if (!is.null(keyword) & !is.null(keywordInTitle))
    stop("You must only specify either keyword or keywordInTitle.")

  # check that pageSize is <= 100
  if (!is.numeric(pageSize)) {
    stop("You need to insert numeric values for the number of texts per page.")
  }

  if (pageSize > 100) {
    stop("Page size cannot not exceed 100 articles per page.")
  }

  # Error for non-numeric page parameter
  if (!is.numeric(page)) {
    stop("Page should be a number.")
  }

  # Error if language indicated does not match the ones provided by the API
  if (!is.null(language)) {
    if (length(language) > 1) {
      stop("You cannot specify more than one language.")
    }
    if(!language %in% c("ar", "de", "en", "es", "fr", "he", "it", "nl", "no",
                        "pt", "ru", "se", "ud", "zh")){
      stop(paste(language, 'is not valid. Please specify it using "ar", "de", "en", "es", "fr", "he", "it", "nl", "no", "pt", "ru", "se", "ud", or "zh"') )
    }
  }

  # Error if selected sorting does not match the ones provided by the API
  if (!sortBy %in% c("publishedAt", "relevancy", "popularity")) {
    stop("Sortings can be only by 'publishedAt', 'relevancy', or 'popularity'.")
  }

  # Bind together various search parameters as comma-separated strings as required by API
  # Parameter: sources (plus limit to maximum of 20 sources)
  source_vec <- news_sources(api_key = api_key) %>%
    select(id) %>%
    pull()

  if (!is.null(sources)) {
    if (length(sources) > 20) {
      stop("You cannot specify more than 20 sources.")
    }
    map_lgl(sources, function(x){if (!x %in% source_vec) {
      stop(paste0(x, " is not a valid source name. ",
                  "See terms_sources for a list of valid sources."))
    }})
    sources <- paste(sources, collapse = ",")
  }

  # Parameter: domains
  if (!is.null(domains)) {
    domains <- paste(domains, collapse = ",")
  }

  # Parameter: excludeDomains
  if (!is.null(excludeDomains)) {
    excludeDomains <- paste(excludeDomains, collapse = ",")
  }

  # Make sure an API key is provided
  if (nchar(api_key) == 0)
    stop(
      paste0(
        "You did not specify your API key as an argument or as a global variable.",
        " See documentation for further info."
      )
    )

  # Parse date
  lubridate_orders <- c("ymdHMs","ymdHMsz", "ymd")
  lubridate_orders_string <- paste(lubridate_orders, collapse = ", ")

  if (!is.null(from)) {
    from_parsed <- parse_date_time(from, lubridate_orders, quiet = TRUE)
    if (is.na(from_parsed)) {
      stop(paste0("From argument needs conform to one of the following lubridate orders: ",
                  lubridate_orders_string, ". See help for lubridate::parse_date_time. ",
                  "If in doubt, use %Y-%m-%d for a date or %Y-%m-%d %H:%M:%S for datetime."))
    }
    from <- format(from_parsed, "%Y-%m-%dT%H:%M:%S")
  }

  if (!is.null(to)) {
    to_parsed <- parse_date_time(to, lubridate_orders, quiet = TRUE)
    if (is.na(to_parsed)) {
      stop(paste0("To argument needs conform to one of the following lubridate orders: ",
                  lubridate_orders_string, ". See help for lubridate::parse_date_time. ",
                  "If in doubt, use %Y-%m-%d for a date or %Y-%m-%d %H:%M:%S for datetime."))
    }
    to <- format(to_parsed, "%Y-%m-%dT%H:%M:%S")
  }

  # Accessing the API  -----------------------------------------------------
  # Build URL
  query_params <- list(
    q               = keyword,
    qInTitle        = keywordInTitle,
    language        = language,
    sources         = sources,
    domains         = domains,
    excludeDomains  = excludeDomains,
    from            = from,
    to              = to,
    sortBy          = sortBy,
    pageSize        = pageSize,
    page            = page
  )

  url <- parse_url(url = "https://newsapi.org/v2/everything")
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
        query_params <- list(
          q               = keyword,
          qInTitle        = keywordInTitle,
          language        = language,
          sources         = sources,
          domains         = domains,
          excludeDomains  = excludeDomains,
          from            = from,
          to              = to,
          sortBy          = sortBy,
          pageSize        = pageSize,
          page            = page
        )

        url <- parse_url(url = "https://newsapi.org/v2/everything")
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
