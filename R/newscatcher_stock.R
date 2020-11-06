#' Returns document from specific ticker
#'
#' \code{newscatcher_stocks} returns document form specific ticker and stocks
#' Free api can only return 5 document, so make sure to specify page_size = 5.
#' Another things to remember is free api can only make 21 request/hour.
#' If you want to retrive another document more than 5 document please write a loop that only return 5 document/ request and sleep at least one hour after 21 request.
#'
#'
#' @param ticker Ticker of a company you want to make your search about. Pure ticker without specifying its Stock Exchange. eg: AAPL for Apple Inc. ticker.
#' @param key_people Logical value to to match an article when it contains a name of one of the company C-level officer.
#' @param not_sources One or more sources to be excluded from the search.
#' @param from Character that specify point in time to start the search, E.g. ("2020/05/01", "2020-05-01" ,"2020-05-01 12:54:14", "2020-05-01 4:15am", "yesterday 2:02 am", "2020/05/01 12:55 EST").
#' @param to_rank Upper boundary of the rank of news website to filter by. A number between 0 and 999999.
#' @param page_size How many articles to return per page. Defaults to 50, max is 100.
#' @param from_rank Lowest boundary of the rank of news website to filter by. Important: lower rank means that a source is more popular.
#' @param stock Specify the Stock Exchange of the ticker that you look for. We support tickers from this list:
#'
#'               * ASX
#'               * AMEX
#'               * NYSE
#'               * NASDAQ
#'               * EURONEXT
#'               * HKEX
#'               * LSE
#'               * SGX
#'               * SIX
#'               * TSX
#'               * TSXV
#'
#'               We really encourage you precise your Stock Exchange, because there are identical tickers among them.
#' @param to Until which point in time to search for.
#' @param sort_by A character that specify sort method. Possible value:
#'
#'                * "relevancy" — the most relevant results first,
#'                * "date" — the most recently published results first,
#'                * "rank" — the results from the highest-ranked sources first
#' @param sources Character vector with one or more news resources to filter your search. It should be the normal form of the URL, for example, nytimes.com, theguardian.com.
#' @param page The number of the page. Use it to scroll through the results. Defaults to 1.
#' @param media Adds to the output of the call two more variables: media and media_content.
#'
#'             * media - the main image published with an article
#'             * media_content - a comma-separated string of all images used in an article
#' @param lang Logical value. Specifies the language of the search.
#' @param get_all Logical value. If all pages will be collected then gel_all is TRUE.
#' @param get_all Logical value. If all pages will be collected then gel_all is TRUE.
#' @param api_key Character string with the API key you get from newsapi.org.
#'                Passing it is compulsory. Alternatively, a function can be
#'                provided from the global environment.
#'
#' @examples
#' \dontrun{
#' df <- newscatcher_stocks(ticker = "AAPL", page_size = 5)}
#'
#' @importFrom purrr map_lgl pluck
#' @importFrom httr GET parse_url add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>% bind_rows
#'
#' @return A tibble
#' @export

newscatcher_stocks <- function(ticker = NULL,
                               key_people = NULL,
                               not_sources = NULL,
                               from = NULL,
                               to_rank = NULL,
                               page_size = 100,
                               from_rank = NULL,
                               stock = NULL,
                               to = NULL,
                               sort_by = "relevancy",
                               sources = NULL,
                               page = 1,
                               media = NULL,
                               lang = NULL,
                               get_all = FALSE,
                               api_key = Sys.getenv("NEWS_API_KEY")){

  # error and warning handling -----
  if(all(map_lgl(list(ticker, key_people, not_sources, from, to_rank, from_rank, stock, to, sort_by, sources, media, lang), is.null)))
    stop(paste("Please specify at least ticker"))

  ## Check not_sources
  if(!is.null(not_sources)){
    if(length(not_sources)>1)
      not_sources <- paste(not_sources, collapse = ",")
  }

  ## Check to_rank
  if(!is.null(to_rank)){
    if(!to_rank %in% c(0:999999)){
      stop(paste("Please provide  a number between 0 and 999999"))
    }
  }

  ## Check page_size
  if(!is.null(page_size)){
    if(page_size > 100)
      stop(paste("Maximum page_size is 100"))
  }

  ## Check from_rank
  if(!is.null(from_rank)){
    if(!from_rank %in% c(0:999999)){
      stop(paste("Please provide  a number between 0 and 999999"))
    }
  }

  ## Check stock
  if(!is.null(stock)){
    if(!stock %in% c("SX", "AMEX", "NYSE", "NASDAQ", "EURONEXT", "HKEX", "LSE", "SGX", "SIX", "TSX", "TSXV" ))
      stop(paste(stock, "is not valid"))
  }

  ## Check sort_by
  if(!is.null(sort_by)){
    if(length(sort_by)>1)
      stop(paste("Please specify a sort_by type. eg: relevancy, date, or rank "))

  }

  ## Check sources
  if(!is.null(sources)){
    if(length(sources)>1)
      sources <- paste(sources, collapse = ",")
  }

  ## Check lang
  if(!is.null(lang)){
    if(length(lang)>1){
      stop(paste("Please provide at least a lang"))
    }
    if(!lang %in% c("af", "ar", "bg", "bn", "ca","cn", "cs",
                    "cy", "da", "de", "el", "en", "es", "et",
                    "fa", "fi", "fr", "gu", "he","hi", "hr",
                    "hu", "id", "it", "ja", "kn", "ko", "lt",
                    "lv", "mk", "ml", "mr", "ne", "nl", "no",
                    "pa", "pl", "pt", "ro", "ru", "sk", "sl",
                    "so", "sq", "sv", "sw", "ta", "te", "th",
                    "tl", "tr", "tw", "uk", "ur", "vi")){
      stop(paste('Please specify lang using "af", "ar", "bg", "bn", "ca","cn", "cs",',
                 '"cy", "da", "de", "el", "en", "es", "et",',
                 '"fa", "fi", "fr", "gu", "he","hi", "hr",',
                 '"hu", "id", "it", "ja", "kn", "ko", "lt",',
                 '"lv", "mk", "ml", "mr", "ne", "nl", "no",',
                 '"pa", "pl", "pt", "ro", "ru", "sk", "sl",',
                 '"so", "sq", "sv", "sw", "ta", "te", "th",',
                 '"tl", "tr", "tw", "uk", "ur", or "vi"'))
    }
  }



  ## Check api_key
  if (nchar(api_key) == 0){
    stop(paste("You did not correctly specify your API key neither as global",
               "variable nor with the function call. See documentation for",
               "further info."))
  }

  ## Check api_key
  if (nchar(api_key) == 0){
    stop(paste("You did not correctly specify your API key neither as global",
               "variable nor with the function call. See documentation for",
               "further info."))
  }

  # Query to api ----
  api_host <- "newscatcher.p.rapidapi.com"

  query_params <- list(key_people = key_people,
                       not_sources = not_sources,
                       from = from,
                       to_rank = to_rank,
                       page_size = page_size,
                       from_rank = from,
                       stock = stock,
                       to = to,
                       sort_by = sort_by,
                       sources = sources,
                       page = page,
                       media = media,
                       lang = lang,
                       ticker = ticker
  )

  url <- parse_url(url =  "https://newscatcher.p.rapidapi.com/v1/stocks")
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

  tidy_data <- parsed %>% pluck(6) %>% as_tibble()

  tot_pages <- parsed %>% pluck(4)

  if(get_all){
    if(tot_pages > 1){
      for(i in seq.int(2, tot_pages)){
        query_params <- list(key_people = key_people,
                             not_sources = not_sources,
                             from = from,
                             to_rank = to_rank,
                             page_size = page_size,
                             from_rank = from,
                             stock = stock,
                             to = to,
                             sort_by = sort_by,
                             sources = sources,
                             page = page,
                             media = media,
                             lang = lang,
                             ticker = ticker
        )

        url <- parse_url(url =  "https://newscatcher.p.rapidapi.com/v1/stocks")
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

        temp <- parsed %>% pluck(6) %>% as_tibble()

        tidy_data <- bind_rows(tidy_data, temp)
      }
    }
  }

  print(tot_pages)

  return(tidy_data)

}
