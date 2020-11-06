#' Returns document count from specific keywords
#'
#' \code{newscatcher_aggregation} returns document aggregation form specific keyword and filter.
#' Please note for free api you only get 5 articles per call. Free api limit the request per hour up to  21 request/hour
#'
#'
#' @param keyword Character string that contains the searchterm.
#' @param topic The topic to which you want to restrict the articles of your choice. This parameter is experimental.
#' @param ranked_only Logical value to limit the search only for the sources which are in top 1 million online websites. Unranked sources are assigned a rank that equals to 999999.
#' @param sources Character vector with one or more news resources to filter your search. It should be the normal form of the URL, for example, nytimes.com, theguardian.com.
#' @param agg_by How you want to aggregate the articles that math your query. Possible value: day or hour.
#' @param country Country to which you want to narrow your search. This parameter is experimental. We advise you to use it in conjunction with the lang parameter.
#' @param from Character that specify point in time to start the search, E.g. ("2020/05/01", "2020-05-01" ,"2020-05-01 12:54:14", "2020-05-01 4:15am", "yesterday 2:02 am", "2020/05/01 12:55 EST").
#' @param to_rank Upper boundary of the rank of news website to filter by. A number between 0 and 999999.
#' @param lang Specifies the language of the search.
#' @param not_sources One or more sources to be excluded from the search.
#' @param to Until which point in time to search for.
#' @param from_rank Lowest boundary of the rank of news website to filter by. Important: lower rank means that a source is more popular.
#' @param api_key Character string with the API key you get from newsapi.org.
#'                Passing it is compulsory. Alternatively, a function can be
#'                provided from the global environment.
#'
#' @examples
#' \dontrun{df <- newscatcher_aggregation(keyword = "BNI", from = "2020-10-26")}
#'
#' @importFrom purrr map_lgl pluck
#' @importFrom httr GET parse_url add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>% bind_rows
#'
#' @return A tibble
#' @export
newscatcher_aggregation <- function(keyword = NULL,
                                    topic  = NULL,
                                    ranked_only = NULL,
                                    sources   = NULL,
                                    agg_by   = NULL,
                                    country   = NULL,
                                    from  = NULL,
                                    to_rank = NULL,
                                    lang = NULL,
                                    not_sources = NULL,
                                    to      = NULL,
                                    from_rank = NULL,
                                    api_key = Sys.getenv("NEWS_API_KEY")
                                    ) {

  # error and warnings handling -----
  ## Check all argument value
  if(all(map_lgl(list(keyword, topic, ranked_only, sources, sources, agg_by, country, from, to_rank, lang, not_sources, to, from_rank), is.null))){
    stop(paste("Please provide at least keyword"))
  }

  ## Check keyword
  if(!is.null(keyword)){
    if(length(keyword)>1){
      stop(paste("You want to specify more than one keyword? Please check https://www.notion.so/API-Documentation-e15cc61b6c1c4b0a904392f034779653#4193e38e5e1c49f6aec51a5948f636d2 and find Advanced qwith examples"))
    }
  }


  ## Check topic
  if(!is.null(topic)){
    if(!topic %in% c("news", "sport", "tech", "world", "finance", "politics",
                     "business", "economics", "entertainment")){
      stop(paste('topic must be "news", "sport", "tech", "world", "finance", "politics",',
                 '"business", "economics", or "entertainment"'))
    }
    if(length(topic) > 1){
      stop(paste("Please provide only one topic"))
    }
  }


  ## Check ranked_only
  if(!is.null(ranked_only)){
    ranked_only <- ifelse(ranked_only, "True", "False")

  }

  ## Check sources
  if(!is.null(sources)){
    if(length(sources)>1){
      sources <- paste(sources, collapse = ",")
    }
  }

  ## Check agg_by
  if(!is.null(agg_by)){
    if(!agg_by %in% c("day", "hour")){
      stop(paste("Please specify agg_by using day or hour"))
    }
  }

  ## Check country
  if(!is.null(country)){
    if(length(country)>1){
      stop(paste("Please provide only one country"))
    }
  }

  ## Check to_rank
  if(!is.null(to_rank)){
    if(!to_rank %in% c(0:999999)){
      stop(paste("Please provide  a number between 0 and 999999"))
    }
  }

  ## Check lang
  if(!is.null(lang)){
    if(length(lang)>1){
      stop(paste("Please provide at least one lang"))
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

  ## Check not_sources
  if(!is.null(not_sources)){
    if(length(not_sources)>1){
      not_sources <- paste(sources, collapse = ",")
    }
  }

  ## Check from_rank
  if(!is.null(from_rank)){
    if(!from_rank %in% c(0:999999)){
      stop(paste("Please provide  a number between 0 and 999999"))
    }
  }

  ## Check api_key
  if (nchar(api_key) == 0){
    stop(paste("You did not correctly specify your API key neither as global",
               "variable nor with the function call. See documentation for",
               "further info."))
  }

  # Query to api ----
  api_host <- "newscatcher.p.rapidapi.com"

  query_params <- list(topic  = topic,
                       ranked_only = ranked_only,
                       sources   = sources,
                       agg_by   = agg_by,
                       country   = country,
                       from  = from,
                       to_rank = to_rank,
                       lang = lang,
                       not_sources = not_sources,
                       to      = to,
                       from_rank = from_rank,
                       q = keyword
  )

  url <- parse_url(url =  "https://newscatcher.p.rapidapi.com/v1/aggregation")
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

  tidy_data <- parsed %>% pluck(2) %>% as_tibble()

  return(tidy_data)
}



