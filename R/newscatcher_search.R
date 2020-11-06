#' Returns document from specific keywords
#'
#' \code{newscatcher_search} returns document form specific keyword and filter.
#' Free api can only return 5 document, so make sure to specify page_size = 5.
#' Another things to remember is free api can only make 21 request/hour.
#' If you want to retrive another document more than 5 document please write a loop that only return 5 document/ request and sleep at least one hour after 21 request.
#'
#'
#' @param keyword Character string that contains the searchterm.
#' @param ranked_only Logical value to limit the search only for the sources which are in top 1 million online websites. Unranked sources are assigned a rank that equals to 999999.
#' @param search_in By default, the function search what you specified in keyword argument in both title and summary of the article. However, you can limit this to either title or summary
#' @param not_sources One or more sources to be excluded from the search.
#' @param sort_by A character that specify sort method. Possible value:
#'
#'                * "relevancy" — the most relevant results first,
#'                * "date" — the most recently published results first,
#'                * "rank" — the results from the highest-ranked sources first
#' @param sources Character vector with one or more news resources to filter your search. It should be the normal form of the URL, for example, nytimes.com, theguardian.com.
#' @param to_rank Upper boundary of the rank of news website to filter by. A number between 0 and 999999.
#' @param to Until which point in time to search for.
#' @param from Character that specify point in time to start the search, E.g. ("2020/05/01", "2020-05-01" ,"2020-05-01 12:54:14", "2020-05-01 4:15am", "yesterday 2:02 am", "2020/05/01 12:55 EST").
#' @param topic The topic to which you want to restrict the articles of your choice. This parameter is experimental.
#' @param page_size How many articles to return per page. Defaults to 50, max is 100.
#' @param lang Logical value. Specifies the language of the search.
#' @param country Country to which you want to narrow your search. This parameter is experimental. We advise you to use it in conjunction with the lang parameter.
#' @param page The number of the page. Use it to scroll through the results. Defaults to 1.
#' @param from_rank Lowest boundary of the rank of news website to filter by. Important: lower rank means that a source is more popular.
#' @param get_all Logical value. If all pages will be collected then gel_all is TRUE.
#' @param api_key Character string with the API key you get from newsapi.org.
#'                Passing it is compulsory. Alternatively, a function can be
#'                provided from the global environment.
#'
#' @examples
#' \dontrun{
#' df <- newscatcher_search(keyword = "BNI", page_size = 5)}
#'
#' @importFrom purrr map_lgl pluck
#' @importFrom httr GET parse_url add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>% bind_rows
#'
#' @return A tibble
#' @export

newscatcher_search <- function(keyword = NULL,
                               ranked_only = NULL,
                               search_in = NULL,
                               not_sources = NULL,
                               sort_by = "relevancy",
                               sources = NULL,
                               to_rank = NULL,
                               to = NULL,
                               from = NULL,
                               topic = NULL,
                               page_size = 100,
                               lang = NULL,
                               country = NULL,
                               page = 1,
                               from_rank = NULL,
                               get_all = FALSE,
                               api_key = Sys.getenv("NEWS_API_KEY")){

  # error and warning handling -----
  if(all(map_lgl(list(keyword, ranked_only, search_in, not_sources, sources, to_rank, to, from, topic, lang, country, from_rank), is.null)))
    stop(paste("Please specify at least keyword"))

  ## Check search_in
  if(!is.null(search_in)){
    if(length(search_in)>1){
      stop(paste("Please specify a search_in"))
    if(!search_in %in% c("title", "summary"))
      stop(paste("Please provide title or summary"))
    }
  }

  ## Check not_sources
  if(!is.null(not_sources)){
    if(length(not_sources)>1)
      not_sources <- paste(not_sources, collapse = ",")
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

  ## Check to_rank
  if(!is.null(to_rank)){
    if(!to_rank %in% c(0:999999)){
      stop(paste("Please provide  a number between 0 and 999999"))
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

  ## Check page_size
  if(!is.null(page_size)){
    if(page_size > 100)
      stop(paste("Maximum page_size is 100"))
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

  ## Check api_key
  if (nchar(api_key) == 0){
    stop(paste("You did not correctly specify your API key neither as global",
               "variable nor with the function call. See documentation for",
               "further info."))
  }

  # Query to api ----
  api_host <- "newscatcher.p.rapidapi.com"

  query_params <- list(ranked_only = ranked_only,
                       search_in = search_in,
                       not_sources = not_sources,
                       sort_by = sort_by,
                       sources = sources,
                       to_rank = to_rank,
                       to = to,
                       from = from,
                       topic = topic,
                       page_size = page_size,
                       lang = lang,
                       country = country,
                       page = page,
                       from_rank = from_rank,
                       q = keyword
  )

  url <- parse_url(url =  "https://newscatcher.p.rapidapi.com/v1/search")
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
        query_params <- list(ranked_only = ranked_only,
                             search_in = search_in,
                             not_sources = not_sources,
                             sort_by = sort_by,
                             sources = sources,
                             to_rank = to_rank,
                             to = to,
                             from = from,
                             topic = topic,
                             page_size = page_size,
                             lang = lang,
                             country = country,
                             page = i,
                             from_rank = from_rank,
                             q = keyword
        )

        url <- parse_url(url =  "https://newscatcher.p.rapidapi.com/v1/search_free")
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
