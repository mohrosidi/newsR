#' Returns document from specific keywords
#'
#' \code{newscatcher_searchfree} returns document form specific keyword and filter.
#' The function can return up to 100 document per call despite using free api.
#'
#'
#' @param keyword Character string that contains the searchterm.
#' @param ranked_only Logical value to limit the search only for the sources which are in top 1 million online websites. Unranked sources are assigned a rank that equals to 999999.
#' @param page_size How many articles to return per page. Defaults to 50, max is 100.
#' @param media Adds to the output of the call two more variables: media and media_content.
#'
#'             * media - the main image published with an article
#'             * media_content - a comma-separated string of all images used in an article
#' @param lang Logical value. Specifies the language of the search.
#' @param page The number of the page. Use it to scroll through the results. Defaults to 1.
#' @param get_all Logical value. If all pages will be collected then gel_all is TRUE.
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

newscatcher_searchfree <- function(keyword      = NULL,
                                   ranked_only  = NULL,
                                   page_size    = 50,
                                   media        = TRUE,
                                   lang         = NULL,
                                   page         = 1,
                                   get_all      = FALSE,
                                   api_key = Sys.getenv("NEWS_API_KEY")

){


  # error and warning handling -----
  if(all(map_lgl(list(keyword, ranked_only, lang), is.null))){
    stop(paste("Please provide at least keyword"))
  }

  ## Check keyword
  if(!is.null(keyword)){
    if(length(keyword)>1){
      stop(paste("You want to specify more than one keyword? Please check https://www.notion.so/API-Documentation-e15cc61b6c1c4b0a904392f034779653#4193e38e5e1c49f6aec51a5948f636d2 and find Advanced qwith examples"))
    }
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

  # Query to api ----
  api_host <- "newscatcher.p.rapidapi.com"

  query_params <- list(ranked_only  = ranked_only,
                       page_size    = page_size,
                       media        = media,
                       lang         = lang,
                       page         = 1,
                       q            = keyword
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

  tidy_data <- parsed %>% pluck(6) %>% as_tibble()

  tot_pages <- parsed %>% pluck(4)

  if(get_all){
    if(tot_pages > 1){
      for(i in seq.int(2, tot_pages)){
        query_params <- list(ranked_only  = ranked_only,
                             page_size    = page_size,
                             media        = media,
                             lang         = lang,
                             page         = i,
                             q            = keyword
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

  return(tidy_data)

}
