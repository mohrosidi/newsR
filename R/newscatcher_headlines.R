#' Returns lastest headlinse from specific filter
#'
#' \code{newscatcher_headlines} returns document of latest headlines form specific filter.
#'
#'
#' @param topic The topic to which you want to restrict the articles of your choice. This parameter is experimental.
#' @param lang Specifies the language of the search.
#' @param country Country to which you want to narrow your search. This parameter is experimental. We advise you to use it in conjunction with the lang parameter.
#' @param api_key Character string with the API key you get from newsapi.org.
#'                Passing it is compulsory. Alternatively, a function can be
#'                provided from the global environment.
#' @param media Adds to the output of the call two more variables: media and media_content.
#'
#'             * media - the main image published with an article
#'             * media_content - a comma-separated string of all images used in an article
#'
#' @examples
#' \dontrun{df <- newscatcher_headlines(topic = "news", country = "id")}
#'
#' @importFrom purrr map_lgl pluck
#' @importFrom httr GET parse_url add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>% bind_rows
#'
#' @return A tibble
#' @export
newscatcher_headlines <- function(topic = NULL,
                                  lang = NULL,
                                  country = NULL,
                                  media = TRUE,
                                  api_key = Sys.getenv("NEWS_API_KEY")){

  # error and warning handling ----
  if(all(map_lgl(list(topic, lang, country), is.null))){
    stop(paste("Please specify at least topic, lang, country"))
  }

  ## Check topic
  if(!is.null(topic)){
    if(length(topic)>1){
      stop(paste("Please provide at least a topic"))
    }
    if(!topic %in%  c("news", "sport", "tech", "world", "finance", "politics",
                      "business", "economics", "entertainment")){
      stop(paste('topic must be "news", "sport", "tech", "world", "finance", "politics",',
                 '"business", "economics", or "entertainment"'))
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

  ## Check country
  if(!is.null(country)){
    if(length(country)>1){
      stop(paste("Please provide only one country"))
    }
  }

  ## Make sure an API key is provided
  if (nchar(api_key) == 0)
    stop(
      paste0(
        "You did not specify your API key as an argument or as a global variable.",
        " See documentation for further info."
      )
    )

  # Query to api ----
  api_host <- "newscatcher.p.rapidapi.com"

  query_params <- list(topic = topic,
                       lang = lang,
                       country = country,
                       media = media
  )

  url <- parse_url(url =  "https://newscatcher.p.rapidapi.com/v1/latest_headlines")
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
