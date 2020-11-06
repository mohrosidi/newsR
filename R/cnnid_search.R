#' Get resources of CNN Indonesia
#'
#' \code{cnnid_search} returns from CNN Indonesia based on keyword that we specify.
#'
#' @param keyword Character string that contains the searchterm for the API's
#'                data base.
#' @examples
#' \dontrun{
#' df <- cnnid_search(keyword = "jokowi")
#' }
#' @importFrom httr content GET
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_dfr pluck
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>% select pull left_join
#' @return A tibble
#' @export


cnnid_search <- function(keyword){

  link <- NULL

  # error and warning handling -----
  if(length(keyword) > 1){
    stop(paste("You can specify a keyword"))
  }

  if(!is.character(keyword)){
    stop(paste("keyword must be character"))
  }

  # connect to api ----
  keyword <- tolower(keyword)
  url <- paste0("https://www.news.developeridn.com/search/?q=", keyword)

  resp <- GET(url)

  # parse the data ----
  parsed <- fromJSON(
    content(
      resp, as = "text", type = "application/json", encoding = "UTF-8"
    ),
    flatten = TRUE
  )

  result <- parsed %>% pluck(1)

  # get news details ----
  vec <- result %>%
    select(link) %>%
    pull()

  tidy_data <- map_dfr(vec, function(x){

    url <- paste0("https://www.news.developeridn.com/detail/?url=", x)
    resp <- GET(url)
    parsed <- fromJSON(
      content(
        resp, as = "text", type = "application/json", encoding = "UTF-8"
      ),
      flatten = TRUE
    )

    result <- parsed %>% pluck(1)

    return(result)

  }) %>% as_tibble() %>%
    left_join(result, by = "judul")



  return(tidy_data)

}


