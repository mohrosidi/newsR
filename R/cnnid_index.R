#' Get random resources of CNN Indonesia
#'
#' \code{cnnid_search} returns random news from CNN Indonesia.
#'
#' @examples
#' \dontrun{
#' df <- cnnid_index()
#' }
#' @importFrom httr content GET
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_dfr pluck
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>% select pull left_join
#' @return A tibble
#' @export


cnnid_index <- function(){

  link <- NULL

  url <- "https://www.news.developeridn.com/"

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

    Sys.sleep(2)

    return(result)

  }) %>% as_tibble() %>%
    left_join(result, by = "judul")



  return(tidy_data)
}
