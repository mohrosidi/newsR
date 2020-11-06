#' Get resources of CNN Indonesia
#'
#' \code{cnnid_search} returns from CNN Indonesia based on category that we specify.
#'
#' @param category Character string that contains the category for the API's
#'                data base.
#' @examples
#' \dontrun{
#' df <- cnn_category(category = "nasional")
#' }
#' @importFrom httr content GET
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_dfr pluck
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>% select pull left_join case_when
#' @return A tibble
#' @export
cnnid_category <- function(category = NULL){
  link <- NULL
  # error and warning handling ------
  if(!is.character(category)){
    stop(paste("category must be a character"))
  }

  if(!category %in% c("nasional", "international", "ekonomi", "olahraga",
                      "teknologi", "hiburan", "gaya hidup")){
    stop(paste(category, 'is not valid. Please specify the category as "nasional", "international", "ekonomi", "olahraga",',
                      '"teknologi", "hiburan", "gaya hidup"'))
  }


  url <- case_when(category == "nasional" ~ "https://www.news.developeridn.com/nasional",
                   category == "international" ~ "https://www.news.developeridn.com/internasional",
                   category == "ekonomi" ~ "https://www.news.developeridn.com/ekonomi",
                   category == "olahraga" ~ "https://www.news.developeridn.com/olahraga",
                   category == "teknologi" ~ "https://www.news.developeridn.com/teknologi",
                   category == "hiburan" ~ "https://www.news.developeridn.com/hiburan",
                   TRUE ~ "https://www.news.developeridn.com/gaya-hidup")
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
