prompt_lm <- function(prompt, endpoint = "http://localhost:1234/v1/completions",
                      api_key = "lm-studio", verbose = F, ...) {
  api_url <- endpoint
  api_key <- api_key
  
  
  headers <- httr::add_headers(
    Content-Type = "application/json",
    Authorization = paste("Bearer", api_key)
  )
  
  
  body <- list(
    prompt = prompt,
    ...
  )
  
  
  response <- httr::POST(
    url = api_url,
    body = jsonlite::toJSON(body),
    encode = "json",
    headers
  )
  
  
  if (httr::status_code(response) == 200) {
    response_content <- httr::content(response, as = "parsed")
    if (verbose) {
      return(response_content)
    } else {
      return(response_content$choices[[1]]$text)
    }
  } else {
    stop("API request failed with status code: ", httr::status_code(response))
  }
}

prompt_lm("Hello!")