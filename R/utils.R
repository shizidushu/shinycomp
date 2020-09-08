#' Get a cookie value by name from a cookie string
#'
#' @param cookie_string the cookie string
#' @param name the name of the cookie
#'
get_cookie <- function(cookie_string, name) {
  cookies <- unlist(strsplit(cookie_string , split = "; ", fixed = TRUE))
  cookies <- lapply(cookies, function(x) {unlist(strsplit(x, split="=", fixed = TRUE))})

  cookies <- Reduce(function(acc, x) {
    val <- x[[2]]
    names(val) <- x[[1]]
    c(acc,val)
  },cookies,list())

  value <- cookies[[name]]
  if (rlang::is_empty(value)) {
    return(NULL)
  } else {
    return(value)
  }
}

#' Get user information as dataframe
#'
#' @param token token to be verified
get_user_df <- function(token) {
  resp <- httr::GET(getOption('shinycomp.verify_token_api_url', 'http://fastapi/api/v1/verify_xtoken'), query = list(xtoken = token))
  df <- jsonlite::fromJSON(httr::content(resp, "text", encoding="UTF-8"), simplifyDataFrame  = TRUE)
  df
}

#' check token is valid or not from server
#'
#' @param token token to be verified
verify_token <- function (token) {
  user <- get_user_df(token)
  if (identical(user, list())) {
    FALSE
  } else {
    TRUE
  }
}