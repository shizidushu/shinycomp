#' Set function default arguments
#'
#' @param func Function
#' @param recursive Recursively merge defaults with new arguments or not
#' @param ... Default arguments
#'
#' @export
set_func_defaults <- function(func, recursive = FALSE, ...) {
  cl <- as.call(c(list(quote(func)), list(...)))
  cl <- match.call(func,cl)
  args <- as.list(cl)[-1]
  newfunc <- function(...) {
    new_cl <- as.call(c(list(quote(func)), list(...)))
    new_cl <- match.call(func,new_cl)
    new_args <- as.list(new_cl)[-1]
    if (recursive) {
      new_args <- utils::modifyList(args, new_args, keep.null = TRUE)
    }  else {
      keep_args <- setdiff(names(args), names(new_args))
      new_args <- c(args[keep_args], new_args)
    }
    do.call(func, new_args)
  }
}


#' Remove the URL query
#'
#' Remove the entire query string from the url.  This function should only be called
#' inside the server function of your 'shiny' app.
#'
#' @param session the Shiny session
#'
remove_query_string <- function(session = shiny::getDefaultReactiveDomain()) {
  shiny::updateQueryString(
    session$clientData$url_pathname,
    mode = "replace",
    session = session
  )
}

#' Get a cookie value by name from a cookie string
#'
#' @param cookie_string the cookie string
#' @param name the name of the cookie
#'
#' @export
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
  resp <- httr::GET(
    url = getOption('shinycomp.get_userinfo_url', 'http://fastapi/api/v1/get_user_info_by_apitoken'),
    query = list(apitoken = token)
    )
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


