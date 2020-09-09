
#' secure shiny ui
#'
#' @param ui UI of the application.
#' @param ... not implemented
#' @export
secure_ui  <- function(ui, ...) {
  ui <- force(ui)

  function(request) {
    token_from_header <- request$HTTP_TOKEN

    query <- shiny::parseQueryString(request$QUERY_STRING)
    token_from_query <- query$xtoken

    token_from_cookie <- get_cookie(request$HTTP_COOKIE, 'token')

    if (!is.null(token_from_header)) {
      token_query_or_header <- token_from_header
    } else {
      token_query_or_header <- token_from_query
    }

    if (!is.null(token_query_or_header)) {
      token <- token_query_or_header
    } else {
      token <- token_from_cookie
    }

    # show log in page if token is null and login is allowed
    if (is.null(token)) {
      if (getOption('shinycomp.openlogin', TRUE)) {
        return(loginUI(id = "login"))
      } else {
        return("Unauthorized")
      }
    }

    is_valid <- verify_token(token)

    # if token is not valid
    if (!is_valid) {
      return (
        tagList(
          singleton(shiny::tags$head(shiny::tags$script(src = "https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js"))),
          shiny::tags$script("Cookies.remove('token')"),
          shiny::tags$p("token is invalid")
        )
      )
    }


    if (is.function(ui)) {
      ui <- ui(request)
    }
    return (
      tagList(
        singleton(shiny::tags$head(shiny::tags$script(src = "https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js"))),
        shiny::tags$script(paste0("
            Cookies.set(
              'token',
              '", token, "',
              { expires: 1 } // set cookie to expire in 1 day
            );
          ")),
        shiny::tags$div(
          style = "display: none;",
          textInput('token', label = NULL, value = token)
        ),
        waiter::use_waiter(),
        waiter::waiter_show_on_load(waiter::spin_3circles(), color = "#ffffff"),
        shinydisconnect::disconnectMessage(
          text = "您的会话已超时, 刷新页面可重新查看",
          refresh = "现在刷新",
          background = "#f89f43",
          colour = "white",
          overlayColour = "grey",
          overlayOpacity = 0.3,
          refreshColour = "brown"
        ),
        ui
      )
    )

  }

}


#' secure shiny server
#'
#' @param server server of of application
#' @param ... not implemented
#' @export
secure_server <- function(server, ...) {
  server <- force(server)
  function(input, output, session) {
    loginServer(id = "login")
    session$userData$user <- shiny::reactiveVal(NULL)
    shiny::observe({
      user_df <- get_user_df(input$token)
      session$userData$user(user_df)
    })

    server(input, output, session)
    waiter::waiter_hide()
  }
}
