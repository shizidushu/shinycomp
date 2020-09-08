#' login UI module
#'
#' Shiny UI Module for use with \link{login}
#'
#' Call via \code{loginUI("your_id")}
#'
#' @param id Shiny id
#' @param title header title for the login panel
#' @param user_title label for the user name text input
#' @param pass_title label for the password text input
#' @param login_title label for the login button
#'
#' @return Shiny UI
#'
#' @export
loginUI <- function(id, title = "请登录", user_title = "用户名", pass_title = "密码",
                    login_title = "登录", error_message="用户名或密码错误") {
  ns <- shiny::NS(id)

  shiny::div(id = ns("panel"), style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
             shiny::wellPanel(
               shiny::tags$h2(title, class = "text-center", style = "padding-top: 0;"),

               shiny::textInput(ns("user_name"), shiny::tagList(shiny::icon("user"), user_title)),

               shiny::passwordInput(ns("password"), shiny::tagList(shiny::icon("unlock-alt"), pass_title)),

               shiny::div(
                 style = "text-align: center;",
                 shiny::actionButton(ns("button"), login_title, class = "btn-primary", style = "color: white;")
               ),

               shinyjs::hidden(
                 shiny::div(id = ns("error"),
                            shiny::tags$p(error_message,
                                          style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center"))
               )
             )
  )
}



loginServer <- function(id, log_out = NULL) {
  moduleServer(id, function(input, output, session) {

    shiny::observeEvent(log_out(), {

      shiny::updateTextInput(session, "password", value = "")
    })

    shiny::observeEvent(input$button, {
      username <- input$user_name
      password<- input$password
      username <- 'simon'
      password <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHRlcm5hbF9pZCI6NzR9.rAm41MQ15YkNgHP8FzhdmsKgmtiJw03jnJRe1WQBJZg"
      resp <- httr::POST(
        url = getOption('shinycomp.login_url', 'http://fastapi/api/v1/login/access-token'),
        httr::accept_json(),
        body = list(username= username, password=password), encode = "form")
      if (httr::status_code(resp) == 200) {
        token <- content(resp)$access_token
        # write token to cookie?
      } else {
        shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
        shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
      }
    })


  })
}
