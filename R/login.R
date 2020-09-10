#' login UI module
#'
#' Call via \code{loginUI("your_id")}
#'
#' @param id Shiny id
#' @param title header title for the login panel
#' @param user_title label for the user name text input
#' @param pass_title label for the password text input
#' @param login_title label for the login button
#' @param error_message error messsage when failed.
#'
#' @return Shiny UI
#'
#' @export
loginUI <- function(id, title = "请登录", user_title = "用户名", pass_title = "密码",
                    login_title = "登录", error_message="用户名或密码错误") {
  ns <- shiny::NS(id)

  jsCode <- 'shinyjs.settokencookie = function(token) {
    Cookies.set("token", escape(token), { expires: 1 }); // set cookie to expire in 1 day
  }
  shinyjs.rmtokencookie = function() {
    Cookies.remove("token");
  }
  '

  shiny::div(id = ns("panel"), style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
             shiny::fluidPage(
               shiny::wellPanel(
                 htmltools::singleton(shiny::tags$head(shiny::tags$script(src = "https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js"))),
                 shinyjs::useShinyjs(),
                 shinyjs::extendShinyjs(text = jsCode),
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

  )
}


#' login server module
#'
#' @param id Shiny id
#'
#' @export
loginServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$button, {
      username <- input$user_name
      password<- input$password
      resp <- httr::POST(
        url = getOption('shinycomp.login_url', 'http://fastapi/api/v1/login/access-token'),
        httr::accept_json(),
        body = list(username= username, password=password),
        encode = "form"
        )

      if (httr::status_code(resp) == 200) {
        access_token <- httr::content(resp)$access_token
        resp_me <- httr::GET(
          url = getOption('shinycomp.get_users_me_url', 'http://fastapi/api/v1/users/me'),
          httr::add_headers(Authorization = paste("Bearer", access_token, sep = " "))
        )
        userid <- httr::content(resp_me)$external_id
        resp_apitoken <- httr::GET(
          url = getOption('shinycomp.get_apitoken_url', 'http://fastapi/api/v1/get_apitoken'),
          httr::accept_json(),
          httr::add_headers(Authorization = paste("Bearer", access_token, sep = " ")),
          query = list(external_id = userid)
        )
        token <- jsonlite::fromJSON(
          httr::content(resp_apitoken, "text", encoding="UTF-8"),
          simplifyDataFrame  = TRUE
          )$ApiToken
        print('token')
        print(token)
        shinyjs::js$settokencookie(token)
        remove_query_string()
        session$reload()
      } else {
        shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
        shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
      }
    })


  })
}
