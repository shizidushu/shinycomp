
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinycomp

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/shinycomp)](https://CRAN.R-project.org/package=shinycomp)
<!-- badges: end -->

This is a simple auth for shiny. Checks token from header or query or
cookie.

## Installation

You can install the released version of shinycomp from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("shinycomp")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shizidushu/shinycomp")
```

## Example

``` r
library(shiny)
library(shinycomp)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$distPlot <- renderPlot({
        print(session$userData$user)
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}


ui <- secure_ui(ui)
server <- secure_server(server)

# Run the application
shinyApp(ui = ui, server = server)
```
