# /* answer for  https://stackoverflow.com/q/49439096/1104685 */
#

library(qwraps2)
options(qwraps2_markup = "markdown")
library(shiny)
library(dplyr)
library(knitr) # for knit2html

ui <- fluidPage( tabsetPanel(
  tabPanel("Summary",
           # mainPanel(tableOutput('summarytab'))
           mainPanel(uiOutput('summarytab'))
  )
)
)

server <- function(input, output){

  output$summarytab <- renderUI({

    our_summary1 <-
      list("Miles per gallon" =
             list("min" = ~ min(mpg),
                  "max" = ~ max(mpg),
                  "mean (sd)" = ~ qwraps2::mean_sd(mpg)),
           "Cylinder" =
             list("min" = ~ min(cyl),
                  "max" = ~ max(cyl),
                  "mean (sd)" = ~ qwraps2::mean_sd(cyl))

      )
    stable <- summary_table(filter(mtcars),our_summary1)
    HTML(knit2html(text=capture.output(stable), fragment.only=TRUE))
    # do not forget 'capture.output'
  })

}

shinyApp(ui = ui, server = server)
