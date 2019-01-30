#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)

rmdfiles <- c("poweranalysisanova.rmd")
sapply(rmdfiles, knit, quiet = T)

ui <- shinyUI(
  fluidPage(
    includeMarkdown("poweranalysisanova.md")
  )
)
server <- function(input, output) { }

shinyApp(ui, server)
