library(shiny)
library(DT)
library(data.table)
library(lubridate)
library(shinyWidgets)
library(shinyalert)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Data Entry"),
  #setBackgroundColor("#292929"
  # ),
  ### This is to adjust the width of pop up "showmodal()" for DT modify table 
  tags$head(tags$style(HTML('

                            .modal-lg {
                            width: 1200px;
                            }
                            '))),
  helpText("Note: Remember to save any updates!"),
  br(),
  ### tags$head() is to customize the download button
  tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
  downloadButton("Trich_csv", "Download CSV", class="butt"),
  uiOutput("MainBody_trich"),actionButton(inputId = "Updated_trich",label = "Save")
))