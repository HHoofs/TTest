# setwd("C:/Users/GettingThingsR/Dropbox/Onderwijs/EPI4906 Advanced statistical techniques/WEEK3")
library(shiny)
# require(markdown)
# library(shinyRGL)

#' Define UI for application that plots random 3d points
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(pageWithSidebar(
  
  
  # Application title
  headerPanel(" ~GZW1006 Experimenteren en interpreteren: t-test",windowTitle="GettingThingsR"),
  
  
  # Sidebar with a slider input for number of points
  sidebarPanel(
    tags$head(
      HTML('<body style="background-color: #00A2DB;">'),
      #              HTML('<body style="background-color: white;">'),
      
#       tags$style(type="text/css", "select { width: 380px; }"),
      tags$style(type="text/css", "select { color: #001C3D }"),
      tags$style(type="text/css", "textarea { max-width: 365px; color: #001C3D}"),
      tags$style(type="text/css", ".jslider { max-width: 380px; color: #001C3D}"),
      tags$style(type="text/css", ".jslider .jslider-label{color: #001C3D ; font-size: 12px;}"),
      tags$style(type="text/css", ".jslider .jslider-value{color: #001C3D ; font-weight:bold; font-size: 16px;}"),      
      tags$style(type='text/css', ".well { max-width: 380px; }"),
      tags$style(type='text/css', ".span4 .well { background-color: white; border-color: #00A2DB}"),
#       tags$style(type='text/css', ".span4 { max-width: pink; border-color: #00A2DB}"),
      tags$style(type='text/css', ".span12  { color: #001C3D; }"),
      tags$style(type='text/css', ".span4  { color: #001C3D; }") ,   

# tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
tags$style(type="text/css", "select { max-width: 200px; }"),
tags$style(type="text/css", "textarea { max-width: 185px; }"),
tags$style(type="text/css", ".jslider { max-width: 200px; }"),
tags$style(type='text/css', ".well { max-width: 310px; }"),
tags$style(type='text/css', ".span4 { max-width: 310px; }")
      
      #         tags$style(type='text/css', ".span8 .tabbable { background-color: #E84E10; }")
      
      
      
    ),
    #   sliderInput("alpha","alpha",min=.001,max=1,value=.05,step=.001),
    selectInput("alpha","alpha",c(.001,.01,.05,.1)),
    radioButtons("sided", "Richting Hypothese",
                 list("H0: μ = ..." = "=",
                      "H0: μ ≥ ..." = "<",
                      "H0: μ ≤ ..." = ">")),

#     selectInput("sided","Richting Hypothese",c("=","<",">")),
    wellPanel(checkboxInput("ttest",strong("One Sample T-Test"),value=TRUE),
              conditionalPanel(condition = paste("input.ttest == true"),
                               sliderInput("one_u", "u",min=0, max=10, value=7, step=.1),
                               sliderInput("one_X", "X",min=0, max=10, value=7, step=.1),
                               sliderInput("one_sd", "s",min=0, max=12, value=1, step=.1),
                               sliderInput("one_N", "N",min=0, max=2000, value=100, step=1)
              )),
    wellPanel(checkboxInput("options",strong("Opties"),value=FALSE),
              conditionalPanel(condition = paste("input.options == true"),  
                               sliderInput("lim", "Bereik",min=0, max=20, value=5, step=1),
#                                checkboxInput("answ1", "Toetsing",value=FALSE),
                               checkboxInput("answ2", "Concluse",value=FALSE)
                               
              )),
    
    
    HTML('<a href="http://www.maastrichtuniversity.nl/" target="_blank"><img src="logo.jpg" alt="Maastricht University"  border="0" style="border: #00A2DB solid 1px; border-radius: 5px;"/></a>')
    #     img(src="logo.jpg", height = 1245, width = 853)
  ),
  
  # Show the generated 3d scatterplot
  mainPanel(
    tags$head(
      tags$style(type="text/css", "li a{color: #001C3D; font-weight:bold; background-color:white;}")
#       tags$style(type='text/css', '#version {font-family: Lucida Console;background-color:  white; color: :#001C3D;  font-size: 20px}'),
#       tags$style(type='text/css', '#summar {font-family: Lucida Console;background-color:  white; color: :#001C3D;  font-size: 20px}'),
#       tags$style(type='text/css', '#quincunx {float: left;font-family: Lucida Console;background-color:  white; color: :#001C3D;  font-size: 0px}') 
      
    ),
    tabsetPanel(tabPanel("Waardes",plotOutput("hypopaar",width="800px",height="600px")),           
                tabPanel("Kritieke gebied",plotOutput("plot",width="800px",height="600px"),plotOutput("plotH",width="800px",height="300px")),
                tabPanel("p-waarde",plotOutput("plot2",width="800px",height="600px"),plotOutput("plot3",width="800px",height="300px")),
                tabPanel("Betrouwbaarheids Interval",plotOutput("CI",width="800px",height="600px"),plotOutput("CI_info",width="800px",height="300px")),
                
                tabPanel("Overzicht",verbatimTextOutput("summar")),
                tabPanel("Version (C)",verbatimTextOutput("version"))
                #     verbatimTextOutput("summary")
    )
  )))
