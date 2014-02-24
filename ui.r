library(shiny)
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel(" ~GZW1006 Experimenteren en interpreteren: t-test",windowTitle="GettingThingsR"),
  
  # Sidebar with a slider input for number of points
  sidebarPanel(
    # HTML opmaak e.d.
    tags$head(
      HTML('<body style="background-color: #00A2DB;">'),
      tags$style(type="text/css", "select { color: #001C3D }"),
      tags$style(type="text/css", "textarea { max-width: 365px; color: #001C3D}"),
      tags$style(type="text/css", ".jslider { max-width: 380px; color: #001C3D}"),
      tags$style(type="text/css", ".jslider .jslider-label{color: #001C3D ; font-size: 12px;}"),
      tags$style(type="text/css", ".jslider .jslider-value{color: #001C3D ; font-weight:bold; font-size: 16px;}"),      
      tags$style(type='text/css', ".well { max-width: 380px; }"),
      tags$style(type='text/css', ".span4 .well { background-color: white; border-color: #00A2DB}"),
      tags$style(type='text/css', ".span12  { color: #001C3D; }"),
      tags$style(type='text/css', ".span4  { color: #001C3D; }") ,   
      tags$style(type="text/css", "select { max-width: 200px; }"),
      tags$style(type="text/css", "textarea { max-width: 185px; }"),
      tags$style(type="text/css", ".jslider { max-width: 200px; }"),
      tags$style(type='text/css', ".well { max-width: 310px; }"),
      tags$style(type='text/css', ".span4 { max-width: 310px; }")
    ),
    # Keuze alpha
    selectInput("alpha",strong(HTML("&alpha;:")),c(.01,.05,.1),selected=.05),
    # Keuze nulhypothese
    radioButtons("sided",strong("Richting Nul Hypothese"),
                 list("H0: μ = ..." = "=",
                      "H0: μ ≥ ..." = "<",
                      "H0: μ ≤ ..." = ">")),
    # Keuze ttest 
    wellPanel(checkboxInput("ttest",strong("t-toets voor één steekproef"),value=TRUE),
              conditionalPanel(condition = paste("input.ttest == true"),
                               # Populatie gemiddelde
                               sliderInput("one_u", "u (populatiegemiddelde)",min=0, max=10, value=7, step=.1),
                               # Steekproefgemiddelde
                               sliderInput("one_X", "X (steekproefgemiddelde)",min=0, max=10, value=6.9, step=.1),
                               # Standaard deviatie
                               sliderInput("one_sd", "s (standaard deviatie)",min=0, max=12, value=1, step=.1),
                               # Groepsgrootte
                               sliderInput("one_N", "N (steekproefgrootte)",min=0, max=2000, value=42, step=1)
              )),
    wellPanel(checkboxInput("twottest",strong("t-toets voor twee steekproeven"),value=FALSE),
              conditionalPanel(condition = paste("input.twottest == true"),
                               # Steekproefgemiddelde
                               sliderInput("two_X1", "X1 (gemiddelde groep 1)",min=0, max=10, value=6.9, step=.1),
                               # Standaard deviatie
                               sliderInput("two_sd1", "s1 (standaard deviatie groep 1)",min=0, max=12, value=1, step=.1),
                               # Groepsgrootte
                               sliderInput("two_N1", "N (steekproefgrootte groep 1)",min=0, max=2000, value=42, step=1),
                               # Steekproefgemiddelde
                               sliderInput("two_X2", "X1 (gemiddelde groep 2)",min=0, max=10, value=6.9, step=.1),
                               # Standaard deviatie
                               sliderInput("two_sd2", "s1 (standaard deviatie groep 2)",min=0, max=12, value=1, step=.1),
                               # Groepsgrootte
                               sliderInput("two_N2", "N (steekproefgrootte groep 2)",min=0, max=2000, value=42, step=1)
              )),
    # Opties
    wellPanel(checkboxInput("options",strong("Opties"),value=TRUE),
              conditionalPanel(condition = paste("input.options == true"),
                               # Bereik grafieken
                               sliderInput("lim", "Bereik",min=0, max=20, value=5, step=1),
                               # Antwoord?
                               checkboxInput("answ2", "Conclusie",value=FALSE)               
              )),
    
    # Plaatje
    HTML('<a href="http://www.maastrichtuniversity.nl/" target="_blank"><img src="logo.jpg" alt="Maastricht University"  border="0" style="border: #00A2DB solid 1px; border-radius: 5px;"/></a>')
  ),
  
  # Show the generated 3d scatterplot
  mainPanel(
    # Opmaakt html
    tags$head(
      tags$style(type="text/css", "li a{color: #001C3D; font-weight:bold; background-color:white;}")
    ),
    # Verschillende tabPanels
    tabsetPanel(
      tabPanel("Info",HTML("<P STYLE='background-color: white;'>Deze applicatie laat de drie methodes zien waarmee je kan toetsen of een gevonden steekproefgemiddelde overeenkomt
met het populatiegemiddelde (gegeven de hypotheses) of dat twee steekproefgemiddelde uit dezelfde populatie komen. Het tabblad 'waardes' geeft alle relevante informatie en berekend
de t-waarde. Propeer deze t-waarde ook een paar keer zelf te berekenen en controleer of het klopt. De volgende drie
tabbladen laten de drie methodes zien waarmee je kan bepalden welke hypothese je moet aannemen dan wel verwerpen. Probeer
eerst zelf te bedenken welke hypothese je aanneemt en druk vervolgens op conclusie (onder opties), om te controleren of dit
correct is. Het laatste tabblad ('t-tabel') laat zien in welke rijen en welke kolom je moet kijken in de tabel op pagina 306
van Imbos et al. Aan de linkse kant kan je de waardes veranderen, de alpha en de richting van de nulhypothese (en dus ook de
alternatieve). Veel succes en probeer alle verschillende methodes eens uit en kijk (of je weet) wat er gebeurd als je
waardes veranderd. </P>")),
      tabPanel("Waardes",plotOutput("hypopaar",width="800px",height="700px")),           
      tabPanel("Kritieke gebied",plotOutput("plot",width="800px",height="600px"),plotOutput("plotH",width="800px",height="300px")),
      tabPanel("p-waarde",plotOutput("plot2",width="800px",height="600px"),plotOutput("plot3",width="800px",height="300px")),
      tabPanel("Betrouwbaarheids Interval",plotOutput("CI",width="800px",height="600px"),plotOutput("CI_info",width="800px",height="300px")), 
      tabPanel("T Tabel",tableOutput("dataset"),plotOutput("valuett",width="600px",height="200px")),
      tabPanel("Version (C)",verbatimTextOutput("version"),verbatimTextOutput("author"),verbatimTextOutput("licence"))
      
    )
  )))
