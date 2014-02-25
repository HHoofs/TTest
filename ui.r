# The MIT License (MIT)
# 
# Copyright (c) 2014 Huub
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the 'Software' ), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

library(shiny)
shinyUI(
  pageWithSidebar(
    
    # Application title
    headerPanel(" ~GZW1006 Experimenteren en interpreteren: t-test",windowTitle="GettingThingsR"),
    # Sidebar panel --------------------------------------------------------------
    sidebarPanel(
      # HTML opmaak e.d.
      tags$head(
        HTML('<body style="background-color: #00A2DB;">'),
        #         tags$style(type = "text/css", "a{color: #f50000;}"),
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
      radioButtons("sided",strong("Richting Nul Hypothese:"),
                   list("H0: μ = ..." = "=",
                        "H0: μ ≥ ..." = "<",
                        "H0: μ ≤ ..." = ">")
      ),
      # Keuze ttest 
      radioButtons("test",strong("Soort test:"),
                   list("t-toets voor één steekproef" = "onettest",
                        "t-toets voor twee steekproeven" = "twottest"
                   )
      ),
      # uitewerking testen
      wellPanel(
        # Een sample t-test
        conditionalPanel(condition = paste("input.test == 'onettest'"),
                         # Populatie gemiddelde
                         sliderInput("one_u", "u (populatiegemiddelde)",min=0, max=10, value=7, step=.1),
                         # Steekproefgemiddelde
                         sliderInput("one_X", "X (steekproefgemiddelde)",min=0, max=10, value=6.9, step=.1,animate=TRUE),
#                          sliderInput("one_X", "X (steekproefgemiddelde)",min=1, max=10, value=6, step=1,animate=TRUE),
                         # Standaard deviatie
                         sliderInput("one_sd", "s (standaard deviatie)",min=0, max=12, value=1, step=.1),
#                          sliderInput("one_sd", "s (standaard deviatie)",min=1, max=12, value=1, step=1),
                         # Groepsgrootte
                         sliderInput("one_N", "N (steekproefgrootte)",min=0, max=2000, value=42, step=1)
#                          sliderInput("one_N", "N (steekproefgrootte)",min=1980, max=2000, value=1989, step=1)
        ),
        # Twee sample t-test
        conditionalPanel(condition = paste("input.test == 'twottest'"),
                         # Steekproefgemiddelde
                         sliderInput("two_X1", "X1 (gemiddelde groep 1)",min=0, max=10, value=6.9, step=.1),
                         # Standaard deviatie
                         sliderInput("two_sd1", "s1 (standaard deviatie groep 1)",min=0, max=12, value=1, step=.1),
                         # Groepsgrootte
                         sliderInput("two_N1", "N (steekproefgrootte groep 1)",min=0, max=2000, value=42, step=1),
                         # Steekproefgemiddelde
                         sliderInput("two_X2", "X2 (gemiddelde groep 2)",min=0, max=10, value=6.9, step=.1),
                         # Standaard deviatie
                         sliderInput("two_sd2", "s2 (standaard deviatie groep 2)",min=0, max=12, value=1, step=.1),
                         # Groepsgrootte
                         sliderInput("two_N2", "N2 (steekproefgrootte groep 2)",min=0, max=2000, value=42, step=1)
        )
      ),
      # Opties
      wellPanel(checkboxInput("options",strong("Opties"),value=TRUE),
                conditionalPanel(condition = paste("input.options == true"),
                                 # Bereik grafieken
                                 sliderInput("lim", "Bereik",min=0, max=20, value=5, step=1),
                                 # Antwoord?
                                 checkboxInput("conc", "Conclusie",value=FALSE),
                                 checkboxInput("krit", "p-waarde + kritiek gebied",value=FALSE)
                )
      ),
      # Plaatje UNI
      HTML('<a href="http://www.maastrichtuniversity.nl/" target="_blank"><img src="logo.jpg" alt="Maastricht University"  border="0" style="border: #00A2DB solid 1px; border-radius: 5px;"/></a>')
    ),
    
    # Main panel --------------------------------------------------------------
    # Show the generated 3d scatterplot
    mainPanel(
      # Opmaakt html
      tags$head(
        tags$style(type="text/css", "li a{color: white;  background-color:#001C3D;}")
        #         tags$style(type="text/css", ".active li a{color: black; font-weight:bold; background-color:black;}")
        
      ),
      # Verschillende tabPanels
      tabsetPanel(
        # Info hoe alles werkt
        tabPanel("Info",
                 HTML("<P STYLE='background-color: white;'>Deze applicatie laat de drie methodes zien waarmee je kan toetsen of een gevonden steekproefgemiddelde overeenkomt met het populatiegemiddelde (gegeven de hypotheses) of dat twee steekproefgemiddelden uit dezelfde populatie komen. Het tabblad 'waardes' geeft alle relevante informatie en berekent de t-waarde. Probeer deze t-waarde ook een paar keer zelf te berekenen en controleer of het klopt. De volgende drie tabbladen laten de drie methodes zien waarmee je kan bepalen welke hypothese je moet aannemen dan wel verwerpen. Probeer eerst zelf te bedenken welke hypothese je aanneemt en druk vervolgens op conclusie (onder opties), om te controleren of dit correct is. Het laatste tabblad ('t-tabel') laat zien in welke rijen en welke kolom je moet kijken in de tabel op pagina 306 van Imbos et al. Aan de linkerkant kun je de waardes veranderen, de alpha en de richting van de nulhypothese (en dus ook de alternatieve). Veel succes en probeer alle verschillende methodes eens uit en kijk (of je weet) wat er gebeurt als je waardes verandert. </P>")
        ), 
        # Hypotheses en waardes 
        tabPanel("Waardes",
                 plotOutput("waardes",width="800px",height="700px")
        ),
        # Kritieke gebied methode met conclusie
        tabPanel("Kritieke gebied",
                 plotOutput("kritiek_plot",width="800px",height="600px"),
                 plotOutput("kritiek_conc",width="800px",height="300px")
        ),
        # P-Waarde methode met conclusie
        tabPanel("p-waarde",
                 plotOutput("pwaarde_plot",width="800px",height="600px"),
                 plotOutput("pwaarde_conc",width="800px",height="300px")
        ),
        # Betrouwbaarheids interval methode conclusie
        tabPanel("Betrouwbaarheids Interval",
                 plotOutput("CI_plot",width="800px",height="600px"),
                 plotOutput("CI_conc",width="800px",height="300px")
        ), 
        # T tabel om te laten zien waar ze moeten kijken
        tabPanel("T Tabel",
                 plotOutput("critt",width="600px",height="100px"),
                 tableOutput("ttable")
        ),
        # Version e.d.
        tabPanel("Version (C)",
                 verbatimTextOutput("version"),
                 verbatimTextOutput("author"),
                 verbatimTextOutput("licence")
        ) 
      )
    )
  )
)