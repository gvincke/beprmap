## beprmap (Belgium Pigeon Racing Map) Shiny/R app server.R                                           
##                                                                      
## Author(s) :
## -----------
## Grégoire Vincke http://www.gregoirevincke.be            
##                                                                      
## Licences : 
## ---------
## CC-BY for the web page http://www.yapluka.be/sapps/beprmap/
## See http://creativecommons.org/licenses/by/2.0/be/ for more informations       
##
## GPLv2 for source code on https://github.com/gvincke/beprmap 
## See LICENCE.txt or http://www.gnu.org/licenses/old-licenses/gpl-2.0.html for more informations

Sys.setlocale("LC_ALL", "fr_FR.UTF-8")#to be sure that accents in text will be allowed in plots

library(shiny)

# Define UI for zplot
shinyUI(navbarPage("Belgium Pigeon Racing Map",id="main",#http://shiny.rstudio.com/reference/shiny/latest/navbarPage.html
                   header=tags$div(id="lang-div",selectInput("language",uiOutput('uiSBlanguage'),list("English" = "en","Français" = "fr", "Nederlands" ="nl"),selected = "fr",selectize=FALSE)),#Must be defined in UI.R instead of server.R to avoid error of type "trying to select more than one value because input$language is only set after server.R load                
  tabPanel(uiOutput('uiPanelTitle'),value="main",
  fluidPage(
  tags$head(
    tags$head(includeScript("www/js/google-analytics.js")),
    tags$script(type="text/javascript",src="js/scripts.js"),
    tags$link(rel='stylesheet', type="text/css", href="css/style.css")
  ),
  fluidRow(
    column(3,
      wellPanel(
        uiOutput('uiSBtop'),
        selectInput(inputId="towns",label=uiOutput("uiSBTowns"),choices="",selectize=FALSE,multiple=TRUE,size=10),#Label is translated, so have to be set in server.R, but the list must be set in UI.R to be setted before of server.R computation to be filled by Towns values : soit un select est défini ici avec choices ="" et un observe dans server.R le rempli par après; soit le select est directement défini dans server.R mais du coup ne peut être exploité par un script js
        selectInput(inputId="selection",label=uiOutput("uiSBSelection"),choices="",selected="itawc",selectize=FALSE),#selection must be define in UI.R to be reactive to script.js -> create emty select here, use observe to load it with translated content, and uiOutput to have translated label
        uiOutput('uiSBlocationsbottom'),
        uiOutput('uiSBshow'),
        uiOutput('uiSBlicence')
      )
    ),
    column(6,
      uiOutput("uiMain"),
      uiOutput('uiSBtrainingCoords')
    ),
    column(3,
      wellPanel(
        uiOutput('uiSBdistances'),
        tags$table(tags$tr(tags$td(uiOutput('uiSBunit')),tags$td(selectInput("distunit","",choices=c("km","mi"),selectize=FALSE)),tags$td(HTML("&nbsp;")),tags$td(uiOutput('uiSBround')))),#input$distunit must be define in ui.R, but it's label must be define in server;R to be translated, as round level. So i cut the distance ui in pieces to allow each element to be define in ui.R or server.R depending it's caracteristics
        uiOutput('uiSBdistancesb'),
        uiOutput('uiSBtraining'),
        uiOutput('uiSBsimul'),
        uiOutput('uiSBdloutput')
      )
    )
  )
)
),
tabPanel(uiOutput('uiCreditsTitle'),value="credits",
         fluidPage(
           tags$head(
             tags$style(type='text/css', ".col-sm-8 { width: 100%; }")
           ),
           fluidRow(
           column(6,uiOutput('uiCredits1')),
           column(6,uiOutput('uiCredits2'))
          )
        )
      )
    )
)