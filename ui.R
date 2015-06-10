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
shinyUI(fluidPage(
  
  tags$head(
    tags$head(includeScript("www/js/google-analytics.js")),
    tags$script(type="text/javascript",src="js/scripts.js"),
    tags$style(type="text/css", "form.well { margin: 0px; padding : 5px;}"),
    tags$style(type="text/css", "label, div.shiny-input-container div { display: inline; }"),
    tags$style(type="text/css", ".shiny-input-container {margin-bottom:5px;margin-top:5px; line-height:15px;}"),
    #tags$style(type="text/css", '.checkbox input[type="checkbox"],.radio input[type="radio"] { float: none; }'),
    tags$style(type="text/css", 'input[type="text"] { float: none; display : inline;}'),
    tags$style(type="text/css", '.checkbox,.radio { float: none; display:inline; margin-top:5px; margin-bottom:5px;line-height:15px;}'),
    tags$style(type='text/css', "select#selection,select#mapzones,select#racedist { width: 200px; display : inline; }"),
    tags$style(type="text/css", 'input[type="number"] { width: 50px; }'),
    tags$style(type="text/css", ".jslider { max-width: 350px; }"),
    tags$style(type='text/css', ".well { max-width: 400px; }"),#class of the from inside sidebarPanel
    tags$style(type='text/css', ".col-sm-4 { max-width: 350px; }"),#span4 is the span of sidebarPanel (span8 is for the mainPanel)
    tags$style(type='text/css', "#Lon, #Lat { max-width: 100px; }"),
    tags$style(type='text/css', "#note { color:#ff0000;font-size:xx-small; }"),
    tags$style(type="text/css", "h1,h2,h3,h4,h5,h6 {color:#317EAC;}"),
    tags$style(type="text/css", "hr {margin-bottom:8px; margin-top:8px;}")
    
  ),
  
  # Application title
  titlePanel(uiOutput("uiTitle")),
  sidebarLayout(
    sidebarPanel(
      uiOutput("uiSBlanguage"),
      uiOutput("uiSBcaution"),
      HTML('<hr style="border:1px solid #ccc;"/>'),
      uiOutput("uiSBReleaseLocations"),
      uiOutput("uiSBTowns"),
      selectInput("towns","","",selectize=FALSE,multiple=TRUE),
      
      selectInput("selection", strong(HTML("Classification :")),
                  list(" "="unselected",
                       "RFCB : Belgique lieux officiels"="beloff",
                       "RFCB : Tour de Belgique"="beltour",
                       "RFCB : Ligne de l'Ouest"="linew",
                       "RFCB : Ligne du Centre"="linec",
                       "RFCB : Ligne de l'Est"="linee",
                       "RFCB : Vallée du Rhône"="vr",
                       "RFCB : Concours (inter)nationaux" = "natint",
                       "RFCB : Concours (inter)nationaux (Demi-Fond)" = "natintdf",
                       "RFCB : Concours (inter)nationaux (Fond)" = "natintf",
                       "RFCB : Concours (inter)nationaux (Grand-Fond)" = "natintgf",
                       "RFCB : Divers" = "divers",
                       "AWC : Province du Hainaut"="h",
                       "AWC : Province du Brabant-Wallon"="bw",
                       "AWC : Province de Namur"="n",
                       "AWC : Province de Liège"="lg",
                       "AWC : Province du Luxembourg"="lx",
                       "AWC : Tous les lieux de lâcher en Wallonie"="awc",
                       "Itinéraire : AWC" = "cfwawc",
                       "Itinéraire : Andenne" = "centand", 
                       "Itinéraire : Ciney" = "cin",
                       "Itinéraire : Dinant" = "din",
                       "Itinéraire : Floreffe" = "flo",
                       "Itinéraire : Forville"="af",
                       #                      "Itinéraire : L'Avenir de Forville (Vitesse)"="afv",
                       #                      "Itinéraire : L'Avenir de Forville (Demi-Fond)"="afdf",
                       #                      "Itinéraire : L'Avenir de Forville (Fond)"="aff",
                       "Itinéraire : Hamoir" = "ham",
                       "Itinéraire : Havelange" = "hav",
                       "Itinéraire : Marche en Famenne" = "mef",
                       "Itinéraire : Derby Hainaut" = "dh",
                       "Itinéraire : Fédération de l'Espoir" = "fedesp",
                       "Itinéraire : Groupement Condroz - Famenne" = "gcf",
                       "Itinéraire : Union Wallonne Ramillies" = "uwr",
                       #                      "Itinéraire : Union Wallonne Ramillies (Vitesse)" = "uwrv",
                       #                      "Itinéraire : Union Wallonne Ramillies (Demi-Fond)" = "uwrdf",
                       #                      "Itinéraire : Union Wallonne Ramillies (Grand Demi-Fond)" = "uwrgdf",
                       #                      "Itinéraire : Union Wallonne Ramillies (Fond)" = "uwrf",
                       #                      "Itinéraire : Union Wallonne Ramillies (Internationnaux)" = "uwri",
                       "Championnat : Route du Rhône" = "rdr",
                       "Tous" = "all"
                  ),
                  selected="unselected",
                  selectize=FALSE),
      
      selectInput("mapzones", strong(HTML("Limiter la carte :")),
                  list("En fonction des données" = "dyn",
                       "à la Flandre" = "rf",
                       "à la Wallonie" = "rw",
                       "à la Belgique" = "bel",
                       "à la France" = "fra",
                       "à la Belgique et à la France" = "befr",
                       "à la Belgique, la France, et le Nord de l'Espagne" = "befres"),
                  selected="befres",
                  selectize=FALSE),
  
  #     h5(HTML("Type de pigeons concernés")),
  #     selectInput("pigeons", "",
  #                 list("Tous" = "all",
  #                      "Pigeonneaux" = "P", 
  #                      "Yearlings" = "Y",
  #                      "Vieux" = "V",
  #                      "Vieux & Yearlings"="VY"
  #                 )),
  
      checkboxInput("labels", label = "Afficher le nom des lieux de lâchers", value = TRUE),
      HTML('<hr style="border:1px solid #ccc;"/>'),
  
      h4(HTML("Calcul des distances")),
     HTML("Coordonn&eacute;es de r&eacute;f&eacute;rence (<a href='https://fr.wikipedia.org/wiki/WGS_84' target='_blank'>format WGS84</a>)"),
      HTML("<span id='note'>Remplacez ces coordonnées par les vôtres ! Consultez l'Aide.</span>"),
  tags$table(tags$tr(tags$td(textInput("Lat", "Lat","503828.0" )),tags$td(HTML("&nbsp;")),tags$td(textInput("Lon", "Lon","044005.0" )))),
      checkboxInput("kms", label = "Afficher pour chaque lieux sa distance en km ", value = FALSE),
  radioButtons("round", HTML("Arrondir au"),
                list("km"="0",
                     "hm"="1", 
                     "dm"="2",
                     "m"="3"
                ),selected="0"),
      selectInput("racedist", strong(HTML("Type de concours selon la distance")),
              list("Tous" = "all",
                   "Vitesse (0-250Km)" = "V", 
                   "Petit Demi-Fond (250-425Km)" = "PDF",
                   "Demi-Fond (425-600Km)" = "DF",
                   "Fond (600-800Km)" = "F",
                   "Grand Fond (>800Km)"="GF"
              ),selectize=FALSE,selected="all"),
      checkboxInput("circles", label = "Afficher les limites des catégories de distance", value = FALSE),
      
      HTML('<hr style="border:1px solid #ccc;"/>'),
      h4(HTML("Afficher")),
      checkboxInput("zones2015", label = "Zones pour le demi-fond et le fond 2015", value = FALSE),
      checkboxInput("zones2014", label = "Zones pour le demi-fond et le fond 2014", value = FALSE),
      checkboxInput("maintowns", label = "Principales villes du pays", value = FALSE),
      #checkboxInput("perso", label = "Lieux perso (en développement)", value = FALSE),
      HTML('<hr style="border:1px solid #ccc;"/>'),
      HTML("<a rel='license' href='http://creativecommons.org/licenses/by/2.0/be/'><img alt='Licence Creative Commons' style='border-width:0' src='img/cc_by_80x15.png' /></a> Consultez l'onglet <strong>Crédits</strong> pour les mentions de licence.")
  ),
  
    mainPanel(
      uiOutput("uiMain")
    )
  )
))
