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
  titlePanel("Localisation des lieux de lâchers RFCB et AWC et calcul de distance"),
  sidebarLayout(
    sidebarPanel(
      HTML("<span id='note'>ATTENTION : Si vous désirez simplement connaitre vos distances sans vous interesser à la géolocalisation des lieux de lâchers, préférez le <a href='http://www.recht-en-plicht-lembeke.be/accessoires/afstandkbdb.xls'>fichier excel mis à votre disposition par la RFCB</a>.</span>"),
      HTML('<hr style="border:1px solid #ccc;"/>'),

      h4(HTML('Lieux de l&acirc;chers')),
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
  #     selectInput("awc", strong(HTML("Classification AWC :")),
  #                 list(" "="unselected",
  #                      "Province du Hainaut"="h",
  #                      "Province du Brabant-Wallon"="bw",
  #                      "Province de Namur"="n",
  #                      "Province de Liège"="lg",
  #                      "Province du Luxembourg"="lx",
  #                      "Toutes les provinces" = "all"
  #                 ),
  #                 selected="unselected",
  #                 selectize=FALSE),
  #     selectInput("itin", strong(HTML("Itinéraires :")),
  #                 list(" "="unselected",
  #                      "Le Centre Andenne" = "centand", 
  #                      "Fédération de l'Espoir" = "fedesp",
  #                      "Union Wallonne Ramillies" = "uwr"
  #                 ),
  #                 selected="unselected",
  #                 selectize=FALSE),
      selectInput("towns",strong(HTML("Lieux individuels :")),"",selectize=FALSE,multiple=TRUE),
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
      checkboxInput("perso", label = "Lieux perso (en développement)", value = FALSE),
      HTML('<hr style="border:1px solid #ccc;"/>'),
      HTML("<a rel='license' href='http://creativecommons.org/licenses/by/2.0/be/'><img alt='Licence Creative Commons' style='border-width:0' src='img/cc_by_80x15.png' /></a> Consultez l'onglet <strong>Crédits</strong> pour les mentions de licence.")
  ),
  
    mainPanel(
      tabsetPanel(id="Tabset",selected=1,
        tabPanel(
    	"Carte",
    	plotOutput("map",height=700),#,height = "auto"
    	value=1
        ),
  	tabPanel(
  	  "Données",
  	  dataTableOutput('Datas'),
  	  value=2
  	),
  	tabPanel(
  	  "Aide",
  	  h4(HTML("Calcul des distances :")),
  	  p(HTML("Les coordonnées qui sont chargées par défaut comme ''coordonnées de référence'' sont celle du <a href='http://www.walhain.be/Loisirs/office-du-tourisme/les-curiosites-touristiques/le-centre-geographique-de-la-belgique' target=_blank'>centre géographique de la Belgique</a>. ")),
  	  p(HTML("Remplacez les par celles de votre propre colombier et vous bénéficierez d'un calcul de distance adapté à votre localisation personnelle. ")),
  	  p(HTML("Le format à utiliser dans les champs ''Lat'' (latitude) et ''Lon'' (longitude) est le format <a href='https://fr.wikipedia.org/wiki/WGS_84' target='_blank'>WGS84</a>, avec un <strong>point comme symbole décimal</strong>, et non une virgule.")),
  	  p(HTML("<strong>Rappel :</strong> Vos coordonnées figurent au format WGS84 sur votre carte de membre de la Royale Fédération Colombophile Belge.")),
  	  
  	  value=3
  	),
  	tabPanel(
  	  "Crédits",
  	  p(HTML("<strong>Auteur :</strong> Grégoire Vincke - <a href='http://www.gregoirevincke.be' target='_blank'>http://www.gregoirevincke.be</a> - Mars 2015")),
  	  p(HTML("<strong>Licences :</strong> <ul><li><strong>Animation :</strong> <a rel='license' href='http://creativecommons.org/licenses/by/2.0/be/'><img alt='Licence Creative Commons' style='border-width:0' src='img/cc_by_80x15.png' /></a> Cette animation, ainsi que tout ce qu'elle produit, est mise à votre disposition selon les termes de la <a rel='license' href='http://creativecommons.org/licenses/by/2.0/be/'>licence Creative Commons Attribution 2.0 Belgique</a>. Cela signifie que vous pouvez non seulement l'utiliser, mais également copier gratuitement et légalement les images créés par l'animation, les éditer, les modifier, les copier, les réutiliser pour toute utilisation, y compris commerciale, pour autant que vous me créditiez toujours en tant qu'auteur original. Bref que vous spécifiez que c'est moi, et non vous, qui avez créé la version originale, et que c'est depuis cette animation (citer l'URL) que vous l'avez téléchargée.</li><li><strong>Code :</strong> Le code source de cette animation est disponible sur <a href='https://github.com/gvincke/beprmap' target='_blank'>Github.com</a> sous licence <a href='http://www.gnu.org/licenses/old-licenses/gpl-2.0.html' target='_blank'>GPLv2</a>.</li></ul>")),
  	  p(HTML("<strong>Logiciels :</strong> Pour réaliser cette animation, j'ai utilisé les logiciels suivants :")),
  	  HTML("<ul>"),
  	  HTML('<li><strong>R</strong> : R Core Team (2013). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL <a href="http://www.R-project.org/" target=_blank">http://www.R-project.org/</a>.</li>'),
  	  HTML('<li><strong>RStudio</strong> : RStudio Team (2012). RStudio: Integrated Development for R. RStudio, Inc., Boston, MA URL <a href="http://www.rstudio.com/" target=_blank">http://www.rstudio.com/</a>.</li>'),
  	  HTML('<li><strong>shiny</strong> : RStudio and Inc. (2014). shiny: Web Application Framework for R. R package version 0.10.2.1. <a href="http://CRAN.R-project.org/package=shiny" target=_blank">http://CRAN.R-project.org/package=shiny</a> and <a href="http://shiny.rstudio.com" target=_blank">http://shiny.rstudio.com</a>.</li>'),
  	  HTML('<li><strong>maps</strong> : Original S code by Richard A. Becker and Allan R. Wilks. R version by Ray Brownrigg. Enhancements by Thomas P Minka <tpminka@media.mit.edu> (2014). maps: Draw Geographical Maps. R   package version 2.3-9. <a href="http://CRAN.R-project.org/package=maps" target=_blank">http://CRAN.R-project.org/package=maps</a>.</li>'),
  	  HTML('<li><strong>mapdata</strong> : Original S code by Richard A. Becker and Allan R. Wilks. R version by Ray Brownrigg. (2014). mapdata: Extra Map Databases. R package version 2.2-3. <a href="http://CRAN.R-project.org/package=mapdata" target=_blank">http://CRAN.R-project.org/package=mapdata</a>.</li>'),
  	  HTML('<li><strong>mapproj</strong> : Doug McIlroy. Packaged for R by Ray Brownrigg, Thomas P Minka and transition to Plan 9 codebase by Roger Bivand. (2014). mapproj: Map Projections. R package version 1.2-2. <a href="http://CRAN.R-project.org/package=mapproj" target=_blank">http://CRAN.R-project.org/package=mapproj</a>.</li>'),
  	  HTML('<li><strong>png</strong> : Simon Urbanek (2013). png: Read and write PNG images. R package version 0.1-7. <a href="http://CRAN.R-project.org/package=png" target=_blank">http://CRAN.R-project.org/package=png</a>.</li>'),
  	  HTML("</ul>"),
      p(HTML("<strong>Sources :</strong> Pour réaliser cette animation, j'ai récolté des données, et des idées, depuis les sources suivantes :")),
      HTML("<ul>"),
  	  HTML('<li><a href="http://rfcb.be/fr/vluchten-2/cooerdinaten" target="_blank">http://rfcb.be/fr/vluchten-2/cooerdinaten</a> : lieux de lâchers publiés par la Royale Fédération Colombophile Belge.</li>'),
  	  HTML('<li><a href="http://rfcb.be/fr/vluchten-2/vluchtprogramma" target="_blank">http://rfcb.be/fr/vluchten-2/vluchtprogramma</a> : catégorisation par la Royale Fédération Colombophile Belge des concours nationaux et internationaux en demi-fond, fond et grand-fond.</li>'),
      HTML('<li><a href="http://www.kbdb.be/fr/general/programmes-utiles" target="_blank">http://www.kbdb.be/fr/general/programmes-utiles</a> : tableurs Excel de calcul des distances entre colombier et lieux de lâcher, de détermination de la zone de jeux en fonction des coordonnées du colombier, publiés par la Royale Fédération Colombophile Belge.</li>'),
      HTML('<li><a href="http://www.euronamur.net/Pigeon/distances.html" target="_blank">http://www.euronamur.net/Pigeon/distances.html</a> : tableurs Excel de calcul des distances entre colombier et lieux de lâcher, et de vitesse des pigeons.</li>'),
  	  HTML('<li><a href="http://www.movable-type.co.uk/scripts/latlong.html" target="_blank">http://www.movable-type.co.uk/scripts/latlong.html</a> : méthodes de calcul des distances entre deux coordonnées décimales, et de conversion de coordonnées décimales en WGS84.</li>'),
  	  HTML("<li><a href='http://www.movable-type.co.uk/scripts/latlong-vincenty.html' target='_blank'>http://www.movable-type.co.uk/scripts/latlong-vincenty.html</a> : tableurs Excel et méthodes de calcul des coordonnées des points d'un cercle rayonnant autour d'un point à une distance donnée.</li>"),
  	  HTML("<li><a href='http://www.euronamur.net/Pigeon/itinandenne.html' target='_blank'>http://www.euronamur.net/Pigeon/itinandenne.html</a> : itinéraire de la Société Le Centre Andenne, et de la fédération de l'Espoir.</li>"),
  	  HTML("<li><a href='https://www.google.com/maps/d/viewer?dg=feature&msa=0&mid=zH5pFAl52rdk.k4ZvcAQUAw54' target=_blank'>https://www.google.com/maps/d/viewer?dg=feature&msa=0&mid=zH5pFAl52rdk.k4ZvcAQUAw54</a> : carte Google Maps des tracés de zones du demi-fond et du grand-fond. Explications trouvées ici : <a href='http://www.pipa.be/fr/newsandarticles/reports/coup-doeil-sur-lorganisation-du-sport-colombophile-en-belgique-1ere-partie' target='_blank'>http://www.pipa.be/fr/newsandarticles/reports/coup-doeil-sur-lorganisation-du-sport-colombophile-en-belgique-1ere-partie</a></li>"),
  	  HTML("<li><a href='http://www.coordonnees-gps.fr/' target=_blank'>http://www.coordonnees-gps.fr/</a> : utilitaire de pour récupérer les coordonnées GPS d'une adresse via Google Maps, en DD (degrés décimaux) ou au format WGS82 (DMS : Degrés Minutes Secondes).</li>"),
  	  HTML("<li><a href='http://www.1001maps.fr/index.php?page=022/' target=_blank'>http://www.1001maps.fr/index.php?page=022</a> : convertisseur de données GPS au format DD (degrés décimaux) vers le format format WGS82 (DMS : Degrés Minutes Secondes), et vice-versa.</li>"),
  	  HTML("<li><a href='http://stackoverflow.com/questions/23071026/drawing-circle-on-r-map' target=_blank'>http://stackoverflow.com/questions/23071026/drawing-circle-on-r-map</a> : discussion sur http://stackoverflow.com qui m'a permis donnéé l'idée de départ de la fonction de calcul des coordonnées d'un cercle de distance connue autour d'un point de données GPS déterminées.</li>"),
  	  HTML("<li><a href='http://www.walhain.be/Loisirs/office-du-tourisme/les-curiosites-touristiques/le-centre-geographique-de-la-belgique' target=_blank'>http://www.walhain.be/Loisirs/office-du-tourisme/les-curiosites-touristiques/le-centre-geographique-de-la-belgique</a> : coordonnées du centre géopgraphique de la Belgique, situé à Nil-St-Vincent, dans la commune de Walhain, et qui sont les coordonnées de référence définies par défaut pour le calcul des distances.</li>"),
  	  HTML("</ul>"),
  
  	  value=4
  	)
      )
    )
  )
))
