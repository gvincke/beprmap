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

# Sys.setlocale("LC_ALL", "fr_FR.UTF-8")#to be sure that accents in text will be allowed in plots
#TODO : séparer localisation des lieux de lâcher de calcul de distance : vider coordonées de référence => forcer l'encodage de coordonées
#TODO : Réorganiser l'interface pour "forcer l'encodage des coordonnées ?" Ou Visualiser à droite et calcul de distance à gauche ? Ou l'inverse ?
#TODO : ajouter les lieux suivants http://www.colombophiliefr.com/Dossiers/GPS/lieux_lacher.htm
#TODO : importation de fichiers langues pour les select : reactive ou observe : il y a les deux dans le code : uniformiser
library(shiny)
library(DT)
library(maps)
library(mapproj)
library(mapdata)
#library(maptools) # (loads foreign, sp, grid, and lattice)
library(png)

library(sp)#Classes and Methods for Spatial Data : https://cran.r-project.org/web/packages/sp/index.html
#library(BelgiumMaps.StatBel)
#data(BE_ADMIN_PROVINCE)

# Create a reactive object here that we can share between all the sessions.
SRV <- reactiveValues(count=0)#Session reactive values
cc <- readPNG("www/img/cc_by_320x60.png")
ER <- 6371 #Earth Radius in kilometers. http://en.wikipedia.org/wiki/Earth_radius Change this to 3959 and you will have your function working in miles.

shinyServer(function(input, output, session) {
  # https://gist.github.com/trestletech/9926129
  # Increment the number of sessions when one is opened.
  # We use isolate() here to:
  #  a.) Provide a reactive context
  #  b.) Ensure that this expression doesn't take a reactive dependency on
  #      SRV$count -- if it did, every time SRV$count changed, this expression
  #      would run, leading to an infinite loop.
  isolate(SRV$count <- SRV$count + 1)
  
  # When a session ends, decrement the counter.
  session$onSessionEnded(function(){
    # We use isolate() here for the same reasons as above.
    isolate(SRV$count <- SRV$count - 1)
  })
  
  # Reactively update the client. : non necessary here as the UI is defined in server for translation
  #output$count <- renderText({
  #  SRV$count
  #})
  
  data <- read.csv("data/coordonnees_rfcb.csv", sep=";", dec=",", quote="")
  
  observe({#http://stackoverflow.com/questions/28119964/dynamic-input-selector-based-on-uploaded-data
    
    l<-as.list(data$Id)
        l<-setNames(l,data$Label)
    l<-l[order(names(l))]
    l<-as.list(c(" "="empty",as.vector(l)))
    updateSelectInput(#http://www.inside-r.org/packages/cran/shiny/docs/updateSelectInput
      session,
      "towns",
      choices=l)
  })
  
  maintowns <- read.csv("data/coordonnees_principales_villes.csv", sep=";", dec=".", quote="")
  
  lang <- read.delim("data/lang.csv", header = TRUE, sep = "\t", as.is = TRUE,row.names=1) 
  tr <- function(text){ # translates text into current language
    return(sapply(text,function(s) lang[s,input$language], USE.NAMES=FALSE))
  }
  
  langSelection <- read.delim("data/lang-selection.csv", header = TRUE, sep = "\t", as.is = TRUE) 
  row.names(langSelection)<-langSelection$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
  observe({#http://stackoverflow.com/questions/28119964/dynamic-input-selector-based-on-uploaded-data
    l<-list()
    l<-c(" "="unselected",l)
    for(i in 1:nrow(langSelection)){
      l[[langSelection[[input$language]][i]]]<-langSelection$key[i]
    }
    updateSelectInput(#http://www.inside-r.org/packages/cran/shiny/docs/updateSelectInput
      session,
      "selection",
      choices=l)
  })
  
  langmappedzone <- read.delim("data/lang-mappedzone.csv", header = TRUE, sep = "\t", as.is = TRUE) 
  row.names(langmappedzone)<-langmappedzone$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
  getMappedzones<-reactive({
    l<-list()
    for(i in 1:nrow(langmappedzone)){
      l[[langmappedzone[[input$language]][i]]]<-langmappedzone$key[i]
    }
    return(l)
  })
  
  langmappedfanciers <- read.delim("data/lang-mappedfanciers.csv", header = TRUE, sep = "\t", as.is = TRUE) 
  row.names(langmappedfanciers)<-langmappedfanciers$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
  getMappedfanciers<-reactive({
    l<-list()
    for(i in 1:nrow(langmappedfanciers)){
      l[[langmappedfanciers[[input$language]][i]]]<-langmappedfanciers$key[i]
    }
    return(l)
  })
  
  langracingzone <- read.delim("data/lang-racingzone.csv", header = TRUE, sep = "\t", as.is = TRUE) 
  row.names(langracingzone)<-langracingzone$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
  getRacingzones<-reactive({
    l<-list()
    for(i in 1:nrow(langracingzone)){
      l[[langracingzone[[input$language]][i]]]<-langracingzone$key[i]
    }
    return(l)
  })
  
  beladmin <- read.delim("data/lang-belgium-administrative.csv", header = TRUE, sep = "\t", as.is = TRUE) 
  row.names(beladmin)<-beladmin$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
  getBelAdmin<-reactive({
    l<-list()
    for(i in 1:nrow(beladmin)){
      l[[beladmin[[input$language]][i]]]<-beladmin$key[i]
    }
    return(l)
  })
  
  getRacedistances<-reactive({ #read.delim and row.names inside the reactive function due to paste and input$distunit
  langracedist <- read.delim(paste("data/lang-racedist-",input$distunit,".csv",sep=""), header = TRUE, sep = "\t", as.is = TRUE) 
  row.names(langracedist)<-langracedist$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
    l<-list()
    for(i in 1:nrow(langracedist)){
      l[[langracedist[[input$language]][i]]]<-langracedist$key[i]
    }
    return(l)
  })

  langTrainingComputationMethods <- read.delim("data/lang-trainingcomputationmethods.csv", header = TRUE, sep = "\t", as.is = TRUE) 
  row.names(langTrainingComputationMethods)<-langTrainingComputationMethods$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
  getTrainingComputationMethods<-reactive({
    l<-list()
    for(i in 1:nrow(langTrainingComputationMethods)){
      l[[langTrainingComputationMethods[[input$language]][i]]]<-langTrainingComputationMethods$key[i]
    }
    return(l)
  })
  
  getInputValues<-reactive({
    return(input)#collect all inputs
  })
  
  Deg2Rad <- function(Deg){
    return(Deg*(pi/180))
  }
  
  Rad2Deg <- function(Rad){
    return(Rad*(180/pi))
  }
  
  getDistanceInMeters <- function(LonDec,LatDec,RefLonDec,RefLatDec) {
    cons1<-0.99664718933525
    cons2<-6378137
    
    RefLatRad<-Deg2Rad(RefLatDec)
    RefLonRad<-Deg2Rad(RefLonDec)
    
    LatRad<-Deg2Rad(as.numeric(LatDec))
    LonRad<-Deg2Rad(as.numeric(LonDec))
    BHO<-(RefLatRad+LatRad)/2
    IHO<-LonRad-RefLonRad
    NU2<-0.0067394967422767*cos(BHO)^2
    VHO<-sqrt(1+NU2)
    LAHO<-IHO*VHO
    OM1<-atan(cons1*tan(RefLatRad))
    OM2<-atan(cons1*tan(LatRad))
    XHO<-sin(OM1)*sin(OM2)+cos(OM1)*cos(OM2)*cos(LAHO)
    AFSTG<-(cons2/VHO)*(atan(-XHO/sqrt(1-XHO^2))+2*atan(1))
    return(round(AFSTG,0))#Distance in meters (M)
  }
  
  Dec2Km <-function(LonDec,LatDec,RefLonDec,RefLatDec,DistUnitFact){#Utilisée par le calcul de la distance des lieux persos
    v<-getInputValues() # get all values of input list
    #Calculation of distances in Km
    M<-getDistanceInMeters(LonDec,LatDec,RefLonDec,RefLatDec)
    Km<-round(M/1000*DistUnitFact,as.integer(v$round))
    return(Km)
  }

  getDistance <- function(data, cv, DistUnitFact) {
    data$M<-getDistanceInMeters(data$Lon,data$Lat,cv$LonDec,cv$LatDec)
    data$Km<-round(data$M/1000*DistUnitFact,cv$round)
    return(data)
  }
  
  Sexa2Dec <-function(Sexa){#From Sexagésimal to decimal coordinates
    Deg<-floor(Sexa/10000)
    Min<-((floor(Sexa/100)/100-floor(floor(Sexa/100)/100))*100)
    Sec<-((Sexa/100-floor(Sexa/100))*100)
    Dec<-Deg+(Min/60)+(Sec/3600)
    #par(paste(Deg," | ",Min, " | ", Sec," | ",Dec, "\n"))
    # ATTENTION : les coordonnées WGS c'est pas linéaire : 4 00 00 direct en dessous c'est pas 3 99 99 mais 3 59 59 car Min et Sec sont en base 60 !!!!!
    return(Dec)
  }
  
  Dec2Sexa <-function(Dec) {
    Deg<-floor(Dec)
    Min<-floor((Dec-Deg)*60)
    Sec<-round((((Dec-Deg)*60)-Min)*60,1)
    return(Deg*10000+Min*100+Sec)
  }
  
  getLatFromAngleDistance <-function(LatCenterRad,AngRad,DistInKms){
    return(asin(sin(LatCenterRad)*cos(DistInKms/ER)+cos(LatCenterRad)*sin(DistInKms/ER)*cos(AngRad)))  #Latitude of each point of the circle rearding to distance and to angle in radians
  }
  
  getLonFromAngleDistance <-function(LatCenterRad,LonCenterRad,AngRad,DistInKms,NewLatRad){
    return(LonCenterRad+atan2(sin(AngRad)*sin(DistInKms/ER)*cos(LatCenterRad),cos(DistInKms/ER)-sin(LatCenterRad)*sin(NewLatRad))) #Longitude of each point of the circle rearding to distance and to angle in radians
  }
  
  getDistUnitFact<-function(){
    v<-getInputValues() # get all values of input list
    DistUnitFact<-1
    if(v$distunit=='mi'){DistUnitFact<-3959/ER}# radius of the Earth in Mi / radius of the Earth in Km 
    return(DistUnitFact)
  }

  getComputedValues<-reactive({
    v<-getInputValues() # get all values of input list
    cv<-list()#created empty computed values list
    
    cv$round<-as.integer(v$round)
    
    if(v$Lat!="" & v$Lon!=""){
      cv$Lat<-as.numeric(v$Lat)
      cv$Lon<-as.numeric(v$Lon)
      
      cv$LatDec<-Sexa2Dec(cv$Lat) #From Sexagésimal to decimal coordinates
      cv$LonDec<-Sexa2Dec(cv$Lon) #From Sexagésimal to decimal coordinates
         
      cv$DistUnitFact<-getDistUnitFact()
      
      data <- getDistance(data,cv,cv$DistUnitFact)
      cv$maintowns<- getDistance(maintowns,cv,cv$DistUnitFact)
    }
    
    #Data selection : must be after distance computation because some filters are based on distance
    coords<-data
    if(v$selection=="unselected"){coords<-subset(coords,Id %in% c())}

    if(v$selection=="rfcblinew"){coords<-subset(coords,Id %in% c(112,113,114,115,116,117,118,119))}
    if(v$selection=="rfcblinec"){coords<-subset(coords,Id %in% c(120,121,122,123,34))}
    if(v$selection=="rfcblinee"){coords<-subset(coords,Id %in% c(103,50,88,68,27,31,99,47,48,58,91,44))}
    if(v$selection=="rfcbvr"){coords<-subset(coords,Id %in% c(80,81,35,85,100,111,20,93,5,9,30))}
    if(v$selection=="rfcbnatint15"){coords<-subset(coords,Id %in% c(2,6,10,13,16,17,22,39,42,43,49,52,53,57,61,63,64,70,74,76,94,101,102))}
    if(v$selection=="rfcbnatint16"){coords<-subset(coords,Id %in% c(2,6,10,13,16,17,22,39,43,49,52,53,57,61,63,64,70,74,76,94,101,102))}#42 issoudun not in 2016
    if(v$selection=="rfcbnatint17"){coords<-subset(coords,Id %in% c(2,6,10,13,16,17,18,22,43,52,53,57,74,76,94,101,102))}#+18 Carcassonne -39 gueret -49 La Souteraine  -64 Montluçon -70 Narbonne
    if(v$selection=="rfcbnatint18"){coords<-subset(coords,Id %in% c(2,6,10,13,16,17,18,22,43,52,53,57,74,76,94,101,102, 39,63,70,4))}#+39 Montelimar +63 Gueret +70 Narbonne +4 Angoulème
    if(v$selection=="rfcbnatint19"){coords<-subset(coords,Id %in% c(c(105,13,22,97,64,42,6),c(53,102,17,63,61,8,52,89,101),c(74,2,10,94,57,70,76)))}
    if(v$selection=="rfcbnatint20"){coords<-subset(coords,Id %in% c(c(6,14,22,39,49,200),c(8,16,17,43,53,63,89,101,102),c(2,10,57,70,74,76,94)))}
    
    if(v$selection=="rfcbnatintdf15"){coords<-subset(coords,Id %in% c(13,22,39,64,6,49,42))}
    if(v$selection=="rfcbnatintdf16"){coords<-subset(coords,Id %in% c(13,22,39,64,6,49))}#42 issoudun not in 2016
    if(v$selection=="rfcbnatintdf17"){coords<-subset(coords,Id %in% c(13,22,6))}#-39 gueret -64 Montluçon -49 La Souteraine
    if(v$selection=="rfcbnatintdf19"){coords<-subset(coords,Id %in% c(105,13,22,97,64,42,6))}
    if(v$selection=="rfcbnatintdf20"){coords<-subset(coords,Id %in% c(6,14,22,39,49,200))}
    if(v$selection=="rfcbnatintf"){coords<-subset(coords,Id %in% c(53,102,17,63,61,16,43,101,52))}
    if(v$selection=="rfcbnatintf17"){coords<-subset(coords,Id %in% c(53,102,17,16,43,101,52))}#-61 Montauban -63 Montélimar
    if(v$selection=="rfcbnatintf19"){coords<-subset(coords,Id %in% c(53,102,17,63,61,8,52,89,101))}
    if(v$selection=="rfcbnatintf20"){coords<-subset(coords,Id %in% c(8,16,17,43,53,63,89,101,102))}
    if(v$selection=="rfcbnatintgf"){coords<-subset(coords,Id %in% c(74,2,10,94,57,70,76))}
    if(v$selection=="rfcbnatintgf17"){coords<-subset(coords,Id %in% c(74,2,10,94,57,76,18))}#+18 Carcassonne -70 Narbonne
    if(v$selection=="rfcbnatintgf19"){coords<-subset(coords,Id %in% c(74,2,10,94,57,70,76))}
    if(v$selection=="rfcbnatintgf20"){coords<-subset(coords,Id %in% c(2,10,57,70,74,76,94))}
    if(v$selection=="rfcbbeloff"){coords<-subset(coords,Id %in% c(79,59,139,140,55))}
    if(v$selection=="rfcbbeltour"){coords<-subset(coords,Id %in% c(141,142,143,144,145,146,147,148,149,150,7))}
    if(v$selection=="rfcbdivers"){coords<-subset(coords,Id %in% c(151,152,153,154,28,25,32,155,156,37,157,158,159,40,160,161,162,163,56,164,165,166,167,168,169,169,170,171,172,173,174,175,176,177,95,108,178,109,179,180,181))}
    
    if(v$selection=="awch"){coords<-subset(coords,Id %in% c(79,182,183,59))}
    if(v$selection=="awcbw"){coords<-subset(coords,Id %in% c(184))}
    if(v$selection=="awcn"){coords<-subset(coords,Id %in% c(185,168,95,37))}
    if(v$selection=="awclg"){coords<-subset(coords,Id %in% c(186,141,188,189))}
    if(v$selection=="awclx"){coords<-subset(coords,Id %in% c(190,191,7,109))}
    if(v$selection=="awc"){coords<-subset(coords,Id %in% c(79,182,183,59,184,185,168,95,37,186,141,188,189,190,191,7,109))}
    if(v$selection=="itawc15"){coords<-subset(coords,Id %in% c(14,46,105))}
    if(v$selection=="itawc16"){coords<-subset(coords,Id %in% c(14,71,19,23,105))}
    if(v$selection=="itawc17"){coords<-subset(coords,Id %in% c(14,71,64,23,105))}
    if(v$selection=="itawc18"){coords<-subset(coords,Id %in% c(14,71,105,48))}#64,23,
    if(v$selection=="itawc19"){coords<-subset(coords,Id %in% c(6,71,64,105,48))}#-14 Bourges -23 Chateauroux +6Argenton (+4 Angouleme annulé)
    if(v$selection=="itawc20"){coords<-subset(coords,Id %in% c(14,48,104))}#-26 Clermont-Ferrand
    
    if(v$selection=="itfedesp"){coords<-subset(coords,Id %in% c(15,25,31,59,80,85,90,91,99,106,14,46,105,152,167,2,6,10,13,16,17,22,39,42,43,49,52,53,57,61,63,64,70,76,94,101,102))}
    if(v$selection=="itcentand15"){coords<-subset(coords,Id %in% c(15,25,31,59,80,85,90,91,99,106))}
    if(v$selection=="itcentand16"){coords<-subset(coords,Id %in% c(103,59,50,88,68,58,54,38,105,193,23,14))}
   # if(v$selection=="itcentand17"){coords<-subset(coords,Id %in% c(31,99,47,91,84,54,38,95))}
    if(v$selection=="itcentand17"){coords<-subset(coords,Id %in% c(31,99,85,91,84,54,38,95))}#85 la ferté remplacé par Sézane car site la ferté pas accessible
    if(v$selection=="itcentand18"){coords<-subset(coords,Id %in% c(99,85,84,31,54,38,80,66,73,193,98))}
    if(v$selection=="itbeaur18"){coords<-subset(coords,Id %in% c(196,99,85,84,54,38,c(19,66,14,105,71,23,192)))}
    
    if(v$selection=="itaf15"){coords<-subset(coords,Id %in% c(25,59,31,25,59,31,99,85,80,91,106,13,15,22,91,106,46,90,105,42,53,102,17,39,74,64,2,63,6,10,61,49,94,16,57,43,70,101,76,52,6))}
    # if(v$selection=="itafv"){coords<-subset(coords,Id %in% c(25,59,31,99,85,80))}
    # if(v$selection=="itafdf"){coords<-subset(coords,Id %in% c(91,106,13,15,22,91,106,46,90,105,42))}
    # if(v$selection=="itaff"){coords<-subset(coords,Id %in% c(53,102,17,39,74,64,2,63,6,10,61,49,94,16,57,43,70,101,76,52,6))}
    if(v$selection=="itaf16"){coords<-subset(coords,Id %in% c(59,103,50,68,58,105,88,19,26,2,6,10,13,16,17,22,39,43,49,52,53,57,61,63,64,70,74,76,94,101,102))}
    if(v$selection=="itaf17"){coords<-subset(coords,Id %in% c(c(59,31,99,47,91),c(54,38,19,14,22,71,6,64,105),c(53,102,17,16,43,101,52),c(74,2,10,94,57,76,18)))}
    
    if(v$selection=="itaf19"){coords<-subset(coords,Id %in% c(c(31,59,99),c(84,85),c(38,193,210),c(48,64,71,6,105),c(c(105,13,22,97,64,42,6),c(53,102,17,63,61,8,52,89,101),c(2,10,94,57,70,76))))}#on n'enloge pas Pau 74
    if(v$selection=="ituwr"){coords<-subset(coords,Id %in% c(59,88,44,98,48,105,19,13,22,39,64,6,49,26,42,53,102,17,63,61,16,43,101,52,74,2,10,94,57,70,76,192))}
    if(v$selection=="ituwrv"){coords<-subset(coords,Id %in% c(59,88,44))}
    if(v$selection=="ituwrdf"){coords<-subset(coords,Id %in% c(98,48))}
    if(v$selection=="ituwrgdf"){coords<-subset(coords,Id %in% c(105,19,13,22,39,64,6,49,26,42))}
    if(v$selection=="ituwrf"){coords<-subset(coords,Id %in% c(53,102,17,63,61,16,43,101,52,192))}
    if(v$selection=="ituwri"){coords<-subset(coords,Id %in% c(74,2,10,94,57,70,76))}
    if(v$selection=="itgcf16"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58,105,13,19,46,26,192))}
    if(v$selection=="itgcf17"){coords<-subset(coords,Id %in% c(31,99,47,91,84,54,38,95,c(14,71,64,23,105)))}#+AWC 2017
    if(v$selection=="itham"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58))}
    if(v$selection=="itmef"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58))}
    if(v$selection=="ithav15"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58,105,13,46))}
    if(v$selection=="ithav20"){coords<-subset(coords,Id %in% c(31,38,54,73,85,91,99,c(14,48,104)))}#+AWC2020 (-Clertmont Ferrand)
    if(v$selection=="itcin"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58))}
    if(v$selection=="itdin"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58,19,26,192))}
    if(v$selection=="itflo"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58))}
    if(v$selection=="itdh"){coords<-subset(coords,Id %in% c(105,19,53,13,102,22,17,39,74,64,2,63,6,10,61,49,94,16,57,43,70,101,76,52,42))}
    if(v$selection=="iterq16"){coords<-subset(coords,Id %in% c(50,88,68,44,98))}
    
    if(v$selection=="itlr19"){coords<-subset(coords,Id %in% c(208,122,123,34,98,73))}
    
    
    if(v$selection=="rdr"){coords<-subset(coords,Id %in% c(102,63,57))}
    
    if(v$selection=="all"){coords<-coords}
    
    #     if(v$pigeons=="P"){coords<-subset(coords,coords$Id %in% c(79))}
    
    if(v$racedist=="V"){coords<-subset(coords,coords$Km<=250*cv$DistUnitFact)}
    if(v$racedist=="PDF"){coords<-subset(coords,coords$Km>250*cv$DistUnitFact & Km<=425*cv$DistUnitFact)}
    if(v$racedist=="DF"){coords<-subset(coords,coords$Km>425*cv$DistUnitFact & Km<=600*cv$DistUnitFact)}
    if(v$racedist=="F"){coords<-subset(coords,coords$Km>600*cv$DistUnitFact & Km<=800*cv$DistUnitFact)}
    if(v$racedist=="GF"){coords<-subset(coords,coords$Km>800*cv$DistUnitFact)}
    
    if(length(v$towns)>0 ){
      if(v$selection=="unselected"){
        coords<-data
      }
      if(v$selection=="unselected"){# | v$selection=="all"

      if("empty" %in% v$towns){
        coords<-subset(coords,coords$Id %in% c())#no more subset is done
      }else {
        coords<-subset(coords,coords$Id %in% v$towns)
      }
      }
    }
    cv$coords<-coords
    #compute WGS84 coordinates Dms.toDMS = function(deg, format, dp) from http://www.movable-type.co.uk/scripts/latlong.html
    
    coords <- transform(coords, LatSign = ifelse(Lat < 0, "-", ""))
    coords <- transform(coords, LonSign = ifelse(Lon < 0, "-", ""))
    
    coords <- transform(coords, LatAbs = abs(Lat))
    coords <- transform(coords, LatSec = LatAbs*3600)
    coords <- transform(coords, Latd = floor(LatSec/3600))
    coords <- transform(coords, Latm = floor((LatSec/60) %% 60))
    coords <- transform(coords, Latm = sprintf( "%02d",Latm))
    coords <- transform(coords, Lats = round(LatSec %% 60,1))
    coords <- transform(coords, Latsf = floor(Lats))
    coords <- transform(coords, Latsd = round((Lats-Latsf)*10,0))
    coords <- transform(coords, Latsf = sprintf( "%02d",Latsf))
    coords <- transform(coords, LatWSG84 = paste(LatSign,Latd,Latm,paste(Latsf,Latsd,sep="."),sep=""))
    
    
    coords <- transform(coords, LonAbs = abs(Lon))
    coords <- transform(coords, LonSec = LonAbs*3600)
    coords <- transform(coords, Lond = floor(LonSec/3600))
    coords <- transform(coords, Lonm = floor((LonSec/60) %% 60))
    coords <- transform(coords, Lonm = sprintf( "%02d",Lonm))
    coords <- transform(coords, Lons = round(LonSec %% 60,1))
    coords <- transform(coords, Lonsf = floor(Lons))
    coords <- transform(coords, Lonsd = round((Lons-Lonsf)*10,0))
    coords <- transform(coords, Lonsf = sprintf( "%02d",Lonsf))
    coords <- transform(coords, LonWSG84 = paste(LonSign,Lond,Lonm,paste(Lonsf,Lonsd,sep="."),sep=""))
    
    if(v$Lat!="" & v$Lon!=""){
      cv$datatoshow<-subset(coords,select=c(Id,Villes,LatWSG84,LonWSG84,Lat,Lon,M,Km,Pays))
      names(cv$datatoshow)[7]<-paste(tr("Dist")," ","(m)",sep="")#change Lat to LatDD
      names(cv$datatoshow)[8]<-paste(tr("Dist")," ","(",v$distunit,")",sep="")#Change Lon to LonDD
    } else {
      cv$datatoshow<-subset(coords,select=c(Id,Villes,LatWSG84,LonWSG84,Lat,Lon,Pays))
    }
    names(cv$datatoshow)[2]<-paste(tr("Towns"))
    names(cv$datatoshow)[3]<-paste(tr("NorthN"),"WSG84",sep=" ")#change LatWSG84 to Lat WSG84
    names(cv$datatoshow)[4]<-paste(tr("EastE"),"WSG84",sep=" ")#change LonWSG84 to Lon WSG84
    names(cv$datatoshow)[5]<-paste(tr("NorthN"),tr("DD"),sep=" ")#change Lat to LatDD
    names(cv$datatoshow)[6]<-paste(tr("EastE"),tr("DD"),sep=" ")#Change Lon to LonDD
    names(cv$datatoshow)[9]<-paste(tr("Country"),sep="")#Change Lon to LonDD
    
    #Set zone of plotting
    #default
    cv$ymin<-c(42.25)
    cv$ymax<-c(51.5)
    cv$xmin<-c(-3)
    cv$xmax<-c(8)
    
    if(v$mapzones=="rf"){
      cv$ymin<-c(50.25)
      cv$ymax<-c(51.5)
      cv$xmin<-c(2.5)
      cv$xmax<-c(6)
    }
    if(v$mapzones=="rw"){
      cv$ymin<-c(49.5)
      cv$ymax<-c(51)
      cv$xmin<-c(4)
      cv$xmax<-c(6)
    }
    if(v$mapzones=="bel"){
      cv$ymin<-c(49.5)
      cv$ymax<-c(51.5)
      cv$xmin<-c(2)
      cv$xmax<-c(7)
    }
    if(v$mapzones=="fra"){
      cv$ymin<-c(42.25)
      cv$ymax<-c(51.05)
      cv$xmin<-c(-3)
      cv$xmax<-c(8)
    }
    if(v$mapzones=="befr"){#| v$mapzones=="ger" | v$mapzones=="esp"
      cv$ymin<-c(42.25)
      cv$ymax<-c(51.5)
      cv$xmin<-c(-3)
      cv$xmax<-c(8)
    }
    if(v$mapzones=="befres"){
      cv$ymin<-c(41)
      cv$ymax<-c(51.5)
      cv$xmin<-c(-3)
      cv$xmax<-c(8)
    }
    if(v$mapzones=="dyn" & nrow(cv$coords)>0){
      if(v$Lat!="" & v$Lon!=""){
        cv$ymin<-c(min(as.numeric(cv$coords$Lat),cv$LatDec))
        cv$ymax<-c(max(as.numeric(cv$coords$Lat),cv$LatDec))
        cv$ymin<-cv$ymin-((cv$ymax-cv$ymin)*0.05)
        cv$ymax<-cv$ymax+((cv$ymax-cv$ymin)*0.05) 
        
        cv$xmin<-c(min(as.numeric(cv$coords$Lon),cv$LonDec))
        cv$xmax<-c(max(as.numeric(cv$coords$Lon),cv$LonDec))
        cv$xmin<-cv$xmin-((cv$xmax-cv$xmin)*0.05)
        cv$xmax<-cv$xmax+((cv$xmax-cv$xmin)*0.20)
      } else {
        cv$ymin<-min(as.numeric(cv$coords$Lat))
        cv$ymax<-max(as.numeric(cv$coords$Lat))
        cv$ymin<-cv$ymin-((cv$ymax-cv$ymin)*0.05)
        cv$ymax<-cv$ymax+((cv$ymax-cv$ymin)*0.05) 
        
        cv$xmin<-min(as.numeric(cv$coords$Lon))
        cv$xmax<-max(as.numeric(cv$coords$Lon))
        cv$xmin<-cv$xmin-((cv$xmax-cv$xmin)*0.05)
        cv$xmax<-cv$xmax+((cv$xmax-cv$xmin)*0.20)
      }
    }
    if(v$mapzones=="man"){
      cv$ymin<-Sexa2Dec(as.numeric(v$MLatMin))
      cv$ymax<-Sexa2Dec(as.numeric(v$MLatMax))
      cv$xmin<-Sexa2Dec(as.numeric(v$MLonMin))
      cv$xmax<-Sexa2Dec(as.numeric(v$MLonMax))
    }
    if(v$mapzones!="man"){ #Map should be 2 times more larger than higher if not manual
      cv$mapheight<-cv$ymax-cv$ymin
      cv$mapwidthmin<-cv$mapheight*2
      mapwidht<-cv$xmax-cv$xmin
      if(mapwidht<cv$mapwidthmin){
        cv$mapwidth<-cv$mapwidthmin
        mapxmiddle<-(cv$xmin+((cv$xmax-cv$xmin)/2))
        cv$xmax<-mapxmiddle+cv$mapwidth*0.48
        cv$xmin<-mapxmiddle-cv$mapwidth*0.48
      } 
    }
    return(cv)
  })
  
  angleDeg <- function(lon1,lat1,lon2,lat2) {#from http://rfcb.be/images/Nuttige_programmas/zoneberekening.xls
    # Compute Radians from Degrees
    lon1<-Deg2Rad(lon1)
    lat1<-Deg2Rad(lat1)
    lon2<-Deg2Rad(lon2)
    lat2<-Deg2Rad(lat2)
    return(Rad2Deg(atan((sin(lon2-lon1)*cos(lat2))/(cos(lat1)*sin(lat2)-sin(lat1)*cos(lat2)*cos(lon1-lon2)))))
  }
  # sign of Ang in degrees are not trasnformed in Radians in the same way if the detination is in NE, SE, SW or NW of origin
  # TODO : try to optimise that computation to avoid this +180 : see Deg2Rad2Deg.txt
  AngDeg2RadLoft2Dest<-function(loftcoord,destcoord,AngDeg){
    if(loftcoord$x >= destcoord$x){ # Est or West
      if(AngDeg<0){ # Notrh or South
        AngRad <- Deg2Rad(AngDeg)
      } else {
        AngRad <- Deg2Rad(AngDeg+180)
      }
    } else {
      if(AngDeg>0){# Notrh or South
        AngRad <- Deg2Rad(AngDeg)
      } else {
        AngRad <- Deg2Rad(AngDeg+180)
      }
    }
    return(AngRad)
  }
  AngDeg2RadDest2Loft<-function(loftcoord,destcoord,AngDeg){      # sign of Ang in degrees are not trasnformed in Radians in the same way if the detination is in NE, SE, SW or NW of origin
    if(loftcoord$x >= destcoord$x){ # Est or West
      if(AngDeg>0){ # Notrh or South
        AngRad <- Deg2Rad(AngDeg)
      } else {
        AngRad <- Deg2Rad(AngDeg+180)
      }
    } else {
      if(AngDeg<0){# Notrh or South
        AngRad <- Deg2Rad(AngDeg)
      } else {
        AngRad <- Deg2Rad(AngDeg+180)
      }
    }
    return(AngRad)
  }
  
  getTrainingAngle <- function(mycoord,destcoords){#(mycoord,cv$coords) #coords<-mapproject(cv$coords$Lon[i],cv$coords$Lat[i])
    v<-getInputValues()
    trainingangles<-c()
    for(i in 1:nrow(destcoords)){
      coords<-mapproject(destcoords$Lon[i],destcoords$Lat[i])
      AngDeg<-angleDeg(mycoord$x,mycoord$y,coords$x,coords$y)
      AngRad<-AngDeg2RadLoft2Dest(mycoord,coords,AngDeg)
      trainingangles <- c(trainingangles,AngRad)
    }
    if(v$trainingmethods=="mean"){
      trainingangle<-mean(trainingangles)
    }
    if(v$trainingmethods=="bissect"){
      trainingangle<-mean(c(max(trainingangles),min(trainingangles)))
    }
    return(trainingangle)
  }
  
  getTrainingLatDeg <- function (Coords,AngRad,Km) {
    Lat1Rad <- Deg2Rad(Coords[1])#Latitude of the center of the circle in radians#From degrees to radians rad= deg*(pi/180)
    LatTrainingRad <- getLatFromAngleDistance(Lat1Rad,AngRad,Km)
    LatTrainingDeg <- Rad2Deg(LatTrainingRad)#Latitude of each point of the circle in degrees#From radians to degrees deg = rad*(180/pi)
    return(LatTrainingDeg)
  }
  
  getTrainingLonDeg <- function (Coords,AngRad,Km) {
    Lat1Rad <- Deg2Rad(Coords[1])#Latitude of the center of the circle in radians#From degrees to radians rad= deg*(pi/180)
    Lon1Rad <- Deg2Rad(Coords[2])#Longitude of the center of the circle in radians
    LatTrainingRad <- getLatFromAngleDistance(Lat1Rad,AngRad,Km)
    LonTrainingRad <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad,Km,LatTrainingRad)
    LonTrainingDeg <- Rad2Deg(LonTrainingRad)#Longitude of each point of the circle in degrees#From radians to degrees deg = rad*(180/pi)
    return(LonTrainingDeg)
  }
  
  plotMap <- function()({ #put plot into a function to be able to render it for both output and download
    
    plotDist <- function(LatDec, LonDec, Km, Color,AngDeg1=1,AngDeg2=360) { #inspired form http://www.movable-type.co.uk/scripts/latlong-vincenty.html and http://stackoverflow.com/questions/23071026/drawing-circle-on-r-map
      DistUnitFact<-getDistUnitFact()
      AngDeg<-c(AngDeg1,seq(ceiling(AngDeg1),floor(AngDeg2)),AngDeg2) # car seq ne donne que des suites d'entier : donc on rajoute les valeurs décimales avant et après
      AngRad <- Deg2Rad(AngDeg)
      Lat1Rad <- Deg2Rad(LatDec)#Latitude of the center of the circle in radians#From degrees to radians rad= deg*(pi/180)
      Lon1Rad <- Deg2Rad(LonDec)#Longitude of the center of the circle in radians

      for(i in 1:length(Km)){# Pour chaque distance, faire un cercle ou un aarc de cercle
        Lat2Rad <- getLatFromAngleDistance(Lat1Rad,AngRad,Km[i])
        Lon2Rad <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad,Km[i],Lat2Rad)
        Lat2Deg <- Rad2Deg(Lat2Rad)#Latitude of each point of the circle in degrees#From radians to degrees deg = rad*(180/pi)
        Lon2Deg <- Rad2Deg(Lon2Rad)#Longitude of each point of the circle in degrees#From radians to degrees deg = rad*(180/pi)
        
        if(AngDeg1==1 && AngDeg2==360){
          polygon(c(Lon2Deg),c(Lat2Deg),lty=2,border=Color)
          text(Lon2Deg[120],Lat2Deg[120],srt=60, labels = paste(round(Km[i]*DistUnitFact,0),v$distunit,sep=" "), pos=3,cex=0.8,col=Color)#angle 0 is vertical in a map, not horizontal as in common geometry ! http://www.ats.ucla.edu/stat/r/faq/angled_labels.htm
          text(Lon2Deg[240],Lat2Deg[240],srt=-60, labels = paste(round(Km[i]*DistUnitFact,0),v$distunit,sep=" "), pos=3,cex=0.8,col=Color)
        } else { #Arc
          lines(c(Lon2Deg),c(Lat2Deg), col=Color)#, lty=2
        }
      }
    }
 

    
    plotFlightLine <- function(mycoord,coords,DKm,Color){#mycoords and coords are coodinates generated bu mappproject() function of mapproj library : they are list with $x (lon) and $y (lat) values. They are set for only one couple of points source (mycoords) and destination (coords)
      v<-getInputValues() # get all values of input list
      # 1: compute distance between the two coordinates (cf line 142) : this is already done in cv$coords$Km
      c1<-list()
      c2<-list()
      c1$Lon<-mycoord$x #needed to be renamed to be used by getDistance()
      c1$Lat<-mycoord$y #needed to be renamed to be used by getDistance()
      c2$LonDec<-coords$x #needed to be renamed to be used by getDistance()
      c2$LatDec<-coords$y #needed to be renamed to be used by getDistance()
      c2$round<-as.integer(v$round) #needed by getDistance()
      DistUnitFact<-1#distance value must stay in KM !! If in Mi the line will be too short
      Dist<-getDistance(c1, c2, DistUnitFact)
      # 2: compute angle between these two coordinates #Angles in degrees relatively to reference loft
      AngDeg <- angleDeg(mycoord$x,mycoord$y,coords$x,coords$y)#lon1,lat1,lon2,lat2
      AngRad<-AngDeg2RadLoft2Dest(mycoord,coords,AngDeg)
      # 3: compute intermediate coordinates each Dkm distance
      Kms <- seq(0,Dist$Km,by=DKm)
      if(!Dist$Km %in% Kms){
        Kms<-c(Kms,Dist$Km)# Add total distance to sequence if not already inside to be sure that the line covers the all distance
      }
      # 4: plot lines between theses intermediates coordinates to respect the earth curve
      Lat1Rad <- Deg2Rad(c1$Lat)#Latitude of the loft coordinates in radians #From degrees to radians rad= deg*(pi/180)
      Lon1Rad <- Deg2Rad(c1$Lon)#Longitude of the loft coordinates in radians
      
      lineLat<-c()
      lineLon<-c()
      for(j in 1:(length(Kms)-1)){
        if(j==1){
          Lat2Rad1 <- getLatFromAngleDistance(Lat1Rad,AngRad,Kms[j])
          Lon2Rad1 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad,Kms[j],Lat2Rad1)
          
          Lat2Deg1 <-Rad2Deg(Lat2Rad1)
          Lon2Deg1 <-Rad2Deg(Lon2Rad1)
          
          lineLat<-c(lineLat,Lat2Deg1)
          lineLon<-c(lineLon,Lon2Deg1)
        }

        Lat2Rad2 <- getLatFromAngleDistance(Lat1Rad,AngRad,Kms[j+1])
        Lon2Rad2 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad,Kms[j+1],Lat2Rad2)
                
        Lat2Deg2 <-Rad2Deg(Lat2Rad2)
        Lon2Deg2 <-Rad2Deg(Lon2Rad2)
        
        lineLat<-c(lineLat,Lat2Deg2)# vector of all latitude of dots of this line
        lineLon<-c(lineLon,Lon2Deg2)# vector of all longitude of dots of this line
      }
      if(length(lineLat) & length(lineLon)){
        # only on line of vectors of lat and lon : apply to plotZonesRFCB where the is sum of lines segmets of couples of coordinates : one line = sum of segments, here it's only one line with vectors of coordinates : line lot (dotted, shaded) is more beautifull
        lines(lineLon,lineLat,lty=3,type="l",col=Color)#lty 1 solid 2 dashed 3 dotted
        #points(lineLon[1],lineLat[1],col="yellow")#Proove that first coordinates is the loft
      }
      # Keep this line below to be able to see difference between two type of lines : streight line between the two points, or reographically corrected line infunction of projection (above)
      # lines(c(mycoord[1],coords[1]),c(mycoord[2],coords[2]),lty=2,col="green")
    }
    
    plotPigeonsLocationSimulation <- function(mycoord,coords,DKm,Color){#mycoords and coords are coodinates generated bu mappproject() function of mapproj library : they are list with $x (lon) and $y (lat) values. They are set for only one couple of points source (mycoords) and destination (coords)
      v<-getInputValues() # get all values of input list
      # 1: compute distance between the two coordinates (cf line 142) : this is already done in cv$coords$Km
      c1<-list()
      c2<-list()
      c1$Lon<-mycoord$x #needed to be renamed to be used by getDistance()
      c1$Lat<-mycoord$y #needed to be renamed to be used by getDistance()
      c2$LonDec<-coords$x #needed to be renamed to be used by getDistance()
      c2$LatDec<-coords$y #needed to be renamed to be used by getDistance()
      c2$round<-as.integer(v$round) #needed by getDistance()
      DistUnitFact<-1#distance value must stay in KM !! If in Mi the line will be too short
      Dist<-getDistance(c1, c2, DistUnitFact)
      # 2: compute angle between these two coordinates #Angles in degrees relatively to reference loft
      AngDeg <- angleDeg(coords$x,coords$y,mycoord$x,mycoord$y)#lon1,lat1,lon2,lat2
      AngRad<-AngDeg2RadDest2Loft(mycoord,coords,AngDeg)
      # 3 : compute intermediate coordinates : compute both min and max distance regarding to time
      TimeInMinutes<-v$minutes+(v$hours*60)+(v$days*1440)
      KmMin<-round((v$speed[1]*TimeInMinutes)/1000,3)
      KmMax<-round((v$speed[2]*TimeInMinutes)/1000,3)
      # 4: compute intermediate coordinates each Dkm distance
      if(KmMax>Dist$Km){KmMax<-Dist$Km}#limit KmMax to Race distance
      if(KmMin>Dist$Km){KmMin<-Dist$Km}#to avoid sign error for DKm in seq()
      Kms <- seq(KmMin,KmMax,by=DKm)
      if(!KmMax %in% Kms){
        Kms<-c(Kms,KmMax)# Add total distance to sequence if not already inside to be sure that the line covers the all distance
      }
      # 5 : computes coordinates of both min and max location, based on original coordinates, angle, and distance
      
      # 6: plot lines between theses intermediates coordinates to respect the earth curve
      Lat1Rad <- Deg2Rad(c2$LatDec)#Latitude of the loft coordinates in radians #From degrees to radians rad= deg*(pi/180)
      Lon1Rad <- Deg2Rad(c2$LonDec)#Longitude of the loft coordinates in radians
      
      lineLat<-c()
      lineLon<-c()
      for(j in 1:(length(Kms)-1)){
        if(j==1){
          Lat2Rad1 <- getLatFromAngleDistance(Lat1Rad,AngRad,Kms[j])
          Lon2Rad1 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad,Kms[j],Lat2Rad1)
          
          Lat2Deg1 <-Rad2Deg(Lat2Rad1)
          Lon2Deg1 <-Rad2Deg(Lon2Rad1)
          
          lineLat<-c(lineLat,Lat2Deg1)
          lineLon<-c(lineLon,Lon2Deg1)
        }
        
        Lat2Rad2 <- getLatFromAngleDistance(Lat1Rad,AngRad,Kms[j+1])
        Lon2Rad2 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad,Kms[j+1],Lat2Rad2)
        
        Lat2Deg2 <-Rad2Deg(Lat2Rad2)
        Lon2Deg2 <-Rad2Deg(Lon2Rad2)
        
        lineLat<-c(lineLat,Lat2Deg2)# vector of all latitude of dots of this line
        lineLon<-c(lineLon,Lon2Deg2)# vector of all longitude of dots of this line
      }
      if(length(lineLat) & length(lineLon)){
        # only on line of vectors of lat and lon : apply to plotZonesRFCB where the is sum of lines segmets of couples of coordinates : one line = sum of segments, here it's only one line with vectors of coordinates : line lot (dotted, shaded) is more beautifull
        lines(lineLon,lineLat,lty=1,lwd = 3,type="l",col=Color)#lty 1 solid 2 dashed 3 dotted
      }
      # Keep this line below to be able to see difference between two type of lines : streight line between the two points, or reographically corrected line infunction of projection (above)
      # lines(c(mycoord[1],coords[1]),c(mycoord[2],coords[2]),lty=2,col="green")
    }
    

    
    plotTrainingFlightLine <- function(Coords,AngRad,Km,DKm,Color){
      v<-getInputValues() # get all values of input list
      # regarder comment on fait pour lignes des zones plus que pour les lignes de vol car = aussi un point, angle et distance
      Lat1Rad <- Deg2Rad(Coords[1])#Latitude of the center of the circle in radians#From degrees to radians rad= deg*(pi/180)
      Lon1Rad <- Deg2Rad(Coords[2])#Longitude of the center of the circle in radians

      Kms <- seq(Km[1],Km[2],by=DKm)
      if(!Km[2] %in% Kms){
        Kms<-c(Kms,Km[2])
      }

      for(j in 1:(length(Kms)-1)){
        Lat2Rad1 <- getLatFromAngleDistance(Lat1Rad,AngRad,Kms[j])
        Lon2Rad1 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad,Kms[j],Lat2Rad1)
        
        Lat2Rad2 <- getLatFromAngleDistance(Lat1Rad,AngRad,Kms[j+1])
        Lon2Rad2 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad,Kms[j+1],Lat2Rad2)
        
        Lat2Deg1 <-Rad2Deg(Lat2Rad1)
        Lon2Deg1 <-Rad2Deg(Lon2Rad1)
        
        Lat2Deg2 <-Rad2Deg(Lat2Rad2)
        Lon2Deg2 <-Rad2Deg(Lon2Rad2)

        lines(c(Lon2Deg1,Lon2Deg2),c(Lat2Deg1,Lat2Deg2),lty=2,type="l",col=Color)
      }
      LatTrainingDeg<-getTrainingLatDeg(Coords,AngRad,v$trainingdistance)
      LonTrainingDeg<-getTrainingLonDeg(Coords,AngRad,v$trainingdistance)
      points(LonTrainingDeg,LatTrainingDeg,pch=20,col=Color)
    }
    
    plotZonesRFCB <- function(Coords,AngDeg,Km,DKm,Color){
      Lat1Rad <- Deg2Rad(Coords[1])#Latitude of the center of the circle in radians#From degrees to radians rad= deg*(pi/180)
      Lon1Rad <- Deg2Rad(Coords[2])#Longitude of the center of the circle in radians
      AngRad <- Deg2Rad(AngDeg)
      Kms <- seq(Km[1],Km[2],by=DKm)
      if(!Km[2] %in% Kms){
        Kms<-c(Kms,Km[2])
      }
      for(i in 1:7){# Tracer les lignes
        for(j in 1:(length(Kms)-1)){
          Lat2Rad1 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Kms[j])
          Lon2Rad1 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Kms[j],Lat2Rad1)
          
          Lat2Rad2 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Kms[j+1])
          Lon2Rad2 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Kms[j+1],Lat2Rad2)
          
          Lat2Deg1 <-Rad2Deg(Lat2Rad1)
          Lon2Deg1 <-Rad2Deg(Lon2Rad1)
          
          Lat2Deg2 <-Rad2Deg(Lat2Rad2)
          Lon2Deg2 <-Rad2Deg(Lon2Rad2)
          if(i %in% c(2,4,6)){
            if(v$zonehidemiddle == FALSE) {
              lines(c(Lon2Deg1,Lon2Deg2),c(Lat2Deg1,Lat2Deg2),lty=2,col=Color)
            }
          } else {
            lines(c(Lon2Deg1,Lon2Deg2),c(Lat2Deg1,Lat2Deg2),col=Color)
          }
        }
      }
      # Compute % of fanciers when fanciers
      if(v$zonesfanciers){
        if(v$fanciers =="anvers1553"){
          fanciers <- read.csv("data/fanciers-belgium-anvers-1553.csv", sep=",", dec=".")
        }
        if(v$fanciers=="liege1580"){
          fanciers <- read.csv("data/fanciers-belgium-liege-1580.csv", sep=",", dec=".")
        }
        if(v$fanciers=="liege535"){
          fanciers <- read.csv("data/fanciers-belgium-liege-535.csv", sep=",", dec=".")
        }
        # calculer les angles avec lieux de lâcher (ici les ditances sont pas nécéssaires car zones 2014 ou 2015 sont en profondeurs)
        fanciers$anglerad <- NA
        for(i in 1:nrow(fanciers)){ #add angle to data
          fanciers[i,]$anglerad<-Deg2Rad(angleDeg(Coords[2],Coords[1],fanciers[i,]$longitude,fanciers[i,]$latitude))
        }
        nfanciersbyzones<-c()
        pcfanciersbyzones<-c()
        for(i in 2:length(AngRad)){
          if(v$zonehidemiddle == TRUE) {#si on veut masquer les demi-fond : calculer n et pc de 1 à 3, 3 - 5, 5 - 7, mais faire un array de 6 pour simplifier le plots. Idéalement on devrait ne calculer que une fois sur deux, mais j'optimiserait plus tard
            #faire i<-i+1 si i est pair
            if(i %% 2 == 0) {i<-i+1}
            subfanciers<-subset(fanciers, anglerad > AngRad[i-2] & anglerad <=AngRad[i])
          } else {
            subfanciers<-subset(fanciers, anglerad > AngRad[i-1] & anglerad <=AngRad[i])
          }
          nfanciersbyzones<-c(nfanciersbyzones,nrow(subfanciers))
          pcfanciersbyzones<-c(pcfanciersbyzones,round(nrow(subfanciers)/nrow(fanciers)*100,digits =2))
        }
      }
      #Plot labels
      AngDeg <- c(AngDeg[2]-2.5,AngDeg[3]-(AngDeg[3]-AngDeg[2])*0.5,AngDeg[4]-(AngDeg[4]-AngDeg[3])*0.5,AngDeg[5]-(AngDeg[5]-AngDeg[4])*0.5,AngDeg[6]-(AngDeg[6]-AngDeg[5])*0.5,AngDeg[6]+2.5)#Angles in radians defining zones relatively to Chastres
      if(v$zonehidemiddle == FALSE) {
        labels<-c("A1","A2","B1","B2","C1","C2")
      } else {
        labels<-c("A","A","B","B","C","C")#on met tout et on affiche 1/2 = plus simple que de tout calculer pour 3 valeurs au lieu de 6
      }
      Lat1Rad <- Deg2Rad(Coords[1])#Latitude of the center of the circle in radians#From degrees to radians rad= deg*(pi/180)
      Lon1Rad <- Deg2Rad(Coords[2])#Longitude of the center of the circle in radians
      AngRadLines <- AngRad #backup AngRad used to lines to plot labels when great distance only
      AngRad <- Deg2Rad(AngDeg)
      for(i in 1:6){
        if(v$zones=="2015"){
          midDistCoef<-2
        } else {#2014
          midDistCoef<-2.5
        }
        if(v$zonesfanciers){#si on affiche les fanciers on recalcule position de label et affiche le n(%)
          if(v$zonehidemiddle == FALSE) {
            Lat2Rad3 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(midDistCoef-0.1))
            Lon2Rad3 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(midDistCoef-0.1),Lat2Rad3)
            Lat2Deg3 <-Rad2Deg(Lat2Rad3)
            Lon2Deg3 <-Rad2Deg(Lon2Rad3)
            text(Lon2Deg3,Lat2Deg3,labels=labels[i],col=Color)
            
            Lat2Rad4 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(midDistCoef+0.2))
            Lon2Rad4 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(midDistCoef+0.2),Lat2Rad4)
            Lat2Deg4 <-Rad2Deg(Lat2Rad4)
            Lon2Deg4 <-Rad2Deg(Lon2Rad4)
            text(Lon2Deg4,Lat2Deg4,labels=paste(nfanciersbyzones[i],"(",pcfanciersbyzones[i],"%)",sep=''),col=Color)
          } else {
            #recalculer position labels et n(%)
            if(i %in% c(2,4,6)){
              # Recalculer les corrdonnées des labels pour calquer celle sde slignes intermédiaires
              Lat2Rad5 <- getLatFromAngleDistance(Lat1Rad,AngRadLines[i],Km[1]+(Km[2]-Km[1])/(midDistCoef-0.1))
              Lon2Rad5 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRadLines[i],Km[1]+(Km[2]-Km[1])/(midDistCoef-0.1),Lat2Rad5)
              Lat2Deg5 <-Rad2Deg(Lat2Rad5)
              Lon2Deg5 <-Rad2Deg(Lon2Rad5)
              text(Lon2Deg5,Lat2Deg5,labels= labels[i],col=Color)
              
              Lat2Rad6 <- getLatFromAngleDistance(Lat1Rad,AngRadLines[i],Km[1]+(Km[2]-Km[1])/(midDistCoef+0.2))
              Lon2Rad6 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRadLines[i],Km[1]+(Km[2]-Km[1])/(midDistCoef+0.2),Lat2Rad6)
              Lat2Deg6 <-Rad2Deg(Lat2Rad6)
              Lon2Deg6 <-Rad2Deg(Lon2Rad6)
              text(Lon2Deg6,Lat2Deg6,labels=paste(nfanciersbyzones[i],"(",pcfanciersbyzones[i],"%)",sep=''),col=Color)
            }
          }
        } else { #SI pas de fanciers => juste label 
          if(v$zonehidemiddle == FALSE) {
            Lat2Rad3 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/midDistCoef)
            Lon2Rad3 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/midDistCoef,Lat2Rad3)
            Lat2Deg3 <-Rad2Deg(Lat2Rad3)
            Lon2Deg3 <-Rad2Deg(Lon2Rad3)
            text(Lon2Deg3,Lat2Deg3,labels= labels[i],col=Color)
          } else {
            if(i %in% c(2,4,6)){
              # Recalculer les corrdonnées des labels pour calquer celle sde slignes intermédiaires
              Lat2Rad5 <- getLatFromAngleDistance(Lat1Rad,AngRadLines[i],Km[1]+(Km[2]-Km[1])/(midDistCoef))
              Lon2Rad5 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRadLines[i],Km[1]+(Km[2]-Km[1])/(midDistCoef),Lat2Rad5)
              Lat2Deg5 <-Rad2Deg(Lat2Rad5)
              Lon2Deg5 <-Rad2Deg(Lon2Rad5)
              text(Lon2Deg5,Lat2Deg5,labels= labels[i],col=Color)
            } 
          }
        }
      }
    }
    
    plotZonesDyn <- function(Coords,DKm,Color){#,AngDeg,Km
      v<-getInputValues() # get all values of input list
      cv<-getComputedValues()
      Lat1Rad <- Deg2Rad(Coords$y)#Latitude of the center of the circle in radians
      Lon1Rad <- Deg2Rad(Coords$x)#Longitude of the center of the circle in radians
      
      BelgiumMapData<-map("world2Hires","Belgium",resolution=0,plot=FALSE)
      BelgiumCoords<-list()
      BelgiumCoords$x<-BelgiumMapData$x[!is.na(BelgiumMapData$x)]
      BelgiumCoords$y<-BelgiumMapData$y[!is.na(BelgiumMapData$y)]
      
      AnglesZonesDeg<-c()
      for(i in 1:length(BelgiumCoords$x)){
        AnglesZonesDeg<-c(AnglesZonesDeg,angleDeg(Coords$x,Coords$y,BelgiumCoords$x[i],BelgiumCoords$y[i])) #(lon1,lat1,lon2,lat2))
      }

      AngleDegMin<-min(AnglesZonesDeg,na.rm = TRUE)
      AngleDegMax<-max(AnglesZonesDeg,na.rm = TRUE)
      #cat(paste("Min : ",AngleDegMin," |  Max : ",AngleDegMax))
      AngleRadMin<-Deg2Rad(AngleDegMin)
      AngleRadMax<-Deg2Rad(AngleDegMax)
      AngleRadDiff<-AngleRadMax-AngleRadMin
      
      # Compute % of fanciers when fanciers
      if(v$fanciers =="anvers1553"){
        fanciers <- read.csv("data/fanciers-belgium-anvers-1553.csv", sep=",", dec=".")
      }
      if(v$fanciers=="liege1580"){
        fanciers <- read.csv("data/fanciers-belgium-liege-1580.csv", sep=",", dec=".")
      }
      if(v$fanciers=="liege535"){
        fanciers <- read.csv("data/fanciers-belgium-liege-535.csv", sep=",", dec=".")
      }
      # calculer les angles avec lieux de lâcher (ici les ditances sont pas nécéssaires car zones 2014 ou 2015 sont en profondeurs)
      fanciers$anglerad <- NA
      fanciers$Km <- NA
      for(i in 1:nrow(fanciers)){ #add angle to data
        fanciers[i,]$anglerad<-Deg2Rad(angleDeg(Coords$x,Coords$y,fanciers[i,]$longitude,fanciers[i,]$latitude))
        if(v$zones=="dynvertcircrel" || v$zones=="dynvertcircrel2"){
          #calculer les distances de chaque fancier avec lieu de lâcher
          fanciers[i,]$Km <- getDistanceInMeters(Coords$x,Coords$y,fanciers[i,]$longitude,fanciers[i,]$latitude)/1000 #(lon1,lat1,lon2,lat2)
        }
      }
      # classer par ordre croissant
      fanciers<-fanciers[with(fanciers, order(anglerad)),]
      
      if(v$zones=="dynvertabs"){
        AngRad<-seq(from=AngleRadMin,to=AngleRadMax,length.out=7)
      }
      if(v$zones=="dynvertrel" || v$zones=="dynvertcircrel" || v$zones=="dynvertcircrel2"){
        # calculer les rangs et déterminer les limites des 100/6 % des fanciers concernés
        AngRad<-c()
        AngRad<-c(AngRad,AngleRadMin)
        for(a in 1:5){
          AngRad<-c(AngRad,(fanciers[floor(nrow(fanciers)*(a/6)),]$anglerad+fanciers[ceiling(nrow(fanciers)*(a/6)),]$anglerad)/2)#% pas égaux quand nrow(fanciers) n'est pas un multiple de 6
        }
        AngRad<-c(AngRad,AngleRadMax)
      }
      # ajouter le % de fancier par zone (doit pour ça avoir le AngRad et la distance calculés)
      if(v$zonesfanciers){
        nfanciersbyzones<-c()
        pcfanciersbyzones<-c()
        for(i in 2:length(AngRad)){
          if(v$zones=="dynvertabs" || v$zones=="dynvertrel") {
            if(v$zonehidemiddle == TRUE) {#si on veut masquer les demi-fond : calculer n et pc de 1 à 3, 3 - 5, 5 - 7, mais faire un array de 6 pour simplifier le plots. Idéalement on devrait ne calculer que une fois sur deux, mais j'optimiserait plus tard
              #faire i<-i+1 si i est pair
              if(i %% 2 == 0) {i<-i+1}
              subfanciers<-subset(fanciers, anglerad > AngRad[i-2] & anglerad <=AngRad[i])
            } else {
              subfanciers<-subset(fanciers, anglerad > AngRad[i-1] & anglerad <=AngRad[i])
            }
            nfanciersbyzones<-c(nfanciersbyzones,nrow(subfanciers))
            pcfanciersbyzones<-c(pcfanciersbyzones,round(nrow(subfanciers)/nrow(fanciers)*100,digits =2))
          } else {# un angle sur deux, et prendre la distance en compte
            if(i %in% c(2,4,6)){
              subfanciers<-subset(fanciers, anglerad > AngRad[i-1] & anglerad <=AngRad[i+1])
              if(v$zones=="dynvertcircrel2"){
                subsubfanciers<-subset(subfanciers, Km <= median(subfanciers$Km))
                nfanciersbyzones<-c(nfanciersbyzones,nrow(subsubfanciers))
                pcfanciersbyzones<-c(pcfanciersbyzones,round(nrow(subsubfanciers)/nrow(fanciers)*100,digits =2))
                
                subsubfanciers<-subset(subfanciers, Km > median(subfanciers$Km))
                nfanciersbyzones<-c(nfanciersbyzones,nrow(subsubfanciers))
                pcfanciersbyzones<-c(pcfanciersbyzones,round(nrow(subsubfanciers)/nrow(fanciers)*100,digits =2))
              } else {#dynvertcircrel
                subsubfanciers<-subset(subfanciers, Km <= v$zonecirccut)
                nfanciersbyzones<-c(nfanciersbyzones,nrow(subsubfanciers))
                pcfanciersbyzones<-c(pcfanciersbyzones,round(nrow(subsubfanciers)/nrow(fanciers)*100,digits =2))
                
                subsubfanciers<-subset(subfanciers, Km > v$zonecirccut)
                nfanciersbyzones<-c(nfanciersbyzones,nrow(subsubfanciers))
                pcfanciersbyzones<-c(pcfanciersbyzones,round(nrow(subsubfanciers)/nrow(fanciers)*100,digits =2))
                
              }
            }
          }
        }
      }
 
      # définir les distance entre lesquelle tracer les lignes verticales
      Km<-c()
      for(i in 1:length(BelgiumCoords$x)){
        Km<-c(Km,getDistanceInMeters(Coords$x,Coords$y,BelgiumCoords$x[i],BelgiumCoords$y[i])/1000) #(lon1,lat1,lon2,lat2))
      }
      Km<-c(min(Km)-10,max(Km)+10)
      Kms <- seq(Km[1],Km[2],by=DKm)
      if(!Km[2] %in% Kms){
        Kms<-c(Kms,Km[2])
      }
      
      if(v$zones=="dynvertcircrel2"){
        KmLabels<-c()
      }
      
      for(i in 1:length(AngRad)){
        for(j in 1:(length(Kms)-1)){
          Lat2Rad1 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Kms[j])
          Lon2Rad1 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Kms[j],Lat2Rad1)

          Lat2Rad2 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Kms[j+1])
          Lon2Rad2 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Kms[j+1],Lat2Rad2)

          Lat2Deg1 <-Rad2Deg(Lat2Rad1)
          Lon2Deg1 <-Rad2Deg(Lon2Rad1)

          Lat2Deg2 <-Rad2Deg(Lat2Rad2)
          Lon2Deg2 <-Rad2Deg(Lon2Rad2)
          if(i %in% c(2,4,6)){
            if(v$zones!="dynvertcircrel" && v$zones!="dynvertcircrel2"){
              if(v$zonehidemiddle == FALSE){
                lines(c(Lon2Deg1,Lon2Deg2),c(Lat2Deg1,Lat2Deg2),lty=2,col=Color)
              }
            }
          } else {#i=1,3,5,7
            lines(c(Lon2Deg1,Lon2Deg2),c(Lat2Deg1,Lat2Deg2),col=Color)
          }
        }
        if(v$zones == "dynvertcircrel" && i %in% c(1,3,5)){
          plotDist(Coords$y,Coords$x,v$zonecirccut,"#666666",Rad2Deg(AngRad[i]),Rad2Deg(AngRad[i+2]))
        }
        if((v$zones=="dynvertcircrel2") && i %in% c(1,3,5)){# Pour tous les angles sauf le dernier, dessiner l'arc de l'angle jusqu'à l'angle suivant
          # Entre l'angle et l'angle suivant : sélectionner les coordonnées du sample, calculer leur distance, et en trouver la médiane qu'on file à l'arc
          subfanciers<-subset(fanciers, anglerad >= AngRad[i] & anglerad < AngRad[i+2])
          KmLabels<-c(KmLabels,median(subfanciers$Km))
          plotDist(Coords$y,Coords$x,median(subfanciers$Km),"#666666",Rad2Deg(AngRad[i]),Rad2Deg(AngRad[i+2]))# TODO
        }
      }
      AngRadLines<-AngRad #Backup of AngRad for lines to plot great distance labels at the same angle that lines on middle distance when midle distance is hided
      #Plot labels
      Deltas<-c()
      if(v$zones!="dynvertcircrel" && v$zones!="dynvertcircrel2"){
        for(i in 2:7){
          Deltas<-c(Deltas,(AngRad[i]-AngRad[i-1])/2)
        }
        AngRad<-AngRad[1:6]
      } else {
        for(i in c(3,5,7)){
          Deltas<-c(Deltas,(AngRad[i]-AngRad[i-2])/2)
        }
        AngRad<-AngRad[c(1,3,5)]
      }
      #Delta<-(AngRad[2]-AngRad[1])/2
      AngRad<-AngRad+Deltas
      #AngRad<-c(AngRad,Delta+AngRad[5]+Delta)
      if(v$zones=="dynvertcircrel" || v$zones=="dynvertcircrel2") {
        labels<-c("A1","A2","B1","B2","C1","C2")
      } else {
        if(v$zonehidemiddle == FALSE){
          labels<-c("A1","A2","B1","B2","C1","C2")
        } else {
          labels<-c("A","A","B","B","C","C")#on met tout et on affiche 1/2 = plus simple que de tout calculer pour 3 valeurs au lieu de 6
        }
      }
      Lat1Rad <- Deg2Rad(Coords$y)#Latitude of the center of the circle in radians
      Lon1Rad <- Deg2Rad(Coords$x)#Longitude of the center of the circle in radians
      if(v$zones!="dynvertcircrel" && v$zones!="dynvertcircrel2"){
        steps<-c(1:6)
      } else {
        steps<-c(1:3)
      }
      j<-c(0)
      for(i in steps){
        if(v$zones=="dynvertcircrel" || v$zones=="dynvertcircrel2"){
          if(v$zones=="dynvertcircrel2"){
            KmLabel<-c(KmLabels[i]-15,KmLabels[i]+15)
          } else {
            KmLabel<-c(v$zonecirccut-15,v$zonecirccut+15)
          }
          Lat2Rad1 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],KmLabel[1])
          Lon2Rad1 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],KmLabel[1],Lat2Rad1)
          Lat2Deg1 <-Rad2Deg(Lat2Rad1)
          Lon2Deg1 <-Rad2Deg(Lon2Rad1)
          Lat2Rad2 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],KmLabel[2])
          Lon2Rad2 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],KmLabel[2],Lat2Rad2)
          Lat2Deg2 <-Rad2Deg(Lat2Rad2)
          Lon2Deg2 <-Rad2Deg(Lon2Rad2)
          if(v$zonesfanciers){
            Lat2Rad3 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],KmLabel[1]-10)
            Lon2Rad3 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],KmLabel[1]-10,Lat2Rad3)
            Lat2Deg3 <-Rad2Deg(Lat2Rad3)
            Lon2Deg3 <-Rad2Deg(Lon2Rad3)
            
            Lat2Rad4 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],KmLabel[2]+10)
            Lon2Rad4 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],KmLabel[2]+10,Lat2Rad4)
            Lat2Deg4 <-Rad2Deg(Lat2Rad4)
            Lon2Deg4 <-Rad2Deg(Lon2Rad4)
          } 

        } else { # when not dynvertcircrel2 or dynvertcircrel2 : so when dynvertabs or dynvertrel
          
          if(v$zonesfanciers){#3 et #4 c'est quand on montre le DF. #5 et #§ c'est quand on masque le DF -> on montre que le fond
            if(v$zonehidemiddle == FALSE){#3 et #4
              Lat2Rad3 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(2-0.1))
              Lon2Rad3 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(2-0.1),Lat2Rad3)
              Lat2Deg3 <-Rad2Deg(Lat2Rad3)
              Lon2Deg3 <-Rad2Deg(Lon2Rad3)
                                 
              Lat2Rad4 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(2+0.1))
              Lon2Rad4 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(2+0.1),Lat2Rad4)
              Lat2Deg4 <-Rad2Deg(Lat2Rad4)
              Lon2Deg4 <-Rad2Deg(Lon2Rad4)
            } else {#5 et #6 : on montre les fanciers ET on masque les DF
              if(i %in% c(2,4,6)){
                if(v$zones=="dynvertrel"){# TODO : if dynvertrel : recalculer l'écart entre lignes pleines pour remettra label plus au milieu
                  DeltasVertRel<-(AngRadLines[i+1]-AngRadLines[i-1])/2
                  Lat2Rad5 <- getLatFromAngleDistance(Lat1Rad,AngRadLines[i-1]+DeltasVertRel,Km[1]+(Km[2]-Km[1])/(2-0.1))
                  Lon2Rad5 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRadLines[i-1]+DeltasVertRel,Km[1]+(Km[2]-Km[1])/(2-0.1),Lat2Rad5)
                  Lat2Deg5 <-Rad2Deg(Lat2Rad5)
                  Lon2Deg5 <-Rad2Deg(Lon2Rad5)
                  
                  Lat2Rad6 <- getLatFromAngleDistance(Lat1Rad,AngRadLines[i-1]+DeltasVertRel,Km[1]+(Km[2]-Km[1])/(2+0.1))
                  Lon2Rad6 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRadLines[i-1]+DeltasVertRel,Km[1]+(Km[2]-Km[1])/(2+0.1),Lat2Rad6)
                  Lat2Deg6 <-Rad2Deg(Lat2Rad6)
                  Lon2Deg6 <-Rad2Deg(Lon2Rad6)
                  
                } else {#dynvertabs
                  Lat2Rad5 <- getLatFromAngleDistance(Lat1Rad,AngRadLines[i],Km[1]+(Km[2]-Km[1])/(2-0.1))
                  Lon2Rad5 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRadLines[i],Km[1]+(Km[2]-Km[1])/(2-0.1),Lat2Rad5)
                  Lat2Deg5 <-Rad2Deg(Lat2Rad5)
                  Lon2Deg5 <-Rad2Deg(Lon2Rad5)
                  
                  Lat2Rad6 <- getLatFromAngleDistance(Lat1Rad,AngRadLines[i],Km[1]+(Km[2]-Km[1])/(2+0.1))
                  Lon2Rad6 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRadLines[i],Km[1]+(Km[2]-Km[1])/(2+0.1),Lat2Rad6)
                  Lat2Deg6 <-Rad2Deg(Lat2Rad6)
                  Lon2Deg6 <-Rad2Deg(Lon2Rad6)
                  
                }
              }
            }
          } else {#pas zonefanciers
            if(v$zonehidemiddle == FALSE){
              Lat2Rad3 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/2)
              Lon2Rad3 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/2,Lat2Rad3)
              Lat2Deg3 <-Rad2Deg(Lat2Rad3)
              Lon2Deg3 <-Rad2Deg(Lon2Rad3)
            } else {
              if(i %in% c(2,4,6)){
                if(v$zones=="dynvertrel"){# TODO : if dynvertrel : recalculer l'écart entre lignes pleines pour remettra label plus au milieu
                  DeltasVertRel<-(AngRadLines[i+1]-AngRadLines[i-1])/2
                  Lat2Rad5 <- getLatFromAngleDistance(Lat1Rad,AngRadLines[i-1]+DeltasVertRel,Km[1]+(Km[2]-Km[1])/2)
                  Lon2Rad5 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRadLines[i-1]+DeltasVertRel,Km[1]+(Km[2]-Km[1])/2,Lat2Rad5)
                  Lat2Deg5 <-Rad2Deg(Lat2Rad5)
                  Lon2Deg5 <-Rad2Deg(Lon2Rad5)
                } else {#dynvertabs
                  Lat2Rad5 <- getLatFromAngleDistance(Lat1Rad,AngRadLines[i],Km[1]+(Km[2]-Km[1])/2)
                  Lon2Rad5 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRadLines[i],Km[1]+(Km[2]-Km[1])/2,Lat2Rad5)
                  Lat2Deg5 <-Rad2Deg(Lat2Rad5)
                  Lon2Deg5 <-Rad2Deg(Lon2Rad5)
                }
              }
            }
          }
        }

        if(v$zones!="dynvertcircrel" && v$zones!="dynvertcircrel2"){#dynvertabs or dynvertrel
          if(v$zonesfanciers){#On montre les fanciers ==> recalculer les positions labels et n(%) qd on masque le DF
            if(v$zonehidemiddle == FALSE){
              text(Lon2Deg3,Lat2Deg3,labels= labels[i],col=Color)
              text(Lon2Deg4,Lat2Deg4,labels=paste(nfanciersbyzones[i],"(",pcfanciersbyzones[i],"%)",sep=''),col=Color)
            } else {#recalculer les positions labels et n(%) qd on masque le DF
              if(i %in% c(2,4,6)){
                text(Lon2Deg5,Lat2Deg5,labels= labels[i],col=Color)
                text(Lon2Deg6,Lat2Deg6,labels=paste(nfanciersbyzones[i],"(",pcfanciersbyzones[i],"%)",sep=''),col=Color)
              }
            }
          } else {#On montre PAS les fanciers => QUE le label = ok
            if(v$zonehidemiddle == FALSE){
              text(Lon2Deg3,Lat2Deg3,labels= labels[i],col=Color)
            } else { # When Hide Middle Distance Label 
              if(i %in% c(2,4,6)){
                if(v$zones=="dynvertrel"){# TODO : if dynvertrel : recalculer l'écart entre lignes pleines pour remettra label plus au milieu
                  text(Lon2Deg5,Lat2Deg5,labels= labels[i],col=Color)
                } else {#dynvertabs
                  text(Lon2Deg5,Lat2Deg5,labels= labels[i],col=Color)
                }
              }
            }
          }
        } else {#dynvertcircrel et dynvertcircrel2 
          text(Lon2Deg1,Lat2Deg1,labels= labels[i+j],col=Color)
          text(Lon2Deg2,Lat2Deg2,labels= labels[i+j+1],col=Color)
          if(v$zonesfanciers){
            text(Lon2Deg3,Lat2Deg3,labels= paste(nfanciersbyzones[i],"(",pcfanciersbyzones[i+j],"%)",sep=''),col=Color)
            text(Lon2Deg4,Lat2Deg4,labels= paste(nfanciersbyzones[i],"(",pcfanciersbyzones[i+j+1],"%)",sep=''),col=Color)
          } 
          j<-j+1
        }

      }
    }
    
    AsSexa <-function(Sexa) {
      Deg<-floor(Sexa/10000)
      Min<-round(((floor(Sexa/100)/100-floor(floor(Sexa/100)/100))*100),0)
      for(i in 1:length(Min)) {
        if(Min[i]==60){
          Min[i]<-0
          Deg[i]<-Deg[i]+1
        } else {
          if(Min[i]>60){
            Min[i]<-59
          }
        }
      }
      Sec<-round(((Sexa/100-floor(Sexa/100))*100),1)
      for(i in 1:length(Sec)){
        if(Sec[i]==60){
          Sec[i]<-0
          Min[i]<-Min[i]+1
        } else {
          if(Sec[i]>60){
            Sec[i]<-59
          }
        }
      }
      return(Deg*10000+Min*100+Sec)
    }
    
    #WGS84 are Sexagecimal : but numeric input is in base 10 : here is the function to force these inputs to work as if they where in Sexagesimal
    WGS84FieldsNames<-c('MLatMax', 'MLatMin', 'MLonMax', 'MLonMin',"z2020Lat","z2020NLon1","z2020NLon2","z2020SLon1","z2020SLon2")
    WGS84FieldsValues<-c()
    for(k in 1:length(WGS84FieldsNames)){
      WGS84FieldsValues<-c(WGS84FieldsValues,input[[WGS84FieldsNames[k]]])
    }
    observeEvent(WGS84FieldsValues, {
      Sexa <- WGS84FieldsValues
      SexaCorrected<-AsSexa(Sexa)
      for(i in 1:length(Sexa)){
        if(SexaCorrected[i] != Sexa[i]){
          updateNumericInput(session, WGS84FieldsNames[i], value = SexaCorrected[i])
        }
      }
    })
    
    AngDeg2RadSectorsSouth<-function(AngDeg){#AngDeg = 0 = North #AngDeg > 0 = North-> Est -> South #AngDeg <0 = North -> West ->South
      if(AngDeg < 0){ # Est or West
        if(AngDeg < -90){ # Notrh or South
          AngRad <- Deg2Rad(AngDeg)
        } else {
          AngRad <- Deg2Rad(AngDeg+180)
        }
      } else {
        if(AngDeg > 90){# Notrh or South
          AngRad <- Deg2Rad(AngDeg)
        } else {
          AngRad <- Deg2Rad(AngDeg+180)
        }
      }
      return(AngRad)
    }
    # TODO : voir Deg2Rad2Deg.txt
    AngDeg2RadSectorsNorth<-function(AngDeg){#AngDeg = 0 = North #AngDeg > 0 = North-> Est -> South #AngDeg <0 = North -> West ->South
      if(AngDeg < 0){ # Est or West
        if(AngDeg < -90){ # Notrh or South
          AngRad <- Deg2Rad(AngDeg+180)
        } else {
          AngRad <- Deg2Rad(AngDeg)
        }
      } else {
        if(AngDeg > 90){# Notrh or South
          AngRad <- Deg2Rad(AngDeg+180)
        } else {
          AngRad <- Deg2Rad(AngDeg)
        }
      }
      return(AngRad)
    }
    
    plotZonesRFCB2020Sectors <- function(Coords,AngRad,Km){#,DKm,Color
      Lat1Rad <- Deg2Rad(Coords[1])#Latitude of the center of the circle in radians#From degrees to radians rad= deg*(pi/180)
      Lon1Rad <- Deg2Rad(Coords[2])#Longitude of the center of the circle in radians
      
      DKm<-5
      Color<-'black'
      Kms <- seq(Km[1],Km[2],by=DKm)
      if(!Km[2] %in% Kms){
        Kms<-c(Kms,Km[2])
      }
      
      for(j in 1:(length(Kms)-1)){
        Lat2Rad1 <- getLatFromAngleDistance(Lat1Rad,AngRad,Kms[j])
        Lon2Rad1 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad,Kms[j],Lat2Rad1)
        
        Lat2Rad2 <- getLatFromAngleDistance(Lat1Rad,AngRad,Kms[j+1])
        Lon2Rad2 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad,Kms[j+1],Lat2Rad2)
        
        Lat2Deg1 <-Rad2Deg(Lat2Rad1)
        Lon2Deg1 <-Rad2Deg(Lon2Rad1)
        
        Lat2Deg2 <-Rad2Deg(Lat2Rad2)
        Lon2Deg2 <-Rad2Deg(Lon2Rad2)
        
        lines(c(Lon2Deg1,Lon2Deg2),c(Lat2Deg1,Lat2Deg2),type="l",col=Color)
      }

    }
    
    getZonesRFCB2020SectorDist <- function(data, Lat, Lon, angledeg) {#data must have x, y anglerad et KM names
      # For the Km : take Belgium coordinates, compute angles form center, and then subset coordinates that have the same angle that the one set here, then compute distance
      for(i in 1:nrow(data)){ #add angle to data
        data[i,]$anglerad<-Deg2Rad(angleDeg(Sexa2Dec(Lon),Sexa2Dec(Lat),data[i,]$x,data[i,]$y))
        data[i,]$Km <- floor(getDistanceInMeters(Sexa2Dec(Lon),Sexa2Dec(Lat),data[i,]$x,data[i,]$y)/1000) #(lon1,lat1,lon2,lat2)
      }
      subset<-subset(data, anglerad >= Deg2Rad(angledeg-1) & anglerad <= Deg2Rad(angledeg+1))
      return(min(subset$Km))#min : stop at first met boundary #max : stop at last met boundary
    }
    
    plotZonesRFCB2020 <- function(){# Sones S1 S2 S3 (Sud et secteurs 1 2 3), et N1, N2, N3 (Nord, 1 2 3)
      # Soit les secteurs sont perpendiculaires : c'était le cas avant et c'est le cas de la plupart de lignes commentées ici
      # Soit les secteurs sont à angle constant (pas plus de sens que perpendiculaire)
      # Soit les secteurs snt une projection depuis Limoges, comme les zones 2015
      # Donc 2 situations : Angle constant (dont 90° n'est qu'une valeur possible) soit une projection : faisons déjà à angle constant car si c'est pour une projection autant passer aux zones dynamiques. En plus il reste un slot dans le tableau de valeurs pour y glisser l'angle ;-)
      BelgiumMapData<-map("world2Hires","Belgium",resolution=0,plot=FALSE)
      BelgiumCoords<-list()
      BelgiumCoords$x<-BelgiumMapData$x[!is.na(BelgiumMapData$x)]
      BelgiumCoords$y<-BelgiumMapData$y[!is.na(BelgiumMapData$y)]
      BelgiumCoords$anglerad <- NA
      BelgiumCoords$Km <- NA
      BelgiumCoordData<-as.data.frame(BelgiumCoords)
      
      #cat(v$z2020NLat1)
      subset<-subset(BelgiumCoordData, y >= Sexa2Dec(v$z2020Lat)-0.01 & y <= Sexa2Dec(v$z2020Lat)+0.01)
      rightsubset<-subset(subset, x<=min(subset$x)+((max(subset$x)-min(subset$x))/3))#select coordinates of the rigth third of the country :to avoir bar to go throw comins-warneton
      leftsubset<-subset(subset, x>=max(subset$x)-((max(subset$x)-min(subset$x))/3))#select coordinates of the left third of the country :to avoir bar to go throw foeron
      x <-seq(from =max(rightsubset$x), to =min(leftsubset$x), length.out=100)
      for(i in 2:length(x) ){
        lines(c(x[i-1],x[i]),c(Sexa2Dec(v$z2020Lat),Sexa2Dec(v$z2020Lat)))#separateur horizontal
      }
      #Nord
      subset<-subset(BelgiumCoordData, y >= Sexa2Dec(v$z2020Lat))
      NordBel<-subset #BU for angle/distances computations
      text(x=min(subset$x)+(Sexa2Dec(v$z2020NLon1)-min(subset$x))/2,y=Sexa2Dec(v$z2020Lat)+(max(BelgiumCoords$y)-Sexa2Dec(v$z2020Lat))/20,labels="N1")
      text(x=Sexa2Dec(v$z2020NLon1)+(Sexa2Dec(v$z2020NLon2)-Sexa2Dec(v$z2020NLon1))/2,y=Sexa2Dec(v$z2020Lat)+(max(BelgiumCoords$y)-Sexa2Dec(v$z2020Lat))/20,labels="N2")
      text(x=Sexa2Dec(v$z2020NLon2)+(Sexa2Dec(v$z2020NLon3)-Sexa2Dec(v$z2020NLon2))/2,y=Sexa2Dec(v$z2020Lat)+(max(BelgiumCoords$y)-Sexa2Dec(v$z2020Lat))/20,labels="N3")
      text(x=Sexa2Dec(v$z2020NLon3)+(max(subset$x)-Sexa2Dec(v$z2020NLon3))/2,y=Sexa2Dec(v$z2020Lat)+(max(BelgiumCoords$y)-Sexa2Dec(v$z2020Lat))/20,labels="N4")
      # subset<-subset(BelgiumCoordData, x >= Sexa2Dec(v$z2020NLon1)-0.01 & x <= Sexa2Dec(v$z2020NLon1)+0.01)
      # y <-seq(from =Sexa2Dec(v$z2020Lat), to =max(subset$y), length.out=100)
      # for(i in 2:length(y) ){
      #   lines(c(Sexa2Dec(v$z2020NLon1),Sexa2Dec(v$z2020NLon1)),c(y[i-1],y[i]))#separateur vertical
      # }
      plotZonesRFCB2020Sectors(c(Sexa2Dec(v$z2020Lat),Sexa2Dec(v$z2020NLon1)),AngDeg2RadSectorsNorth(v$z2020Angle),c(0,getZonesRFCB2020SectorDist(NordBel,v$z2020Lat, v$z2020NLon1, v$z2020Angle)))

      
      # subset<-subset(BelgiumCoordData, x >= Sexa2Dec(v$z2020NLon2)-0.01 & x <= Sexa2Dec(v$z2020NLon2)+0.01)
      # y <-seq(from =Sexa2Dec(v$z2020Lat), to =max(subset$y), length.out=100)
      # for(i in 2:length(y) ){
      #   lines(c(Sexa2Dec(v$z2020NLon2),Sexa2Dec(v$z2020NLon2)),c(y[i-1],y[i]))#separateur vertical
      # }
      plotZonesRFCB2020Sectors(c(Sexa2Dec(v$z2020Lat),Sexa2Dec(v$z2020NLon2)),AngDeg2RadSectorsNorth(v$z2020Angle),c(0,getZonesRFCB2020SectorDist(NordBel,v$z2020Lat, v$z2020NLon2, v$z2020Angle)))
      
      plotZonesRFCB2020Sectors(c(Sexa2Dec(v$z2020Lat),Sexa2Dec(v$z2020NLon3)),AngDeg2RadSectorsNorth(v$z2020Angle),c(0,getZonesRFCB2020SectorDist(NordBel,v$z2020Lat, v$z2020NLon3, v$z2020Angle)))
      
      #Sud
      subset<-subset(BelgiumCoordData, y <= Sexa2Dec(v$z2020Lat))
      SudBel<-subset #BU for angle/distances computations
      text(x=min(subset$x)+(Sexa2Dec(v$z2020SLon1)-min(subset$x))/2,y=Sexa2Dec(v$z2020Lat)-(Sexa2Dec(v$z2020Lat)-min(BelgiumCoords$y))/30,labels="S1")
      text(x=Sexa2Dec(v$z2020SLon1)+(Sexa2Dec(v$z2020SLon2)-Sexa2Dec(v$z2020SLon1))/2,y=Sexa2Dec(v$z2020Lat)-(Sexa2Dec(v$z2020Lat)-min(BelgiumCoords$y))/30,labels="S2")
      text(x=Sexa2Dec(v$z2020SLon2)+(max(subset$x)-Sexa2Dec(v$z2020SLon2))/2,y=Sexa2Dec(v$z2020Lat)-(Sexa2Dec(v$z2020Lat)-min(BelgiumCoords$y))/30,labels="S3")
      
      # subset<-subset(BelgiumCoordData, x >= Sexa2Dec(v$z2020SLon1)-0.01 & x <= Sexa2Dec(v$z2020SLon1)+0.01)
      # y <-seq(from =min(subset$y), to =Sexa2Dec(v$z2020Lat), length.out=100)
      # for(i in 2:length(y) ){
      #   lines(c(Sexa2Dec(v$z2020SLon1),Sexa2Dec(v$z2020SLon1)),c(y[i-1],y[i]))#separateur vertical
      # }
      #plotZonesRFCB2020Sectors(c(Sexa2Dec(v$z2020Lat),Sexa2Dec(v$z2020SLon1)),Deg2Rad(v$z2020Angle-180))
      plotZonesRFCB2020Sectors(c(Sexa2Dec(v$z2020Lat),Sexa2Dec(v$z2020SLon1)),AngDeg2RadSectorsSouth(v$z2020Angle),c(0,getZonesRFCB2020SectorDist(SudBel,v$z2020Lat, v$z2020SLon1, v$z2020Angle)))
      
      # subset<-subset(BelgiumCoordData, x >= Sexa2Dec(v$z2020SLon2)-0.01 & x <= Sexa2Dec(v$z2020SLon2)+0.01)
      # y <-seq(from =min(subset$y), to =Sexa2Dec(v$z2020Lat), length.out=100)
      # for(i in 2:length(y) ){
      #   lines(c(Sexa2Dec(v$z2020SLon2),Sexa2Dec(v$z2020SLon2)),c(y[i-1],y[i]))#separateur vertical
      # }
      #plotZonesRFCB2020Sectors(c(Sexa2Dec(v$z2020Lat),Sexa2Dec(v$z2020SLon2)),Deg2Rad(v$z2020Angle-180))
      plotZonesRFCB2020Sectors(c(Sexa2Dec(v$z2020Lat),Sexa2Dec(v$z2020SLon2)),AngDeg2RadSectorsSouth(v$z2020Angle),c(0,getZonesRFCB2020SectorDist(SudBel,v$z2020Lat, v$z2020SLon2, v$z2020Angle)))
    }
    
    plotZonesDynCircVertRel <- function(Coords){#,AngDeg,Km,,DKm,Color
      # First subset fanciers in 2 circular zone based on a 3/7 %
      # Then cut the first zones in 3 ans de second zone in 4

      # Compute % of fanciers when fanciers
      if(v$fanciers =="anvers1553"){
        fanciers <- read.csv("data/fanciers-belgium-anvers-1553.csv", sep=",", dec=".")
      }
      if(v$fanciers=="liege1580"){
        fanciers <- read.csv("data/fanciers-belgium-liege-1580.csv", sep=",", dec=".")
      }
      if(v$fanciers=="liege535"){
        fanciers <- read.csv("data/fanciers-belgium-liege-535.csv", sep=",", dec=".")
      }
      fanciers$anglerad <- NA
      fanciers$Km <- NA
      for(i in 1:nrow(fanciers)){ #add angle to data
        fanciers[i,]$anglerad<-Deg2Rad(angleDeg(Coords$x,Coords$y,fanciers[i,]$longitude,fanciers[i,]$latitude))
        fanciers[i,]$Km <- getDistanceInMeters(Coords$x,Coords$y,fanciers[i,]$longitude,fanciers[i,]$latitude)/1000 #(lon1,lat1,lon2,lat2)
      }
      fanciers<-fanciers[with(fanciers, order(Km)),]# order by distance
      cut<-(floor(nrow(fanciers))/(v$zCircVertNSectN+v$zCircVertNSectS))*v$zCircVertNSectS# set the rank for cutting
      cuttingKm<-max(head(fanciers,cut)$Km)

      # # set angle for arc
      Lat1Rad <- Deg2Rad(Coords$y)#Latitude of the center of the circle in radians
      Lon1Rad <- Deg2Rad(Coords$x)#Longitude of the center of the circle in radians
      
      BelgiumMapData<-map("world2Hires","Belgium",resolution=0,plot=FALSE)
      BelgiumCoords<-list()
      BelgiumCoords$x<-BelgiumMapData$x[!is.na(BelgiumMapData$x)]
      BelgiumCoords$y<-BelgiumMapData$y[!is.na(BelgiumMapData$y)]
      BelgiumCoords$angledeg <- NA
      BelgiumCoords$anglerad <- NA
      BelgiumCoords$Km <- NA
      BelgiumCoordData<-as.data.frame(BelgiumCoords)
      for(i in 1:nrow(BelgiumCoordData)){ #add angle to data
        BelgiumCoordData[i,]$angledeg<-angleDeg(Coords$x,Coords$y,BelgiumCoordData[i,]$x,BelgiumCoordData[i,]$y)
        BelgiumCoordData[i,]$anglerad<-Deg2Rad(BelgiumCoordData[i,]$angledeg)
        BelgiumCoordData[i,]$Km <- getDistanceInMeters(Coords$x,Coords$y,BelgiumCoordData[i,]$x,BelgiumCoordData[i,]$y)/1000 #(lon1,lat1,lon2,lat2)
      }
      AngleDegMin<-min(BelgiumCoordData$angledeg,na.rm = TRUE)#for plotDist
      AngleDegMax<-max(BelgiumCoordData$angledeg,na.rm = TRUE)#for plotDist
      AngleRadMin<-Deg2Rad(AngleDegMin)
      AngleRadMax<-Deg2Rad(AngleDegMax)
      #AngleRadDiff<-AngleRadMax-AngleRadMin

      plotDist(Coords$y,Coords$x,cuttingKm,"#666666",AngleDegMin,AngleDegMax)
      
      #step 2 : cut the zones : first the 3 zones south, then the 4 zones north
      subsetbel<-subset(BelgiumCoordData, Km > cuttingKm)
      subsetfanciers<-subset(fanciers, Km > cuttingKm)
      subsetfanciers<-subsetfanciers[with(subsetfanciers, order(anglerad)),]# order by angle
      AngRad<-c()
      AngRad<-c(AngRad,min(subsetbel$anglerad,na.rm = TRUE))
      for(a in 1:(v$zCircVertNSectN)-1){
        AngRad<-c(AngRad,(subsetfanciers[floor(nrow(subsetfanciers)*(a/v$zCircVertNSectN)),]$anglerad+subsetfanciers[ceiling(nrow(subsetfanciers)*(a/v$zCircVertNSectN)),]$anglerad)/2)
      }
      AngRad<-c(AngRad,max(subsetbel$anglerad,na.rm = TRUE))
      
      for(i in 1:length(AngRad)){
          Lat2Rad1 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],cuttingKm)
          Lon2Rad1 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],cuttingKm,Lat2Rad1)
          Lat2Rad2 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],max(subsetbel$Km))
          Lon2Rad2 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],max(subsetbel$Km),Lat2Rad2)
          Lat2Deg1 <-Rad2Deg(Lat2Rad1)
          Lon2Deg1 <-Rad2Deg(Lon2Rad1)
          Lat2Deg2 <-Rad2Deg(Lat2Rad2)
          Lon2Deg2 <-Rad2Deg(Lon2Rad2)
          lines(c(Lon2Deg1,Lon2Deg2),c(Lat2Deg1,Lat2Deg2),col='black')
      }
      # Labels
      labels<-paste("N",c(1:v$zCircVertNSectN,sep=""))
      # n(%) of fanciers
      nfanciersbyzones<-c()
      pcfanciersbyzones<-c()
      for(i in 2:length(AngRad)){
        subfanciers<-subset(subsetfanciers, anglerad > AngRad[i-1] & anglerad <=AngRad[i])
        nfanciersbyzones<-c(nfanciersbyzones,nrow(subfanciers))
        pcfanciersbyzones<-c(pcfanciersbyzones,round(nrow(subfanciers)/nrow(fanciers)*100,digits =2))
      }
      # DKm<-5
      Color<-'black'
      # Kms <- seq(cuttingKm,max(subsetbel$Km),by=DKm)
      # if(!Km[2] %in% Kms){
      #   Kms<-c(Kms,Km[2])
      # }
      Km<-c(cuttingKm,max(subsetbel$Km))
      Deltas<-c()
      for(i in 2:length(AngRad)){
        Deltas<-c(Deltas,(AngRad[i]-AngRad[i-1])/2)
      }
      AngRad<-AngRad[1:length(AngRad)-1]
      AngRad<-AngRad+Deltas
      #plot labels
      for(i in 1:length(AngRad)){
        if(v$zonesfanciers){
          Lat2Rad3 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(3-0.4))
          Lon2Rad3 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(3-0.4),Lat2Rad3)
          Lat2Deg3 <-Rad2Deg(Lat2Rad3)
          Lon2Deg3 <-Rad2Deg(Lon2Rad3)
          
          Lat2Rad4 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(3+0.4))
          Lon2Rad4 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(3+0.4),Lat2Rad4)
          Lat2Deg4 <-Rad2Deg(Lat2Rad4)
          Lon2Deg4 <-Rad2Deg(Lon2Rad4)
          
          text(Lon2Deg3,Lat2Deg3,labels= labels[i],col=Color)
          text(Lon2Deg4,Lat2Deg4,labels=paste(nfanciersbyzones[i],"(",pcfanciersbyzones[i],"%)",sep=''),col=Color)
        } else {
          Lat2Rad3 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(3))
          Lon2Rad3 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Km[1]+(Km[2]-Km[1])/(3),Lat2Rad3)
          Lat2Deg3 <-Rad2Deg(Lat2Rad3)
          Lon2Deg3 <-Rad2Deg(Lon2Rad3)
          
          text(Lon2Deg3,Lat2Deg3,labels= labels[i],col=Color)
        }
      }

      #South
      subsetbel<-subset(BelgiumCoordData, Km <= cuttingKm)
      subsetfanciers<-subset(fanciers, Km <= cuttingKm)
      subsetfanciers<-subsetfanciers[with(subsetfanciers, order(anglerad)),]# order by angle
      AngRad<-c()
      AngRad<-c(AngRad,min(subsetbel$anglerad,na.rm = TRUE))
      for(a in 1:(v$zCircVertNSectS-1)){
        AngRad<-c(AngRad,(subsetfanciers[floor(nrow(subsetfanciers)*(a/v$zCircVertNSectS)),]$anglerad+subsetfanciers[ceiling(nrow(subsetfanciers)*(a/v$zCircVertNSectS)),]$anglerad)/2)
      }
      AngRad<-c(AngRad,max(subsetbel$anglerad,na.rm = TRUE))

      for(i in 1:length(AngRad)){
        Lat2Rad1 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],min(subsetbel$Km,na.rm = TRUE))
        Lon2Rad1 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],min(subsetbel$Km,na.rm = TRUE),Lat2Rad1)
        Lat2Rad2 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],cuttingKm)
        Lon2Rad2 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],cuttingKm,Lat2Rad2)
        Lat2Deg1 <-Rad2Deg(Lat2Rad1)
        Lon2Deg1 <-Rad2Deg(Lon2Rad1)
        Lat2Deg2 <-Rad2Deg(Lat2Rad2)
        Lon2Deg2 <-Rad2Deg(Lon2Rad2)
        lines(c(Lon2Deg1,Lon2Deg2),c(Lat2Deg1,Lat2Deg2),col='black')
      }
      # Labels
      labels<-paste("S",c(1:v$zCircVertNSectS,sep=""))
      # n(%) of fanciers
      nfanciersbyzones<-c()
      pcfanciersbyzones<-c()
      for(i in 2:length(AngRad)){
        subfanciers<-subset(subsetfanciers, anglerad > AngRad[i-1] & anglerad <=AngRad[i])
        nfanciersbyzones<-c(nfanciersbyzones,nrow(subfanciers))
        pcfanciersbyzones<-c(pcfanciersbyzones,round(nrow(subfanciers)/nrow(fanciers)*100,digits =2))
      }
      #<-5
      Color<-'black'
      # Kms <- seq(cuttingKm,max(subsetbel$Km),by=DKm)
      # if(!Km[2] %in% Kms){
      #   Kms<-c(Kms,Km[2])
      # }
      Km<-c(min(subsetbel$Km,na.rm = TRUE),cuttingKm)
      Deltas<-c()
      for(i in 2:length(AngRad)){
        Deltas<-c(Deltas,(AngRad[i]-AngRad[i-1])/2)
      }
      AngRad<-AngRad[1:length(AngRad)-1]
      AngRad<-AngRad+Deltas
      #plot labels
      for(i in 1:length(AngRad)){
        if(v$zonesfanciers){
          Lat2Rad3 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Km[2]-(Km[2]-Km[1])/(4-0.4))
          Lon2Rad3 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Km[2]-(Km[2]-Km[1])/(4-0.4),Lat2Rad3)
          Lat2Deg3 <-Rad2Deg(Lat2Rad3)
          Lon2Deg3 <-Rad2Deg(Lon2Rad3)
          
          Lat2Rad4 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Km[2]-(Km[2]-Km[1])/(4+0.4))
          Lon2Rad4 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Km[2]-(Km[2]-Km[1])/(4+0.4),Lat2Rad4)
          Lat2Deg4 <-Rad2Deg(Lat2Rad4)
          Lon2Deg4 <-Rad2Deg(Lon2Rad4)
          
          text(Lon2Deg3,Lat2Deg3,labels= labels[i],col=Color)
          text(Lon2Deg4,Lat2Deg4,labels=paste(nfanciersbyzones[i],"(",pcfanciersbyzones[i],"%)",sep=''),col=Color)
        } else {
          Lat2Rad3 <- getLatFromAngleDistance(Lat1Rad,AngRad[i],Km[2]-(Km[2]-Km[1])/(4))
          Lon2Rad3 <- getLonFromAngleDistance(Lat1Rad,Lon1Rad,AngRad[i],Km[2]-(Km[2]-Km[1])/(4),Lat2Rad3)
          Lat2Deg3 <-Rad2Deg(Lat2Rad3)
          Lon2Deg3 <-Rad2Deg(Lon2Rad3)
          
          text(Lon2Deg3,Lat2Deg3,labels= labels[i],col=Color)
        }
      }
    }
    

    
    v<-getInputValues()
    cv<-getComputedValues()
    #par(xpd=FALSE)#avoid plot ouside of plot limit, but work in download image but not in plot rendered in Shiny
    par(xpd=FALSE,xaxt="n",yaxt="n",mar = c(0,0,0,0))#mar = c(0,0,0,0),mai = c(0,0,0,0),oma = c(0,0,0,0),omi = c(0,0,0,0),bg="red",xlim=c(cv$xmin,cv$xmax),ylim=c(cv$ymin,cv$ymax)   ,xlim=c(par("usr")[1],par("usr")[2]),ylim=c(par("usr")[3],par("usr")[4])
    # https://stat.ethz.ch/pipermail/r-help/2003-May/033971.html : to set background color for oceans, i must set the map twice and draw a rectangle inside the plot between
    map('worldHires', xlim=c(cv$xmin,cv$xmax),ylim=c(cv$ymin,cv$ymax),fill=TRUE,lforce="e",col="white",myborder=0)#mar = c(0,0,0,0)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white") #utilité ? rect bleu l'override ..
    rect(cv$xmin,  cv$ymin, cv$xmax, cv$ymax, col = "lightblue")
    map('worldHires', xlim=c(cv$xmin,cv$xmax),ylim=c(cv$ymin,cv$ymax),fill=TRUE,col="white",lforce="e",add = TRUE,myborder=0)#mar = c(0,0,0,0),lforce="e"

    if(v$beladmin != "none"){
      gadmX <-readRDS(paste("data/gadm/gadm36_BEL_",v$beladmin,"_sp_interiors.rds",sep=''))
      map(gadmX,add=TRUE,col="gray",xlim=c(cv$xmin,cv$xmax),ylim=c(cv$ymin,cv$ymax),lforce="e")#lforce : avec xlim et y lim : assure qu'on ne plotte pas en dehors des limites
    }
    if(v$zonesfanciers){
      if(v$fanciers=="anvers1553"){
        fanciers <- read.csv("data/fanciers-belgium-anvers-1553.csv", sep=",", dec=".")
      }
      if(v$fanciers=="liege1580"){
        fanciers <- read.csv("data/fanciers-belgium-liege-1580.csv", sep=",", dec=".")
      }
      if(v$fanciers=="liege535"){
        fanciers <- read.csv("data/fanciers-belgium-liege-535.csv", sep=",", dec=".")
      }
      points(fanciers,pch=20,col='red',cex=0.2)
    }
    if(v$Lon!="" & v$Lat!=""){
      mycoord<-mapproject(cv$LonDec,cv$LatDec)
      points(mycoord,pch=18,col='blue',cex=2)

      if(v$circles){
        plotDist(cv$LatDec,cv$LonDec,c(250,425,600,800),"black")
      }
      if(v$zones=="2014"){#Angles in degrees defining zones relatively to Chastres
        plotZonesRFCB(c(48.4297221876,1.5213888709),c(13.5210003004908,24.8144,29.9559,34.0166,38.9018,43.9448,66.8003024954277),c(250,450),20,"black")#Min :  13.5210003004908  |  Max :  66.8003024954277
      }
      if(v$zones=="2015"){#Angles in degrees defining zones relatively to Limoges Min :  8.56848816243249  |  Max :  36.117411637631
        a0 <- 8.56848816243249
        a1 <- angleDeg(1.2052777778,45.5191666667,3.2810555556,51.3508888889)
        a2 <- angleDeg(1.2052777778,45.5191666667,3.9889444444,51.2348055556)
        a3 <- angleDeg(1.2052777778,45.5191666667,4.9106944444,51.4081111111)
        a4 <- angleDeg(1.2052777778,45.5191666667,5.6214722222,51.20725)
        a5 <- angleDeg(1.2052777778,45.5191666667,6.0445555556,50.7285277778)
        a6 <- 36.117411637631
        plotZonesRFCB(c(45.5191666667,1.2052777778),c(a0,a1,a2,a3,a4,a5,a6),c(535,725),20,"black")
      }
      if(v$zones=='2020'){
        plotZonesRFCB2020()
      }
      if(v$training){
        plotDist(cv$LatDec,cv$LonDec,v$trainingdistance,"#666666")
      }
    }
    if(v$PLon!="" & v$PLat!=""){
      pcoord<-mapproject(Sexa2Dec(as.numeric(v$PLon)),Sexa2Dec(as.numeric(v$PLat)))
      points(pcoord,pch=20,col='lightblue',cex=1)
      label<-paste("")
      if(v$labels){label<-paste(label,as.character(v$PRLName),collapse = NULL,sep=' ')}
      if(v$kms){label<-paste(label,Dec2Km(Sexa2Dec(as.numeric(v$PLon)),Sexa2Dec(as.numeric(v$PLat)),Sexa2Dec(as.numeric(v$Lon)),Sexa2Dec(as.numeric(v$Lat)),getDistUnitFact()),collapse = NULL,sep=' ')}
      if(v$flightlines){plotFlightLine(mycoord,pcoord,10,"blue")}
      if(v$locsim){plotPigeonsLocationSimulation(mycoord,pcoord,10,"blue")}
      text(pcoord,label,cex=1,pos=4)
    }
    if(nrow(cv$coords)>0){
      show<-as.integer(row.names(cv$coords))
      for(i in 1:nrow(cv$coords)){
        coords<-mapproject(cv$coords$Lon[i],cv$coords$Lat[i])
        points(coords,pch=20,col='blue',cex=1)
        labels<-paste("")
        if(v$labels){labels<-paste(labels,as.character(cv$coords$Ville[i]),collapse = NULL,sep=' ')}
        if(v$kms){labels<-paste(labels,cv$coords$Km[i],collapse = NULL,sep=' ')}
        if(v$flightlines){plotFlightLine(mycoord,coords,10,"blue")}#coords in degrees (lat,lon) cv$LatDec,cv$LonDec
        if(v$locsim){plotPigeonsLocationSimulation(mycoord,coords,10,"blue")}#coords in degrees (lat,lon) cv$LatDec,cv$LonDec
        text(coords,labels,cex=1,pos=4)
        if(v$zones =="dynvertabs" || v$zones == "dynvertrel" || v$zones == "dynvertcircrel" || v$zones == "dynvertcircrel2" ){
          plotZonesDyn(coords,20,"black")#"#666666"
        }
        if(v$zones == 'dyncircvertrel'){
          plotZonesDynCircVertRel(coords)
        }

      }
      if(v$training){
        trainingangle<-getTrainingAngle(mycoord,cv$coords)
        maxdist<-max(cv$coords$Km)
        plotTrainingFlightLine(c(cv$LatDec,cv$LonDec),trainingangle,c(0,maxdist),10,"#666666")
      }
    }
    if(v$maintowns){
      maintownscoords<-mapproject(maintowns$Lon,maintowns$Lat)
      points(maintownscoords,pch=20,col='green',cex=2)
      
      #Labels of main towns
      maintownslabels<-paste("",sep="")
      if(v$labels){maintownslabels<-paste(maintownslabels,as.character(cv$maintowns$Ville),collapse = NULL,sep=' ')}
      if(v$kms){maintownslabels<-paste(maintownslabels,as.character(cv$maintowns$Km),collapse = NULL,sep=' ')}
      text(maintownscoords,maintownslabels,cex=1,pos=4)
    }
    
    #Add licence
    rasterImage(cc,cv$xmin,cv$ymin,cv$xmin+((cv$xmax-cv$xmin)*0.1),cv$ymin+((cv$ymax-cv$ymin)*0.03))#cv$xmin+2.5,cv$ymin+0.35
    text(cv$xmin+((cv$xmax-cv$xmin)*0.1),(cv$ymin+((cv$ymax-cv$ymin)*0.03)*0.45),paste("CC-BY Grégoire Vincke",sep=""),pos=4,cex=1)
    if(v$distunit=='km') {
      map.scale(x=cv$xmin+((cv$xmax-cv$xmin)*0.005), y=(cv$ymin+((cv$ymax-cv$ymin)*0.07)),metric=TRUE)
    }
    if(v$distunit=='mi') {
      map.scale(x=cv$xmin+((cv$xmax-cv$xmin)*0.005), y=(cv$ymin+((cv$ymax-cv$ymin)*0.07)),metric=FALSE)
    }
  })#,height =600,width=800
  
  output$map <- renderPlot(
    print(plotMap())
  )

  output$Datas <- DT::renderDataTable({
    v<-getInputValues()
    cv<-getComputedValues()
    data<-cv$datatoshow
    datatable(data,options=list(columnDefs = list(list(visible=FALSE, targets=c(0)))))#datatable is from DT package and options = to hide #id of the row displayed
  })

#UI
output$uiPanelTitle <- renderUI({
  tr("Title")
})

output$uiSBlanguage<- renderUI({
  strong(HTML(paste(tr("Language"),":",sep=" ")))
})

output$uiSBtop <- renderUI({
  fluidRow(column(12,"",#Use fluidRow and column 12 to have environment where severals ui stuffs can be defined instead od use uiOutput for each of them
  h4(HTML(tr("ReleaseLocations")))
  ))
})

output$uiSBTowns <- renderUI({
  strong(HTML(paste(tr("Towns"),":",sep=" ")))
})

output$uiSBSelection <- renderUI({
  strong(HTML(paste(tr("Classification"),":",sep=" ")))
})

output$uiSBlocationsbottom <- renderUI({
  fluidRow(column(12,NULL,#Use fluidRow and column 12 to have environment where severals ui stuffs can be defined instead od use uiOutput for each of them
    HTML('<hr style="border:1px dashed #ccc;"/>'),
    h4(HTML(tr("ParticularReleaseLocation"))),
    textInput("PRLName", paste(tr("Name"),":"),"" ),
    HTML(tr("Coords")),
    tags$table(tags$tr(tags$td(textInput("PLat", tr("NorthN"),"" )),tags$td(HTML("&nbsp;")),tags$td(textInput("PLon", tr("EastE"),"" )))),
    HTML('<hr style="border:1px dashed #ccc;"/>'),
    checkboxInput("labels", label = HTML(tr("ShowNames")), value = TRUE),
    checkboxInput("flightlines", label = HTML(tr("ShowFlight")), value = FALSE),
    HTML('<hr style="border:1px solid #ccc;"/>'),
    selectInput(inputId="mapzones",label=strong(HTML(paste(tr("MappedZone")," :",sep=""))),choices=getMappedzones(),selected="befres",selectize=FALSE),
    conditionalPanel(condition = "input.mapzones == 'man'",
       HTML(tr("Coords")),
        tags$table(
          tags$tr(tags$td(),tags$td(strong("Min")),tags$td(HTML("&nbsp;")),tags$td(strong("Max"))),
          tags$tr(tags$td(strong(tr("NorthN"))),tags$td(numericInput("MLatMin","","421000",min = -900000, max = 900000, step=1000)),tags$td(HTML("&nbsp;")),tags$td(numericInput("MLatMax","","514000",min = -900000, max = 900000, step=1000))),
          tags$tr(tags$td(strong(tr("EastE"))),tags$td(numericInput("MLonMin","","-49000",min = -1800000, max = 1800000, step=1000)),tags$td(HTML("&nbsp;")),tags$td(numericInput("MLonMax", "","82000",min = -1800000, max = 1800000, step=1000)))
        )
       ),
  HTML('<hr style="border:1px solid #ccc;"/>')
  ))
})

output$uiSBdistances <- renderUI({
  fluidRow(column(12,"",#Use fluidRow and column 12 to have environment where severals ui stuffs can be defined instead od use uiOutput for each of them
  h4(HTML(tr("CalculDist"))),
  HTML(paste("<p id='note'>",tr("Caution"),"</p>",sep="")),#Soit on met un ensemble prévu pour contenir plusieurs output comme sidebarPanel() ou mainPanel() et tout peut être dedant, soit c'est une portion dans un ensemble comme ça et tout doit être séparé.
  HTML(tr("RefCoords")),
  tags$table(tags$tr(tags$td(textInput("Lat", tr("NorthN"),"503828.0" )),tags$td(HTML("&nbsp;")),tags$td(textInput("Lon", tr("EastE"),"044005.0" )))),
  HTML(paste("<span id='note'>",tr("RefCoordsNote"),"</span>",sep="")),
  checkboxInput("kms", label = tr("ShowDist"), value = FALSE),
  checkboxInput("circles", label = tr("ShowCircles"), value = FALSE)
  ))
})

output$uiSBdistancesb <- renderUI({
  fluidRow(column(12,"",#Use fluidRow and column 12 to have environment where severals ui stuffs can be defined instead od use uiOutput for each of them
    selectInput("racedist", strong(tr("SortDist")),choices=getRacedistances(),selectize=FALSE,selected="all")
  ))
})
output$uiSBunit <- renderUI({
  strong(HTML(tr("DistUnit")))
})
output$uiSBround <- renderUI({
  numericInput("round", tr("Decimales"), 0,min = 0, max = 3, step=1)
})

output$uiSBtraining <- renderUI({
  fluidRow(column(12,"",#Use fluidRow and column 12 to have environment where severals ui stuffs can be defined instead od use uiOutput for each of them
    HTML('<hr style="border:1px solid #ccc;"/>'),
    h4(HTML(tr("PigeonTraining"))),
    checkboxInput("training", label = tr("ComputeTrainingLocation"), value = FALSE),
    conditionalPanel(condition = "input.training",
      sliderInput("trainingdistance", label = strong(tr("TrainingDistance")), min = 10, max = 150, value = 40, step=5),
      selectInput(inputId="trainingmethods",label=strong(HTML(paste(tr("TrainingComputationMethods")," :",sep=""))),choices=getTrainingComputationMethods(),selected="mean",selectize=FALSE)
    )
  ))
})
output$uiSBtrainingCoords <- renderText({
  v<-getInputValues()
  cv<-getComputedValues()
  LatTrainingDeg<-""
  LonTrainingDeg<-""
  if(v$Lon!="" & v$Lat!=""){
    mycoord<-mapproject(cv$LonDec,cv$LatDec)
    if(v$training & nrow(cv$coords)>0){
      trainingangle<-getTrainingAngle(mycoord,cv$coords)
      LatTrainingDeg<-round(getTrainingLatDeg(c(cv$LatDec,cv$LonDec),trainingangle,v$trainingdistance),6)
      LonTrainingDeg<-round(getTrainingLonDeg(c(cv$LatDec,cv$LonDec),trainingangle,v$trainingdistance),6)
      HTML(paste(tr('TrainingCoordinates')," : ","<a href='","https://www.google.com/maps?q=",LatTrainingDeg,",",LonTrainingDeg,"' target='_blank'>",Dec2Sexa(LatTrainingDeg),",",Dec2Sexa(LonTrainingDeg),"</a>",sep=""))
    } else {
      HTML(paste(""))
    }
  } else {
    HTML(paste(""))
  }
})

output$uiSBsimul <- renderUI({
  fluidRow(column(12,"",#Use fluidRow and column 12 to have environment where severals ui stuffs can be defined instead od use uiOutput for each of them
      HTML('<hr style="border:1px solid #ccc;"/>'),
      h4(HTML(tr("PigeonLocation"))),
      checkboxInput("locsim", label = tr("ShowPigeonLocationSimulation"), value = FALSE),
      conditionalPanel(condition = "input.locsim",
        sliderInput("speed", label = strong(tr("PigeonsSpeed")), min = 400, 
                    max = 2200, value = c(800, 1200)),
        strong(HTML(tr("RaceTime"))),
        tags$table(tags$tr(tags$td(numericInput("days", tr("Days"), 0,min = 0, max = 5, step=1)),tags$td(numericInput("hours", tr("Hours"), 0,min = 0, max = 23, step=1)),tags$td(numericInput("minutes", tr("Minutes"), 0,min = 0, max = 59, step=1))))  
      )
  ))
})

output$uiSBdloutput <- renderUI({
  fluidRow(column(12,"",#Use fluidRow and column 12 to have environment where severals ui stuffs can be defined instead od use uiOutput for each of them
      HTML('<hr style="border:1px solid #ccc;"/>'),
      h4(HTML(tr("DownLoadMap"))),
      radioButtons("dlFileType",paste(tr('FileType'),':',sep=" "),choices =c('png','svg','eps')),
      conditionalPanel(condition = "input.dlFileType == 'png'",
                       sliderInput("dlFileSizePx", paste(tr('DimImgPx'),':',sep=" "), 
                                   min=600, max=4000, value=800, step=100)),
      conditionalPanel(condition = "input.dlFileType == 'svg' | input.dlFileType == 'eps'",
                       sliderInput("dlFileSizeIn", paste(tr('DimImgIn'),':',sep=" "), 
                                   min=6, max=40, value=8, step=1)),
      downloadButton('dlMap', tr("DownLoadMap")),
      h4(HTML(tr("DownLoadData"))),
      downloadButton('dlData', tr("DownLoadDataCSV"))
   ))
})

output$dlMap <-downloadHandler(
  filename = function(){paste('map',input$dlFileType,sep='.')},
  content = function(file){
    if(input$dlFileType=="png"){png(file, width = input$dlFileSizePx, height = input$dlFileSizePx)}
    if(input$dlFileType=="svg"){svg(file, width = input$dlFileSizeIn, height = input$dlFileSizeIn)}
    if(input$dlFileType=="eps"){
      setEPS()
      postscript(file, width = input$dlFileSizeIn, height = input$dlFileSizeIn)
      }
    plotMap()
    dev.off()
  }
)

output$dlData <- downloadHandler(
  filename = "beprmap.csv",
  content = function(file) {
    v<-getInputValues()
    cv<-getComputedValues()
    write.csv(cv$datatoshow, file)
  }
)

output$uiSBshow <- renderUI({
  fluidRow(column(12,"",#Use fluidRow and column 12 to have environment where severals ui stuffs can be defined instead od use uiOutput for each of them
        selectInput(inputId="zones",label=strong(HTML(paste(tr("Zones"),":",sep=" "))),choices=getRacingzones(),selected="none",selectize=FALSE),
        conditionalPanel(condition = "input.zones == 'dynvertcircrel'",
          numericInput("zonecirccut","Limite","550",min = 250, max = 1250, step=5, width = 5)
        ),
        conditionalPanel(condition = "input.zones == '2020'",
           HTML(tr("Coords")),
           tags$table(
             tags$tr(tags$td(strong(tr("NorthSouthLimit"))),tags$td(numericInput("z2020Lat","","504600",min = 493000, max = 900000, step=100)),tags$td(HTML("&nbsp;")),tags$td(HTML("&nbsp;")),tags$td(HTML("&nbsp;"))),
             tags$tr(tags$td(strong(tr("SectorsAngle"))),tags$td(numericInput("z2020Angle","","25",min = -180, max = 180, step=1)),tags$td(HTML("&nbsp;")),tags$td(HTML("&nbsp;")),tags$td(HTML("&nbsp;"))),
             tags$tr(tags$td(strong(tr("NorthSectorsLimits"))),tags$td(numericInput("z2020NLon1","","31300",min = 23200, max = 62400, step=100)),tags$td(HTML("&nbsp;")),tags$td(numericInput("z2020NLon2","","40400",min = 23200, max = 62400, step=100)),tags$td(numericInput("z2020NLon3","","45200",min = 23200, max = 62400, step=100))),
             tags$tr(tags$td(strong(tr("SouthSectorsLimits"))),tags$td(numericInput("z2020SLon1","","42300",min = 23200, max = 62400, step=100)),tags$td(HTML("&nbsp;")),tags$td(numericInput("z2020SLon2", "","51700",min = 23200, max = 62400, step=100)),tags$td(HTML("&nbsp;")))
           )
        ),
        conditionalPanel(condition = "input.zones == 'dyncircvertrel'",
            HTML(tr("NumberOfSectors")),
            tags$table(
              tags$tr(tags$td(strong(tr("North"))),tags$td(numericInput("zCircVertNSectN","","4",min = 1, max = 10, step=1)),tags$td(strong(tr("South"))),tags$td(numericInput("zCircVertNSectS","","3",min = 1, max = 10, step=1)))
              )
        ),
        conditionalPanel(condition = "input.zones == 'dynvertabs' || input.zones == 'dynvertrel' || input.zones == '2014' || input.zones == '2015'",
          checkboxInput("zonehidemiddle", label = tr("HideMiddleDistanceSubdivisions"), value = FALSE)
        ),
        conditionalPanel(condition = "input.zones != 'none'",
          selectInput(inputId="fanciers",label=strong(HTML(paste(tr("FanciersDataset"),":",sep=" "))),choices=getMappedfanciers(),selected="anvers1553",selectize=FALSE),
          checkboxInput("zonesfanciers", label = tr("DisplayFanciers"), value = FALSE)
          ),
    selectInput(inputId="beladmin",label=strong(HTML(paste(tr("BelAdminZones"),":",sep=" "))),choices=getBelAdmin(),selected="none",selectize=FALSE),
    HTML('<hr style="border:1px solid #ccc;"/>'),
    h4(tr("Display")),
    checkboxInput("maintowns", label = tr("MainTowns"), value = FALSE),
    #checkboxInput("perso", label = "Lieux perso (en développement)", value = FALSE),
    HTML('<hr style="border:1px solid #ccc;"/>')   
  ))
})

output$uiSBlicence <- renderUI({
  HTML(paste("<a rel='license' href='http://creativecommons.org/licenses/by/2.0/be/'><img alt='Licence Creative Commons' style='border-width:0' src='img/cc_by_80x15.png' /></a>&nbsp;",tr("LicenceSeeCredits"),sep=""))
})

output$uiMain <- renderUI({
  tabsetPanel(id="Tabset",selected=1,
              tabPanel(
                tr("Map"),
                plotOutput("map",height=700),#,height = "auto"
                uiOutput('uiSBtrainingCoords'),# must be call from server.R to allow getComputeValues() to be called without errors
                value=1
              ),
              tabPanel(
                tr("Datas"),
                DT::dataTableOutput('Datas'),
                value=2
              ),
              tabPanel(
                tr("Help"),
                h4(paste(tr("CalculDist"),":",sep=" ")),
                p(HTML(tr("HelpSent1"))),
                p(HTML(tr("HelpSent2"))),
                p(HTML(tr("HelpSent3"))),
                p(HTML(tr("HelpSent4"))),
                h4(paste(tr("TechnicalInformations"),":",sep=" ")),
                p(HTML(tr("SessionCounter")," :",SRV$count)),
                value=3
              )
  )
})


output$uiCreditsTitle <- renderUI({
  tr("Credits")
})

output$uiCredits1 <- renderUI({
  mainPanel(
    p(HTML(paste("<strong>",tr("Author"),":</strong> Grégoire Vincke - <a href='http://www.gregoirevincke.be' target='_blank'>http://www.gregoirevincke.be</a> - ",tr("March")," 2015",sep=""))),
    p(HTML(paste("<strong>",tr("Translations")," :</strong> ",tr("TranslationsHowTo"),sep=""))),
    p(HTML(paste("<strong>Licences :</strong> <ul><li><strong>Animation :</strong> <a rel='license' href='http://creativecommons.org/licenses/by/2.0/be/'><img alt='Licence Creative Commons' style='border-width:0' src='img/cc_by_80x15.png' /></a>",tr("CreditsLicence"),"</li><li><strong>Code :</strong> ",tr("SourceCodeLocation"),"</li></ul>",sep=""))),
    p(HTML(paste("<strong>",tr("Softwares")," :</strong> ",tr("SoftwaresIUsed")," :"))),
    HTML("<ul>"),
    HTML('<li><strong>R</strong> : R Core Team (2013). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL <a href="http://www.R-project.org/" target=_blank">http://www.R-project.org/</a>.</li>'),
    HTML('<li><strong>RStudio</strong> : RStudio Team (2012). RStudio: Integrated Development for R. RStudio, Inc., Boston, MA URL <a href="http://www.rstudio.com/" target=_blank">http://www.rstudio.com/</a>.</li>'),
    HTML('<li><strong>shiny</strong> : RStudio and Inc. (2014). shiny: Web Application Framework for R. R package version 0.10.2.1. <a href="http://CRAN.R-project.org/package=shiny" target=_blank">http://CRAN.R-project.org/package=shiny</a> and <a href="http://shiny.rstudio.com" target=_blank">http://shiny.rstudio.com</a>.</li>'),
    HTML('<li><strong>maps</strong> : Original S code by Richard A. Becker and Allan R. Wilks. R version by Ray Brownrigg. Enhancements by Thomas P Minka <tpminka@media.mit.edu> (2014). maps: Draw Geographical Maps. R   package version 2.3-9. <a href="http://CRAN.R-project.org/package=maps" target=_blank">http://CRAN.R-project.org/package=maps</a>.</li>'),
    HTML('<li><strong>mapdata</strong> : Original S code by Richard A. Becker and Allan R. Wilks. R version by Ray Brownrigg. (2014). mapdata: Extra Map Databases. R package version 2.2-3. <a href="http://CRAN.R-project.org/package=mapdata" target=_blank">http://CRAN.R-project.org/package=mapdata</a>.</li>'),
    HTML('<li><strong>mapproj</strong> : Doug McIlroy. Packaged for R by Ray Brownrigg, Thomas P Minka and transition to Plan 9 codebase by Roger Bivand. (2014). mapproj: Map Projections. R package version 1.2-2. <a href="http://CRAN.R-project.org/package=mapproj" target=_blank">http://CRAN.R-project.org/package=mapproj</a>.</li>'),
    HTML('<li><strong>png</strong> : Simon Urbanek (2013). png: Read and write PNG images. R package version 0.1-7. <a href="http://CRAN.R-project.org/package=png" target=_blank">http://CRAN.R-project.org/package=png</a>.</li>'),
    HTML("</ul>")
  )
})

output$uiCredits2 <- renderUI({
  mainPanel(
    p(HTML(paste("<strong>",tr("Sources")," :</strong> ",tr("SourcesiUsed")," :",sep=""))),
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
    HTML("</ul>")
  )
})
})
