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

library(shiny)
library(maps)
library(mapproj)
library(mapdata)
library(png)

cc <- readPNG("www/img/cc_by_320x60.png")

shinyServer(function(input, output, session) {
  
  data <- read.csv("data/coordonnees_rfcb.csv", sep=";", dec=",", quote="")
  
  observe({#http://stackoverflow.com/questions/28119964/dynamic-input-selector-based-on-uploaded-data
    v<-sort(as.vector(data$Villes)) 
    v<-c(" "="empty",v)
    updateSelectInput(#http://www.inside-r.org/packages/cran/shiny/docs/updateSelectInput
      session,
      "towns",
      choices=v)
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
  
  langracedist <- read.delim("data/lang-racedist.csv", header = TRUE, sep = "\t", as.is = TRUE) 
  row.names(langracedist)<-langracedist$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
  getRacedistances<-reactive({
    l<-list()
    for(i in 1:nrow(langracedist)){
      l[[langracedist[[input$language]][i]]]<-langracedist$key[i]
    }
    return(l)
  })
  
  getInputValues<-reactive({
    return(input)#collect all inputs
  })
  
  getComputedValues<-reactive({
    v<-getInputValues() # get all values of input list
    cv<-list()#created empty computed values list
    
    cv$round<-as.integer(v$round)
    
    if(v$Lat!="" & v$Lon!=""){
      cv$Lat<-as.numeric(v$Lat)
      cv$Lon<-as.numeric(v$Lon)
      
      #From Sexagésimal to decimal coordinates
      LatFact<-1
      if(cv$Lat<0){
        LatFact<-LatFact*-1
      }
      cv$LatDeg<-floor(cv$Lat/10000)*LatFact
      cv$LatMin<-((floor(cv$Lat/100)/100-floor(floor(cv$Lat/100)/100))*100)*LatFact
      cv$LatSec<-((cv$Lat/100-floor(cv$Lat/100))*100)*LatFact 
      cv$LatDec<-cv$LatDeg+(cv$LatMin/60)+(cv$LatSec/3600)
      cv$LatRad<-cv$LatDec*pi/180
      
      LonFact<-1
      if(v$Lon<0){
        LonFact<-LonFact*-1
      }
      cv$LonDeg<-floor(cv$Lon/10000)*LonFact 
      cv$LonMin<-((floor(cv$Lon/100)/100-floor(floor(cv$Lon/100)/100))*100)*LonFact 
      cv$LonSec<-((cv$Lon/100-floor(cv$Lon/100))*100)*LonFact
      cv$LonDec<-cv$LonDeg+(cv$LonMin/60)+(cv$LonSec/3600)
      cv$LonRad<-cv$LonDec*pi/180
      
      #Calculation of distances in Km
      cons1<-0.99664718933525
      cons2<-6378137
      
      data$LatRad<-as.numeric(data$Lat)*pi/180
      data$LonRad<-as.numeric(data$Lon)*pi/180
      data$BHO<-(cv$LatRad+data$LatRad)/2
      data$IHO<-data$LonRad-cv$LonRad
      data$NU2<-0.0067394967422767*cos(data$BHO)^2
      data$VHO<-sqrt(1+data$NU2)
      data$LAHO<-data$IHO*data$VHO
      data$OM1<-atan(cons1*tan(cv$LatRad))
      data$OM2<-atan(cons1*tan(data$LatRad))
      data$XHO<-sin(data$OM1)*sin(data$OM2)+cos(data$OM1)*cos(data$OM2)*cos(data$LAHO)
      data$AFSTG=(cons2/data$VHO)*(atan(-data$XHO/sqrt(1-data$XHO^2))+2*atan(1))
      data$M=round(data$AFSTG,0)
      data$Km=round(data$M/1000,cv$round)
      
      maintowns$LatRad<-as.numeric(maintowns$Lat)*pi/180
      maintowns$LonRad<-as.numeric(maintowns$Lon)*pi/180
      maintowns$BHO<-(cv$LatRad+maintowns$LatRad)/2
      maintowns$IHO<-maintowns$LonRad-cv$LonRad
      maintowns$NU2<-0.0067394967422767*cos(maintowns$BHO)^2
      maintowns$VHO<-sqrt(1+maintowns$NU2)
      maintowns$LAHO<-maintowns$IHO*maintowns$VHO
      maintowns$OM1<-atan(cons1*tan(cv$LatRad))
      maintowns$OM2<-atan(cons1*tan(maintowns$LatRad))
      maintowns$XHO<-sin(maintowns$OM1)*sin(maintowns$OM2)+cos(maintowns$OM1)*cos(maintowns$OM2)*cos(maintowns$LAHO)
      maintowns$AFSTG=(cons2/maintowns$VHO)*(atan(-maintowns$XHO/sqrt(1-maintowns$XHO^2))+2*atan(1))
      maintowns$M=round(maintowns$AFSTG,0)
      maintowns$Km=round(maintowns$M/1000,cv$round)
      cv$maintowns<-maintowns
      
#       perso$LatRad<-as.numeric(perso$Lat)*pi/180
#       perso$LonRad<-as.numeric(perso$Lon)*pi/180
#       perso$BHO<-(cv$LatRad+perso$LatRad)/2
#       perso$IHO<-perso$LonRad-cv$LonRad
#       perso$NU2<-0.0067394967422767*cos(perso$BHO)^2
#       perso$VHO<-sqrt(1+perso$NU2)
#       perso$LAHO<-perso$IHO*perso$VHO
#       perso$OM1<-atan(cons1*tan(cv$LatRad))
#       perso$OM2<-atan(cons1*tan(perso$LatRad))
#       perso$XHO<-sin(perso$OM1)*sin(perso$OM2)+cos(perso$OM1)*cos(perso$OM2)*cos(perso$LAHO)
#       perso$AFSTG=(cons2/perso$VHO)*(atan(-perso$XHO/sqrt(1-perso$XHO^2))+2*atan(1))
#       perso$M=round(perso$AFSTG,0)
#       perso$Km=round(perso$M/1000,cv$round)
#       cv$perso<-perso
    }
    
    #Data selection : must be after distance computation because some filters are based on distance
    coords<-data
    #coords<-c()
    if(v$selection=="unselected"){coords<-subset(coords,Id %in% c())}
    if(v$selection=="rfcblinew"){coords<-subset(coords,Id %in% c(112,113,114,115,116,117,118,119))}
    if(v$selection=="rfcblinec"){coords<-subset(coords,Id %in% c(120,121,122,123,34))}
    if(v$selection=="rfcblinee"){coords<-subset(coords,Id %in% c(103,50,88,68,27,31,99,47,48,58,91,44))}
    if(v$selection=="rfcbvr"){coords<-subset(coords,Id %in% c(80,81,35,85,100,111,20,93,5,9,30))}
    if(v$selection=="rfcbnatint"){coords<-subset(coords,Id %in% c(2,6,10,13,16,17,22,39,42,43,49,52,53,57,61,63,64,70,76,94,101,102))}
    if(v$selection=="rfcbnatintdf"){coords<-subset(coords,Id %in% c(13,22,39,64,6,49,42))}
    if(v$selection=="rfcbnatintf"){coords<-subset(coords,Id %in% c(53,102,17,63,61,16,43,101,52))}
    if(v$selection=="rfcbnatintgf"){coords<-subset(coords,Id %in% c(74,2,10,94,57,70,76))}
    if(v$selection=="rfcbbeloff"){coords<-subset(coords,Id %in% c(79,59,139,140,55))}
    if(v$selection=="rfcbbeltour"){coords<-subset(coords,Id %in% c(141,142,143,144,145,146,147,148,149,150,7))}
    if(v$selection=="rfcbdivers"){coords<-subset(coords,Id %in% c(151,152,153,154,28,25,32,155,156,37,157,158,159,40,160,161,162,163,56,164,165,166,167,168,169,169,170,171,172,173,174,175,176,177,95,108,178,109,179,180,181))}
    
    if(v$selection=="awch"){coords<-subset(coords,Id %in% c(79,182,183,59))}
    if(v$selection=="awcbw"){coords<-subset(coords,Id %in% c(184))}
    if(v$selection=="awcn"){coords<-subset(coords,Id %in% c(185,168,95,37))}
    if(v$selection=="awclg"){coords<-subset(coords,Id %in% c(186,187,188,189))}
    if(v$selection=="awclx"){coords<-subset(coords,Id %in% c(190,191,7,109))}
    if(v$selection=="awc"){coords<-subset(coords,Id %in% c(79,182,183,59,184,185,168,95,37,186,187,188,189,190,191,7,109))}
    if(v$selection=="itawc"){coords<-subset(coords,Id %in% c(14,46,105))}
    
    if(v$selection=="itfedesp"){coords<-subset(coords,Id %in% c(15,25,31,59,80,85,90,91,99,106,14,46,105,152,167,2,6,10,13,16,17,22,39,42,43,49,52,53,57,61,63,64,70,76,94,101,102))}
    if(v$selection=="itcentand"){coords<-subset(coords,Id %in% c(15,25,31,59,80,85,90,91,99,106))}
    if(v$selection=="itaf"){coords<-subset(coords,Id %in% c(25,59,31,25,59,31,99,85,80,91,106,13,15,22,91,106,46,90,105,42,53,102,17,39,74,64,2,63,6,10,61,49,94,16,57,43,70,101,76,52,6))}
    if(v$selection=="itafv"){coords<-subset(coords,Id %in% c(25,59,31,99,85,80))}
    if(v$selection=="itafdf"){coords<-subset(coords,Id %in% c(91,106,13,15,22,91,106,46,90,105,42))}
    if(v$selection=="itaff"){coords<-subset(coords,Id %in% c(53,102,17,39,74,64,2,63,6,10,61,49,94,16,57,43,70,101,76,52,6))}
    if(v$selection=="ituwr"){coords<-subset(coords,Id %in% c(59,88,44,98,48,105,19,13,22,39,64,6,49,26,42,53,102,17,63,61,16,43,101,52,74,2,10,94,57,70,76,192))}
    if(v$selection=="ituwrv"){coords<-subset(coords,Id %in% c(59,88,44))}
    if(v$selection=="ituwrdf"){coords<-subset(coords,Id %in% c(98,48))}
    if(v$selection=="ituwrgdf"){coords<-subset(coords,Id %in% c(105,19,13,22,39,64,6,49,26,42))}
    if(v$selection=="ituwrf"){coords<-subset(coords,Id %in% c(53,102,17,63,61,16,43,101,52,192))}
    if(v$selection=="ituwri"){coords<-subset(coords,Id %in% c(74,2,10,94,57,70,76))}
    if(v$selection=="itgcf"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58,105,13,19,46,26,192))}
    if(v$selection=="itham"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58))}
    if(v$selection=="itmef"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58))}
    if(v$selection=="ithav"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58,105,13,46))}
    if(v$selection=="itcin"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58))}
    if(v$selection=="itdin"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58,19,26,192))}
    if(v$selection=="itflo"){coords<-subset(coords,Id %in% c(95,103,50,88,47,44,58))}
    if(v$selection=="itdh"){coords<-subset(coords,Id %in% c(105,19,53,13,102,22,17,39,74,64,2,63,6,10,61,49,94,16,57,43,70,101,76,52,42))}
    
    if(v$selection=="rdr"){coords<-subset(coords,Id %in% c(102,63,57))}
    
    if(v$selection=="all"){coords<-coords}
    
    #     if(v$pigeons=="P"){coords<-subset(coords,coords$Id %in% c(79))}
    
    if(v$racedist=="V"){coords<-subset(coords,coords$Km<=250)}
    if(v$racedist=="PDF"){coords<-subset(coords,coords$Km>250 & Km<=425)}
    if(v$racedist=="DF"){coords<-subset(coords,coords$Km>425 & Km<=600)}
    if(v$racedist=="F"){coords<-subset(coords,coords$Km>600 & Km<=800)}
    if(v$racedist=="GF"){coords<-subset(coords,coords$Km>800)}
    
    if(length(v$towns)>0 ){
      if(v$selection=="unselected"){
        coords<-data
      }
      if(v$selection=="unselected" | v$selection=="all"){

      if("empty" %in% v$towns){
        coords<-subset(coords,coords$Id %in% c())#no more subset is done
      }else {
        coords<-subset(coords,coords$Villes %in% v$towns)
      }
      }
    }
    
    #cv$datas<-subset(data,data$Nat==1)#& data$Km<=800  & (data$Andenne==1 | data$Perso==1) data,data$Pays=="B"
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
      names(cv$datatoshow)[7]<-paste("Dist (m)")#change Lat to LatDD
      names(cv$datatoshow)[8]<-paste("Dist (km)")#Change Lon to LonDD
    } else {
      cv$datatoshow<-subset(coords,select=c(Id,Villes,LatWSG84,LonWSG84,Lat,Lon,Pays))
    }
    names(cv$datatoshow)[3]<-paste("Lat WSG84")#change LatWSG84 to Lat WSG84
    names(cv$datatoshow)[4]<-paste("Lon WSG84")#change LonWSG84 to Lon WSG84
    names(cv$datatoshow)[5]<-paste("Lat DD")#change Lat to LatDD
    names(cv$datatoshow)[6]<-paste("Lon DD")#Change Lon to LonDD
    
    
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
    
    #Map should be 2 times more larger than higher
    cv$mapheight<-cv$ymax-cv$ymin
    cv$mapwidthmin<-cv$mapheight*2
    mapwidht<-cv$xmax-cv$xmin
    if(mapwidht<cv$mapwidthmin){
      cv$mapwidth<-cv$mapwidthmin
      mapxmiddle<-(cv$xmin+((cv$xmax-cv$xmin)/2))
      cv$xmax<-mapxmiddle+cv$mapwidth*0.48
      cv$xmin<-mapxmiddle-cv$mapwidth*0.48
    } 

    return(cv)
  })
  
  output$map <- renderPlot({
    
    plotDist <- function(LatDec, LonDec, Km) { #inspired form http://www.movable-type.co.uk/scripts/latlong-vincenty.html and http://stackoverflow.com/questions/23071026/drawing-circle-on-r-map
      ER <- 6371 #Earth Radius in kilometers. http://en.wikipedia.org/wiki/Earth_radius Change this to 3959 and you will have your function working in miles.
      AngDeg <- seq(1,360)
      Lat1Rad <- LatDec*(pi/180)#Latitude of the center of the circle in radians#From degrees to radians rad= deg*(pi/180)
      Lon1Rad <- LonDec*(pi/180)#Longitude of the center of the circle in radians
      AngRad <- AngDeg*(pi/180)
      for(i in 1:length(Km)){
        Lat2Rad <-asin(sin(Lat1Rad)*cos(Km[i]/ER)+cos(Lat1Rad)*sin(Km[i]/ER)*cos(AngRad))  #Latitude of each point of the circle rearding to distance and to angle in radians
        Lon2Rad <- Lon1Rad+atan2(sin(AngRad)*sin(Km[i]/ER)*cos(Lat1Rad),cos(Km[i]/ER)-sin(Lat1Rad)*sin(Lat2Rad)) #Longitude of each point of the circle rearding to distance and to angle in radians
        Lat2Deg <- Lat2Rad*(180/pi)#Latitude of each point of the circle in degrees#From radians to degrees deg = rad*(180/pi)
        Lon2Deg <- Lon2Rad*(180/pi)#Longitude of each point of the circle in degrees#From radians to degrees deg = rad*(180/pi)
        polygon(c(Lon2Deg),c(Lat2Deg),lty=2)
        text(Lon2Deg[120],Lat2Deg[120],srt=60, labels = paste(Km[i],"km",sep=" "), pos=3,cex=0.8)#angle 0 is vertical in a map, not horizontal as in common geometry ! http://www.ats.ucla.edu/stat/r/faq/angled_labels.htm
        #         lines(c(LonDec,Lon2Deg[1]),c(LatDec,Lat2Deg[1]))#plot the radius of one angle
        text(Lon2Deg[240],Lat2Deg[240],srt=-60, labels = paste(Km[i],"km",sep=" "), pos=3,cex=0.8)
      }
    }

    
    angleDeg <- function(lon1,lat1,lon2,lat2) {#from http://rfcb.be/images/Nuttige_programmas/zoneberekening.xls
      lon1<-lon1*(pi/180)
      lat1<-lat1*(pi/180)
      lon2<-lon2*(pi/180)
      lat2<-lat2*(pi/180)
      return((atan((sin(lon2-lon1)*cos(lat2))/(cos(lat1)*sin(lat2)-sin(lat1)*cos(lat2)*cos(lon1-lon2))))*180/pi)
    }
    
    plotZonesRFCB <- function(Coords,AngDeg,Km,DKm,Color){
      ER <- 6371 #Earth Radius in kilometers. http://en.wikipedia.org/wiki/Earth_radius Change this to 3959 and you will have your function working in miles.
      Lat1Rad <- Coords[1]*(pi/180)#Latitude of the center of the circle in radians#From degrees to radians rad= deg*(pi/180)
      Lon1Rad <- Coords[2]*(pi/180)#Longitude of the center of the circle in radians
      AngRad <- AngDeg*(pi/180)
      Kms <- seq(Km[1],Km[2],by=DKm)
      if(!Km[2] %in% Kms){
        Kms<-c(Kms,Km[2])
      }
      for(i in 1:5){
        for(j in 1:(length(Kms)-1)){
          Lat2Rad1 <- asin(sin(Lat1Rad)*cos(Kms[j]/ER)+cos(Lat1Rad)*sin(Kms[j]/ER)*cos(AngRad[i]))
          Lon2Rad1 <- Lon1Rad+atan2(sin(AngRad[i])*sin(Kms[j]/ER)*cos(Lat1Rad),cos(Kms[j]/ER)-sin(Lat1Rad)*sin(Lat2Rad1))
          
          Lat2Rad2 <- asin(sin(Lat1Rad)*cos(Kms[j+1]/ER)+cos(Lat1Rad)*sin(Kms[j+1]/ER)*cos(AngRad[i]))
          Lon2Rad2 <- Lon1Rad+atan2(sin(AngRad[i])*sin(Kms[j+1]/ER)*cos(Lat1Rad),cos(Kms[j+1]/ER)-sin(Lat1Rad)*sin(Lat2Rad2))
          
          Lat2Deg1 <-Lat2Rad1*(180/pi)
          Lon2Deg1 <-Lon2Rad1*(180/pi)
          
          Lat2Deg2 <-Lat2Rad2*(180/pi)
          Lon2Deg2 <-Lon2Rad2*(180/pi)
          if(i %in% c(1,3,5)){
            lines(c(Lon2Deg1,Lon2Deg2),c(Lat2Deg1,Lat2Deg2),lty=2,col=Color)
          } else {
            lines(c(Lon2Deg1,Lon2Deg2),c(Lat2Deg1,Lat2Deg2),col=Color)
          }
        }
      }
      #Plot labels
      AngDeg <- c(AngDeg[1]-2.5,AngDeg[2]-(AngDeg[2]-AngDeg[1])*0.5,AngDeg[3]-(AngDeg[3]-AngDeg[2])*0.5,AngDeg[4]-(AngDeg[4]-AngDeg[3])*0.5,AngDeg[5]-(AngDeg[5]-AngDeg[4])*0.5,AngDeg[5]+2.5)#Angles in radians defining zones relatively to Chastres
      labels<-c("A1","A2","B1","B2","C1","C2")
      Lat1Rad <- Coords[1]*(pi/180)#Latitude of the center of the circle in radians#From degrees to radians rad= deg*(pi/180)
      Lon1Rad <- Coords[2]*(pi/180)#Longitude of the center of the circle in radians
      AngRad <- AngDeg*(pi/180)
      for(i in 1:6){
        Lat2Rad1 <- asin(sin(Lat1Rad)*cos(Km[1]/ER)+cos(Lat1Rad)*sin(Km[1]/ER)*cos(AngRad[i]))
        Lon2Rad1 <- Lon1Rad+atan2(sin(AngRad[i])*sin(Km[1]/ER)*cos(Lat1Rad),cos(Km[1]/ER)-sin(Lat1Rad)*sin(Lat2Rad1))
        
        Lat2Rad2 <- asin(sin(Lat1Rad)*cos(Km[2]/ER)+cos(Lat1Rad)*sin(Km[2]/ER)*cos(AngRad[i]))
        Lon2Rad2 <- Lon1Rad+atan2(sin(AngRad[i])*sin(Km[2]/ER)*cos(Lat1Rad),cos(Km[2]/ER)-sin(Lat1Rad)*sin(Lat2Rad2))
        
        Lat2Deg1 <-Lat2Rad1*(180/pi)
        Lon2Deg1 <-Lon2Rad1*(180/pi)
        
        Lat2Deg2 <-Lat2Rad2*(180/pi)
        Lon2Deg2 <-Lon2Rad2*(180/pi)
        
        text(Lon2Deg1,Lat2Deg1,labels= labels[i],col=Color)
        text(Lon2Deg2,Lat2Deg2,labels= labels[i],col=Color)
      }
    }
    
    v<-getInputValues()
    cv<-getComputedValues()
    par(xaxt="n",yaxt="n")
    map('worldHires', xlim=c(cv$xmin,cv$xmax),ylim=c(cv$ymin,cv$ymax),mar = c(0,0,0,0))
    map.axes() 
    if(v$Lon!="" & v$Lat!=""){
      mycoord<-mapproject(cv$LonDec,cv$LatDec)
      points(mycoord,pch=18,col='blue',cex=2)
      if(v$circles){
        plotDist(cv$LatDec,cv$LonDec,c(250,425,600,800))
      }
      if(v$zones2014){
        plotZonesRFCB(c(48.4297221876,1.5213888709),c(24.8144,29.9559,34.0166,38.9018,43.9448),c(250,450),20,"#999999")
      }
      if(v$zones2015){
        #Angles in degrees defining zones relatively to Limoges
        a1 <- angleDeg(1.2052777778,45.5191666667,3.2810555556,51.3508888889)
        a2 <- angleDeg(1.2052777778,45.5191666667,3.9889444444,51.2348055556)
        a3 <- angleDeg(1.2052777778,45.5191666667,4.9106944444,51.4081111111)
        a4 <- angleDeg(1.2052777778,45.5191666667,5.6214722222,51.20725)
        a5 <- angleDeg(1.2052777778,45.5191666667,6.0445555556,50.7285277778)
        plotZonesRFCB(c(45.5191666667,1.2052777778),c(a1,a2,a3,a4,a5),c(535,725),20,"black")
      }
    }
    if(nrow(cv$coords)>0){
      show<-as.integer(row.names(cv$coords))
      for(i in 1:nrow(cv$coords)){
        coords<-mapproject(cv$coords$Lon[i],cv$coords$Lat[i])
        points(coords,pch=20,col='red',cex=1)
        labels<-paste("")
        if(v$labels){labels<-paste(labels,as.character(cv$coords$Ville[i]),collapse = NULL,sep=' ')}
        if(v$kms){labels<-paste(labels,cv$coords$Km[i],collapse = NULL,sep=' ')}
        text(coords,labels,cex=1,pos=4)
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
#     if(v$perso){
#       persocoords<-mapproject(perso$Lon,perso$Lat)
#       points(persocoords,pch=20,col='blue',cex=1)
#       
#       #Labels of perso locations
#       persolabels<-paste("",sep="")
#       if(v$labels){persolabels<-paste(persolabels,as.character(cv$perso$Ville),collapse = NULL,sep=' ')}
#       if(v$kms){persolabels<-paste(persolabels,as.character(cv$perso$Km),collapse = NULL,sep=' ')}
#       text(persocoords,persolabels,cex=1,pos=4)
#     }
    #Add licence
    rasterImage(cc,cv$xmin,cv$ymin,cv$xmin+((cv$xmax-cv$xmin)*0.1),cv$ymin+((cv$ymax-cv$ymin)*0.03))#cv$xmin+2.5,cv$ymin+0.35
    text(cv$xmin+((cv$xmax-cv$xmin)*0.1),(cv$ymin+((cv$ymax-cv$ymin)*0.03)*0.45),paste("CC-BY Grégoire Vincke 2015",sep=""),pos=4,cex=1)
    map.scale(x=cv$xmin, y=(cv$ymax-((cv$ymax-cv$ymin)*0.03)*0.5))
  })#,height =600,width=800
  
  output$Datas = renderDataTable({
    v<-getInputValues()
    cv<-getComputedValues()
    cv$datatoshow
  })

#UI
output$uiTitle <- renderUI({
    titlePanel(tr("Title"))
})#TitlePanel must be set inside renderUI() and not in ui.R titlePanel(uiOutput("uiTitle")) to avoid that html tags are included in the HTML title of the page

output$uiCaution <- renderUI({
  fluidRow(column(12,"",
                  HTML(paste("<span id='note'>",tr("Caution"),"</span>",sep=""))
  ))
})

output$uiSBtop <- renderUI({
  fluidRow(column(12,"",#Use fluidRow and column 12 to have environment where severals ui stuffs can be defined instead od use uiOutput for each of them
  HTML(paste("<span id='note'>",tr("Caution"),"</span>",sep="")),#Soit on met un ensemble prévu pour contenir plusieurs output comme sidebarPanel() ou mainPanel() et tout peut être dedant, soit c'est une portion dans un ensemble comme ça et tout doit être séparé.
  HTML('<hr style="border:1px solid #ccc;"/>'),
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
  selectInput(inputId="mapzones",label=strong(HTML(paste(tr("MappedZone")," :",sep=""))),choices=getMappedzones(),selected="befres",selectize=FALSE),
  #     h5(HTML("Type de pigeons concernés")),
  #     selectInput("pigeons", "",
  #                 list("Tous" = "all",
  #                      "Pigeonneaux" = "P", 
  #                      "Yearlings" = "Y",
  #                      "Vieux" = "V",
  #                      "Vieux & Yearlings"="VY"
  #                 )),
  
  checkboxInput("labels", label = HTML(tr("ShowNames")), value = TRUE),
  HTML('<hr style="border:1px solid #ccc;"/>')
  ))
})

output$uiSBdistances <- renderUI({
  fluidRow(column(12,"",#Use fluidRow and column 12 to have environment where severals ui stuffs can be defined instead od use uiOutput for each of them
  h4(HTML(tr("CalculDist"))),
  HTML(tr("RefCoords")),
  HTML(paste("<span id='note'>",tr("RefCoordsNote"),"</span>",sep="")),
  tags$table(tags$tr(tags$td(textInput("Lat", "Lat","503828.0" )),tags$td(HTML("&nbsp;")),tags$td(textInput("Lon", "Lon","044005.0" )))),
  checkboxInput("kms", label = tr("ShowDist"), value = FALSE),
  radioButtons("round", tr("RoundTo"),
               list("km"="0",
                    "hm"="1", 
                    "dm"="2",
                    "m"="3"
               ),selected="0"),
  selectInput("racedist", strong(tr("SortDist")),choices=getRacedistances(),selectize=FALSE,selected="all"),
  checkboxInput("circles", label = tr("ShowCircles"), value = FALSE),
  
  HTML('<hr style="border:1px solid #ccc;"/>')
  ))
})

output$uiSBshow <- renderUI({
  fluidRow(column(12,"",#Use fluidRow and column 12 to have environment where severals ui stuffs can be defined instead od use uiOutput for each of them
                  h4(tr("Display")),
                  checkboxInput("zones2015", label = tr("Zones2015"), value = FALSE),
                  checkboxInput("zones2014", label = tr("Zones2014"), value = FALSE),
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
                value=1
              ),
              tabPanel(
                tr("Datas"),
                dataTableOutput('Datas'),
                value=2
              ),
              tabPanel(
                tr("Help"),
                h4(HTML("Calcul des distances :")),
                p(HTML("Les coordonnées qui sont chargées par défaut comme ''coordonnées de référence'' sont celle du <a href='http://www.walhain.be/Loisirs/office-du-tourisme/les-curiosites-touristiques/le-centre-geographique-de-la-belgique' target=_blank'>centre géographique de la Belgique</a>. ")),
                p(HTML("Remplacez les par celles de votre propre colombier et vous bénéficierez d'un calcul de distance adapté à votre localisation personnelle. ")),
                p(HTML("Le format à utiliser dans les champs ''Lat'' (latitude) et ''Lon'' (longitude) est le format <a href='https://fr.wikipedia.org/wiki/WGS_84' target='_blank'>WGS84</a>, avec un <strong>point comme symbole décimal</strong>, et non une virgule.")),
                p(HTML("<strong>Rappel :</strong> Vos coordonnées figurent au format WGS84 sur votre carte de membre de la Royale Fédération Colombophile Belge.")),
                
                value=3
              ),
              tabPanel(
                tr("Credits"),
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
})
  
})
