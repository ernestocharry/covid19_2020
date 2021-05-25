rm(list = ls())
# Graficar crecimiento de coronaVirus
# Félix Ernesto Charry Pastrana

#these libraries need to be loaded
library(utils)
library(httr)
library(RColorBrewer)

# Mac FECHP
directory="/Users/F.E.CharryPastrana/Documents/07_Programación-GitHub/Codes"
directory=paste0(directory,'/COVID19_2020')
setwd(directory)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
    authenticate(":", ":", type="ntlm"), 
    write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv(tf)

# Funciones--------------------------------------------------------------------
funcionGraficarPuntos <- function(paises, paises_sobresalir_en_grafica){
  # Esta función grafica los casos de COVID-19 de los países de interés que
  # se encuentra en la lista PAISES. 
  # Previamente se debe llamar una gráfica PLOT. 
  
  for(ii in seq(1,length(paises))){
    pais = paises[ii]
    print(paises[ii])
    
    j = 1
    dias    <- vector(mode="numeric", length=0)
    casos   <- vector(mode="numeric", length=0)
    casosRel <- vector(mode="numeric", length=0)
    muertos <- vector(mode="numeric", length=0)
    
    # Extrayendo informacion
    for(i in seq(nrow(data),1)){
      if( (as.character(data$countriesAndTerritories[i]) == pais) ){
        dias[j]      = j
        casos[j]     = as.numeric(data$cases[i])
        casosRel[j]     = as.numeric(data$cases[i])*1000000/as.numeric(data$popData2018[i])
        muertos[j]   = as.numeric(data$deaths[i])
        j = j+1
      }
    }
    
    # Eliminando los primeros datos en cero 
    j = 1
    for(i in seq(1,length(dias))){
      if(casos[i] >= 10){
        #print(casos[i])
        j = i -1 
        break
      }
    }
    
    #print(j)
    if(j>0){
      dias    = dias[-seq(1,j)]
      casos   = casos[-seq(1,j)]
      casosRel= casosRel[-seq(1,j)]
      muertos = muertos[-seq(1,j)]
      dias    = dias - j
    }
    
    casos[casos==0] <- NA
    casosRel[casosRel==0] <- NA
    muertos[muertos==0] <- NA
    
    #print(casosRel)
    lines(dias, casosRel, col=colors[ii], lwd = 1, lty = ltys)
    
    if(missing(paises_sobresalir_en_grafica)){
      cexT = 0.6
    } else{
      for(jj in seq(1,length(paises_sobresalir_en_grafica))){
        #print(jj)
        if(pais == paises_sobresalir_en_grafica[jj]){
          cexT = 1.5
          print(pais)
          print(paises_sobresalir_en_grafica[jj])
        } else {
          cexT = 0.6
        }
      }
    }
    
    points(dias, casosRel, col=colors[ii], pch=pchs, cex = cexT)
    
  }
}

# Variables común a todas las gráficas -----------------------------------------
{
fecha = Sys.Date()
fechaNumeric = fecha
xLabel = "Días después de 10 casos reportados"
yLabel = 'No. de casos de Coronavirus por cada millón de habitantes '
yLabel = paste0(yLabel, 'reportados al ', fecha)

# Paises de interés: Max 8 por los colores
paises1 = c('China', 'Spain', 'Italy', 
            'United_States_of_America', 'Colombia',
            'Mexico')
#paises1 = c('Togo')
# United_States_of_America

colors = brewer.pal(n = length(paises1) , name = 'Dark2')

MarginLeft   = 2        # cm
MarginRight  = 0.3     # cm
MarginTop    = 0.3         # cm 
MarginBottom = 2    # cm 

widthPDFcm = 11 # cm 1127
heightPDFcm = 20 # cm 2008

cexT = 0.6                # Point size 
ldwT = 1.7                # Line width
pchs = 19
ltys = 1
PosicionLabels = 2
sizeP = 1
}
#### 1- China solo ####
{
  Graph1Name = paste0(directory,"/", fechaNumeric, "_1_Resultados_China.png")
  xLimit = c(0,90)    # Mismo limite para todos
  yLimit = c(0.01,200)  # Anos vs. profundidad
  yy = c('0.1', '10', '100',  '200')
  yNames = c('0.1', '10', '100', '200')
  
  # Labeling 
  PositionX1 = 53 
  PositionY1 = 2 
  Label1     = 'China'
  
  PositionX2 = 10 
  PositionY2 = 8 
  Label2     = 'Italy - Spain'
  
  PositionX3 = 15 
  PositionY3 = 1.5
  Label3     = 'Col - Mx'
  
  PositionX4 = 15 
  PositionY4 = 2
  Label4     = ' '
  
  paises = paises1
  png(Graph1Name, height = heightPDFcm, width = widthPDFcm, units = "cm", res = 300)
  par(mai=c(MarginBottom,MarginLeft,MarginTop,MarginRight)/cm(1))
  
  plot(NA, NA, 
       type = 'n',
       xaxt='n', yaxt='n', ann=FALSE, 
       xlim = xLimit, ylim = yLimit,
       xaxs = "i", yaxs = 'i', 
       lwd = 5)
  
  paises_sobresalir_en_grafica = {}
  
  funcionGraficarPuntos(paises)
  
  axis(1, las = 1, cex.axis=sizeP, line = 0)
  axis(2, las = 1, cex.axis=sizeP, line = 0, at=yy, labels = yNames)
  mtext(xLabel, side=1, line=PosicionLabels, cex=sizeP)
  mtext(yLabel, side=2, line=PosicionLabels+0.5, cex=sizeP)
  
  #text(PositionX1, PositionY1, Label1)
  #text(PositionX2, PositionY2, Label2)
  #text(PositionX3, PositionY3, Label3)
  #text(PositionX4, PositionY4, Label4)
  
  for(i in seq(1,length(paises))){
    if(paises[i] == 'United_States_of_America'){
      paises[i] = 'EE. UU.'
    }
  }
  
  legend("topleft",bg="white", legend = paises, col = colors, pch = pchs, cex=sizeP, horiz = F)
  
  dev.off(); graphics.off()
}

#### 2 - Grafica normal ####
{
  Graph1Name = paste0(directory,"/", fechaNumeric, "_2_Resultados_Normal.png")
  xLimit = c(0,90)    # Mismo limite para todos
  yLimit = c(0.01,200)  # Anos vs. profundidad
  
  # Labeling 
  PositionX1 = 53 
  PositionY1 = 5 
  Label1     = 'China'
  
  PositionX2 = 23 
  PositionY2 = 160 
  Label2     = 'Spain'
  
  PositionX3 = 40 
  PositionY3 = 100
  Label3     = 'Italy'
  
  PositionX4 = 40 
  PositionY4 = 40
  Label4     = 'EE. UU.'
  
  paises = paises1
  png(Graph1Name, height = heightPDFcm, width = widthPDFcm, units = "cm", res = 300)
  par(mai=c(MarginBottom,MarginLeft,MarginTop,MarginRight)/cm(1))
  
  plot(NA, NA, 
       type = 'n',
       xaxt='n', yaxt='n', ann=FALSE, 
       xlim = xLimit, ylim = yLimit,
       xaxs = "i", yaxs = 'i', 
       lwd = 5)
  
  paises_sobresalir_en_grafica = {''}
  
  funcionGraficarPuntos(paises)

  axis(1, las = 1, cex.axis=sizeP, line = 0)
  axis(2, las = 1, cex.axis=sizeP, line = 0)
  mtext(xLabel, side=1, line=PosicionLabels, cex=sizeP)
  mtext(yLabel, side=2, line=PosicionLabels+0.5, cex=sizeP)
  
  text(PositionX1, PositionY1, Label1)
  text(PositionX2, PositionY2, Label2)
  text(PositionX3, PositionY3, Label3)
  text(PositionX4, PositionY4, Label4)
  
  for(i in seq(1,length(paises))){
    if(paises[i] == 'United_States_of_America'){
      paises[i] = 'EE. UU.'
    }
  }
  
  legend("topleft",bg="white", legend = paises, col = colors, pch = pchs, cex=sizeP, horiz = F)
  
  dev.off(); graphics.off()
}

#### 3 - Grafica logaritmica Todos ####
{
  Graph1Name = paste0(directory,"/", fechaNumeric, "_3_Resultados_Log.png")
  xLimit = c(0,90)    # Mismo limite para todos
  yLimit = c(0.1,1000)  # Anos vs. profundidad
  yy = c('0.1', '1', '10', '100',  '200')
  yNames = c('0.1', '1', '10', '100', '200')
  
  # Labeling 
  PositionX1 = 52
  PositionY1 = 2 
  Label1     = 'China'
  
  PositionX2 = 15 
  PositionY2 = 100 
  Label2     = 'Italy - Spain'
  
  PositionX3 = 10 
  PositionY3 = 0.5
  Label3     = 'Col - Mx'
  
  PositionX4 = 15 
  PositionY4 = 2
  Label4     = ' '
  
  paises = paises1
  png(Graph1Name, height = heightPDFcm, width = widthPDFcm, 
      units = "cm", res = 300)
  par(mai=c(MarginBottom,MarginLeft,MarginTop,MarginRight)/cm(1))
  
  plot(NA, NA, 
       type = 'n',
       xaxt='n', yaxt='n', ann=FALSE, 
       xlim = xLimit, ylim = yLimit,
       xaxs = "i", yaxs = 'i', 
       lwd = 5, log = 'y')
  
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = 19)
  
  paises_sobresalir_en_grafica = {''}
  funcionGraficarPuntos(paises)
  
  axis(1, las = 1, cex.axis=sizeP, line = 0)
  axis(2, las = 1, cex.axis=sizeP, line = 0, at=yy, labels = yNames)
  mtext(xLabel, side=1, line=PosicionLabels, cex=sizeP)
  mtext(yLabel, side=2, line=PosicionLabels+0.5, cex=sizeP)
  
  text(PositionX1, PositionY1, Label1)
  text(PositionX2, PositionY2, Label2)
  text(PositionX3, PositionY3, Label3)
  text(PositionX4, PositionY4, Label4)
  
  for(i in seq(1,length(paises))){
    if(paises[i] == 'United_States_of_America'){
      paises[i] = 'EE. UU.'
    }
  }
  
  legend("topright",bg="white", legend = paises, col = colors, 
         pch = pchs, cex=sizeP, horiz = F)
  
  dev.off(); graphics.off()
}


#### 4 - Grafica logaritmica Mx y Col ####
{
  Graph1Name = paste0(directory,"/", fechaNumeric, "_4_Resultados_Mx_Col.png")
  xLimit = c(0,30)    # Mismo limite para todos
  yLimit = c(0.1,10)  # Anos vs. profundidad
  yy = c('0.1', '1', '10', '100',  '200')
  yNames = c('0.1', '1', '10', '100', '200')
  
  # Labeling 
  PositionX1 = 5
  PositionY1 = 0.3 
  Label1     = 'Italy'
  
  PositionX2 = 10 
  PositionY2 = 0.08
  Label2     = 'Col'
  
  PositionX3 = 8 
  PositionY3 = 0.02
  Label3     = 'Mx'
  
  PositionX4 = 16.5
  PositionY4 = 0.015
  Label4     = 'EE. UU.'
  
  paises = paises1
  png(Graph1Name, height = heightPDFcm, width = widthPDFcm, units = "cm", res = 300)
  par(mai=c(MarginBottom,MarginLeft,MarginTop,MarginRight)/cm(1))
  
  plot(NA, NA, 
       type = 'n',
       xaxt='n', yaxt='n', ann=FALSE, 
       xlim = xLimit, ylim = yLimit,
       xaxs = "i", yaxs = 'i', 
       lwd = 5, log = 'y')
  
  paises_sobresalir_en_grafica = c('Mexico', 'Colombia')
  funcionGraficarPuntos(paises, paises_sobresalir_en_grafica)
  
  axis(1, las = 1, cex.axis=sizeP, line = 0)
  axis(2, las = 1, cex.axis=sizeP, line = 0, at=yy, labels = yNames)
  mtext(xLabel, side=1, line=PosicionLabels, cex=sizeP)
  mtext(yLabel, side=2, line=PosicionLabels+0.5, cex=sizeP)
  
  #text(PositionX1, PositionY1, Label1)
  #text(PositionX2, PositionY2, Label2)
  #text(PositionX3, PositionY3, Label3)
  #text(PositionX4, PositionY4, Label4)
  
  for(i in seq(1,length(paises))){
    if(paises[i] == 'United_States_of_America'){
      paises[i] = 'EE. UU.'
    }
  }
  
  legend("topright",bg="white", legend = paises, col = colors, pch = pchs, cex=sizeP, horiz = F)
  
  dev.off(); graphics.off()
}


# Casos totales de infectados y muertos por pais ####
{
AllCountries  = unique(unlist(data$countriesAndTerritories))
Total         = data.frame()
# Pais, fecha, casos, muertes, % muertes, población, porcentaje casos * 100
for(i in seq(1,length(AllCountries))){
  SubTotal = subset(data, data$countriesAndTerritories == AllCountries[i])
  
  Total[i,1] = AllCountries[i]
  Total[i,2] = as.character(max(as.Date(SubTotal$dateRep, format = '%d/%m/%Y')))
  Total[i,3] = sum(SubTotal$cases)
  Total[i,4] = sum(SubTotal$deaths)
  Total[i,5] = round(Total[i,4]*100/Total[i,3], 2)
  Total[i,6] = unique(SubTotal$popData2018)
  Total[i,7] = round(Total[i,3]*100000/Total[i,6],2)
  Total[i,8] = round(Total[i,4]*100000/Total[i,6],2)
  
}
# Definiendo nombres
colnames(Total) = c('Country', 'DateUpDate', 'TotalCases', 'TotalDeaths', 
                    'DeathsPercent', 'popData2018', 
                    'CasesPer100000', 'DeathsPer100000')


# Ordering and takind the first n countries
n = 8
Total = Total[order(Total$TotalDeaths, decreasing = TRUE),]
TotalSome = Total[1:n,]

# Adding some countries manually 
OthersCountries = c('Colombia', 'Mexico', 'Togo')
for(i in seq(1,length(Total$Country))){
  
  for(j in seq(1, length(OthersCountries))){
    if(Total$Country[i]==OthersCountries[j]){
      TotalSome = rbind(TotalSome, Total[i,])
    }
  }
}

}
# Ploting 
#Absolute = 1, Relative = 2
Absolu_or_Relativ = 0
{
if(Absolu_or_Relativ==1){
  Graph1Name = paste0(directory,"/", fechaNumeric, "_5_BarPlot_Abs.png")
}else{
  Graph1Name = paste0(directory,"/", fechaNumeric, "_6_BarPlot_Rel.png")  
}

png(Graph1Name, height = heightPDFcm, width = widthPDFcm, units = "cm", res = 300)
par(mai=c(MarginBottom+1,MarginLeft,MarginTop+1,MarginRight)/cm(1))

xLimit = c(0,nrow(TotalSome)+1)    # Mismo limite para todos

# Ordering
if(Absolu_or_Relativ==1)
{
  TotalSome = TotalSome[order(TotalSome$TotalCases , decreasing = TRUE),]  
} else{
  TotalSome = TotalSome[order(TotalSome$CasesPer100000, decreasing = TRUE),]
}

# Taking and changing the fucking name
xnames = vector()
for(i in seq(1,length(TotalSome$Country))){
  if(TotalSome$Country[i]=='United_States_of_America'){   xnames[i] = 'US'
  } else if(TotalSome$Country[i]=='United_Kingdom'){      xnames[i] = 'UK'
  }else {                   xnames[i] = as.character(TotalSome$Country[i])
  }
}

MatrixToPlot = matrix(data = NA, ncol = 2, nrow = length(xnames))
row.names(MatrixToPlot) = xnames
if(Absolu_or_Relativ==1){
  MatrixToPlot[,1] = TotalSome$TotalCases/1000
  MatrixToPlot[,2] = TotalSome$TotalDeath/1000
  yLabel1=paste0('No. de casos de Coronavirus (en miles) reportados al ',fecha)
  TituloBar = 'Valores Absolutos'
  yLimit = c(0,500)  
  } else {
  MatrixToPlot[,1] = TotalSome$CasesPer100000
  MatrixToPlot[,2] = TotalSome$DeathsPer100000
  yLabel1 = yLabel
  TituloBar = 'Valores Relativos'
  yLimit = c(0,400)  # Anos vs. profundidad
}

my_bar <- barplot(t.default(MatrixToPlot), names.arg = xnames, las = 2, ylab = yLabel1,
        col = c('blue', 'red'), 
        legend = c('No. de Casos', 'No. de Muerte'), 
        main = TituloBar, 
        ylim = yLimit
)

# Add the text 
text(my_bar, t.default(MatrixToPlot[,1] + MatrixToPlot[,2]) +5 , paste0(round(TotalSome$DeathsPercent,1),"%") ,cex=0.7) 


dev.off(); graphics.off()
}
