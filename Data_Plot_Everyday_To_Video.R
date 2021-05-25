# Félix Ernesto Charry Pastrana
# Ciudad de México, 11 de abril de 2020 
# Gráficas de los casos por días para diferentes países para determinado día.
# Las gráficas se unirán para formar un vídeo
rm(list = ls())

#these libraries need to be loaded
library(utils)
library(httr)
library(RColorBrewer)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
    authenticate(":", ":", type="ntlm"), 
    write_disk(tf <- tempfile(fileext = ".csv")))

# User data ####
paises1 = c('China', 'Spain', 'Italy', 
            'United_States_of_America', 'Colombia',
            'Mexico', 'Togo')
paises2 = c('China', 'Spain', 'Italy', 
            'EE. UU.', 'Col.',
            'Mx.', 'Togo')

# ¿Qué vamos a graficar? Cases, death, casesAcum o DeathAcum? Or something 
# iColumna: cases = 2, death = 4, cases Acum = 6, death Acum = 8 
iColumna = 8
#
# STARTING ####
# Mac FECHP
directory="/Users/F.E.CharryPastrana/Documents/GitHub_PersonalCodes/Covid-19"
directory=paste0(directory,'/Plots_Everyday_To_Video/')
setwd(directory)

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv(tf)

# DATA FRAME MODIFIQUE ####
# Adding uncertainty to cases and deaths. As well as calculating the acumulative
# data
dataModif <- data[-c(8,9)]
dataModif["cases.inc"]              = sqrt(dataModif$cases)
dataModif["deaths.inc"]             = sqrt(dataModif$deaths)
dataModif["cases.acumulado"]        = 0
dataModif["cases.acumulado.inc"]    = 0
dataModif["deaths.acumulado"]       = 0
dataModif["deaths.acumulado.inc"]   = 0

uniqueCountries = unique(dataModif$countriesAndTerritories)

for(i in seq(1, length(uniqueCountries))){
  CountryDat=dataModif[dataModif$countriesAndTerritories == uniqueCountries[i],]
  
  NoDatos = nrow(CountryDat)
  CountryDat$cases.acumulado[NoDatos]   = CountryDat$cases[NoDatos]
  CountryDat$deaths.acumulado[NoDatos]  = CountryDat$deaths[NoDatos]
  CountryDat$cases.acumulado.inc[NoDatos] = CountryDat$cases.inc[NoDatos]
  CountryDat$deaths.acumulado.inc[NoDatos] = CountryDat$deaths.inc[NoDatos]
  
  for(j in seq(NoDatos-1,1)){
    CountryDat$cases.acumulado[j] = CountryDat$cases[j] + 
              CountryDat$cases.acumulado[j+1]
    CountryDat$cases.acumulado.inc[j] = sqrt(CountryDat$cases.inc[j]**2 +
              CountryDat$cases.acumulado.inc[j+1]**2)
    CountryDat$deaths.acumulado[j] = CountryDat$deaths[j] + 
              CountryDat$deaths.acumulado[j+1]
    CountryDat$deaths.acumulado.inc[j] = sqrt( CountryDat$deaths.inc[j]**2 + 
              CountryDat$deaths.acumulado.inc[j+1]**2)
  }
  
  dataModif[dataModif$countriesAndTerritories == uniqueCountries[i], 
            "cases.acumulado"] = CountryDat$cases.acumulado
  dataModif[dataModif$countriesAndTerritories == uniqueCountries[i], 
            "deaths.acumulado"] = CountryDat$deaths.acumulado
  dataModif[dataModif$countriesAndTerritories == uniqueCountries[i], 
            "cases.acumulado.inc"] = CountryDat$cases.acumulado.inc
  dataModif[dataModif$countriesAndTerritories == uniqueCountries[i], 
            "deaths.acumulado.inc"] = CountryDat$deaths.acumulado.inc
}

# DATA FRAME PLOT ####

colors = brewer.pal(n = length(paises1) , name = 'Dark2')
paises = paises1

DFramePlot = data.frame()
  
for(ii in seq(1,length(paises))){
  pais = paises[ii]
  print(paises[ii])
  
  j = 1
  
  dias    <- vector(mode="numeric", length=0)
  diasreales <- vector(mode="numeric", length=0); class(diasreales) <- "Date"
  cases <- vector(mode="numeric", length=0)
  death <- vector(mode="numeric", length=0)
  cases.unc <- vector(mode="numeric", length=0)
  death.unc <- vector(mode="numeric", length=0)
  cases.Acu <- vector(mode="numeric", length=0)
  death.Acu <- vector(mode="numeric", length=0)
  cases.Acu.unc <- vector(mode="numeric", length=0)
  death.Acu.unc <- vector(mode="numeric", length=0)
  popData2018 <- vector(mode="numeric", length=0)
  
  # Extrayendo informacion
  for(i in seq(nrow(data),1)){
    if( (as.character(dataModif$countriesAndTerritories[i]) == pais) ){
      dias[j]      = j
      diasreales[j]     = as.POSIXct(data$dateRep[i], format="%d/%m/%Y" )
      cases[j]          = as.numeric(dataModif$cases[i]) 
      death[j]          = as.numeric(dataModif$deaths[i])
      cases.unc[j]      = as.numeric(dataModif$cases.inc[i])
      death.unc[j]      = as.numeric(dataModif$deaths.inc[i])
      cases.Acu[j]      = as.numeric(dataModif$cases.acumulado[i])
      death.Acu[j]      = as.numeric(dataModif$deaths.acumulado[i])
      cases.Acu.unc[j]  = as.numeric(dataModif$cases.acumulado.inc[i]) 
      death.Acu.unc[j]  = as.numeric(dataModif$deaths.acumulado.inc[i])
      popData2018[j]    = as.numeric(dataModif$popData2018[i])
      j = j+1
    }
  }
  
  cases[cases==0] <- NA; death[death==0] <- NA; 
  cases.unc[cases.unc==0] <- NA; death.unc[death.unc==0] <- NA; 
  cases.Acu[cases.Acu==0] <- NA; death.Acu[death.Acu==0] <- NA; 
  cases.Acu.unc[cases.Acu.unc==0] <- NA; death.Acu.unc[death.Acu.unc==0] <- NA; 
  
  Names1 = c('cases','cases.unc','death','death.unc',
             'casesAcu','casesAcu.unc','deathAcu','deathAcu.unc',
             'popData2018')
  Names1 = paste0(as.character(pais),'.',Names1)
  Names1 = c('date', Names1)
  
  paisName = as.character(pais)
  fechaName = 'date'
  casoName = paste0('cases',paisName)
  casosRelName = paste0('casesRel',paisName)
  muertosName = paste0('deaths',paisName)
    
  if(ii==1){
    DFramePlot = data.frame(diasreales, 
                         cases, cases.unc, death, death.unc,
                         cases.Acu, cases.Acu.unc, death.Acu, death.Acu.unc, 
                         popData2018)
    colnames(DFramePlot) = Names1
  }else{
    DFrame2 = data.frame(diasreales, 
                         cases, cases.unc, death, death.unc,
                         cases.Acu, cases.Acu.unc, death.Acu, death.Acu.unc, 
                         popData2018)
    colnames(DFrame2) = Names1
    
    DFramePlot = merge(DFramePlot, DFrame2, all = TRUE)
  }
}


deltaiColumna = (ncol(DFramePlot)-1)/length(paises)

# Starting PLOT ####
# Axis label
xLabel = "Días"

if(iColumna==2){
  yLabel = 'No. de casos COVID-19 (valores absolutos)'  
}else if(iColumna==4){
  yLabel = 'No. de muertes COVID-19 (valores absolutos)'
}else if(iColumna==6){
  yLabel = 'No. de casos COVID-19 (valores acumulados)'
}else if(iColumna==8){
  yLabel = 'No. de muertes COVID-19 (valores acumulados)'
}


cexT = 0.6                # Point size  
ldwT = 1.7                # Line width
pchs = 19
ltys = 1
PosicionLabels = 2
sizeP = 1
k = 50

subtitleName=paste0('Fuente: opendata.ecdc.europa.eu/covid19/',
                    '\n@ernesto_charry')

MarginLeft   = 3        # cm
MarginRight  = 0.3     # cm
MarginTop    = 1         # cm 
MarginBottom = 3    # cm 

widthPDFcm = 11 # cm 1127
heightPDFcm = 20 # cm 2008

for(k in seq(10,nrow(DFramePlot))){
  jInicialDate  = 1
  jFinalDate    = k

  titleName = paste0('Fecha: ',as.character(DFramePlot$date[jFinalDate]))

  print(sprintf('Valor de k: %d. Fecha graficada: %s', k, titleName))

  if(jFinalDate<100){
    secuencia = paste0(as.character(iColumna),'-0',as.character(jFinalDate))
  }else{
  secuencia = paste0(as.character(iColumna),'-',as.character(jFinalDate))
  }
  Graph1Name = paste0(directory,"Plots/", secuencia, ".png")



  x1 = seq(jInicialDate, jFinalDate)

  max1 = 1
  min1 = 1000

  # Para extraer limites
  for(i in seq(iColumna, ncol(DFramePlot)-1, deltaiColumna)){
    y = DFramePlot[jInicialDate:jFinalDate,i]
    if(sum(is.na(y) ==TRUE) != length(y)){
      if( min(y, na.rm=T)<min1){min1 = min(y, na.rm=T)}
      if( max(y, na.rm=T)>max1){max1 = max(y, na.rm=T)}
    }
  }

  xLimit = c(0,max(x1)+1)    # Mismo limite para todos
  #xLimit = c(0,120)    # Mismo limite para todos
  yLimit = c(min1,ceiling(max1/5)*5 )  # Anos vs. profundidad

  png(Graph1Name, height = heightPDFcm, width = widthPDFcm, 
      units = "cm",res=100)
  par(mai=c(MarginBottom,MarginLeft,MarginTop,MarginRight)/cm(1))

  plot(NA, NA, 
     type = 'n',
     xaxt='n', yaxt='n', ann=FALSE, 
     xlim = xLimit, ylim = yLimit,
     xaxs = "i", yaxs = 'i', 
     lwd = 5, log = 'y')

  title(main = titleName, sub = subtitleName, cex.sub=0.8)

  jColor = 1
  i = iColumna
  LegendNumbers = paises2
  for(i in seq(iColumna, ncol(DFramePlot)-1, deltaiColumna)){
    y  = DFramePlot[jInicialDate:jFinalDate,i]
    dy = DFramePlot[jInicialDate:jFinalDate,i+1]
    
    x   = x1[(is.na(y)==FALSE) & (y>1)]
    dy  = dy[(is.na(y)==FALSE) & (y>1)]
    y   = y[(is.na(y)==FALSE) & (y>1)]

    lower = y-dy
    upper = y+dy
    
    if(sum(is.na(y) ==TRUE) != length(y)){
      lines(x, y, col=colors[jColor], lwd = 1, lty = ltys)
      points(x, y, col=colors[jColor], pch=pchs, cex = cexT)
      polygon( c(x, rev(x)), c(upper, rev(lower)), col = rgb(0,0,0,0.1),border = NA)
      
      if(is.na(y[length(y)])==FALSE){
        LegendNumbers[jColor] = paste0(paises2[jColor],
                                       ': ', 
                                       as.character(y[length(y)]))
      }
      
    }
    
    
    
    
    jColor = jColor+1
  }
  
  axis(1, las = 1, cex.axis=sizeP, line = 0)
  axis(2, las = 1, cex.axis=sizeP, line = 0)
  mtext(xLabel, side=1, line=PosicionLabels, cex=sizeP)
  mtext(yLabel, side=2, line=PosicionLabels+1.5, cex=sizeP)

  
  legend("topleft",bg="white", legend = LegendNumbers, 
       col = colors, pch = pchs, cex=sizeP, horiz = F)

  dev.off(); graphics.off()

}

# Into GIF ####
# run ImageMagick
my_command <- paste0('convert Plots/', as.character(iColumna),
                      '-*.png -delay 10 ', as.character(iColumna), 
                      '-animation.gif')
system(my_command)

print(sprintf('Nombre archivo: %s', 
              paste0(as.character(iColumna),'-animation.gif')))