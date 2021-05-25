rm(list = ls())
# Graficar crecimiento de coronaVirus
# Félix Ernesto Charry Pastrana

#these libraries need to be loaded
library(utils)
library(httr)
library(RColorBrewer)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
    authenticate(":", ":", type="ntlm"), 
    write_disk(tf <- tempfile(fileext = ".csv")))

# Valores para modificar -------------------------------------------------------
fecha           = Sys.Date()
paises          = 'Netherlands' # Paises de interés
nMonteCarlos    = 100

FactorC       = 10000
xLimit        = c(0,120)
yLimit        = c(0, 10)

FactorM  = 100
xLimitMuertes = c(0,120)
yLimitMuertes = c(0, 10)

# ------------------------------------------------------------------------------
# Mac FECHP
directory="/Users/F.E.CharryPastrana/Documents/GitHub_PersonalCodes/Covid-19"
setwd(directory)

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv(tf)

# Extrayendo la información del país de interés 
for(ii in seq(1,length(paises))){
  pais = paises[ii]
  
  j = 1; startFlag = 0; jcasos = 1; jmuertos = 1
  
  diascasos       <- vector(mode="numeric", length=0)
  diasmuertos     <- vector(mode="numeric", length=0)
  casos           <- vector(mode="numeric", length=0)
  muertos         <- vector(mode="numeric", length=0)
  
  print(paises[ii])
  # Extrayendo informacion por país
  for(i in seq(nrow(data),1)){
    if( (as.character(data$countriesAndTerritories[i]) == pais) ){
      
      dia = j
      
      if(as.numeric(data$cases[i]) != 0){
        diascasos[jcasos] = dia
        casos[jcasos]     = as.numeric(data$cases[i]) 
        jcasos            = jcasos + 1
        startFlag         = 1
      }
      if(as.numeric(data$deaths[i]) != 0){
        diasmuertos[jmuertos] = dia
        muertos[jmuertos]     = as.numeric(data$deaths[i])
        jmuertos              = jmuertos+1
        startFlag             = 1
      }
      
      j = j+1
      
      if(startFlag==0){j=1}
    }
  }
  
  # La incertidumbre es la raíz de los casos o muertos reportados
  casosInc    = sqrt(casos)
  muertosInc  = sqrt(muertos)
  
  casosAcumul       <- vector(mode="numeric", length=0)
  casosAcumulInc    <- vector(mode="numeric", length=0)
  muertosAcumul     <- vector(mode="numeric", length=0)
  muertosAcumulInc  <- vector(mode="numeric", length=0)
  
  # Casos acumulados
  casosAcumul[1]    = casos[1]
  casosAcumulInc[1] = casosInc[1]
  
  for(i in seq(2,length(casos))){
    casosAcumul[i]      = casos[i] + casosAcumul[i-1]
    casosAcumulInc[i]   = sqrt( casosInc[i]**2 + casosAcumulInc[i-1]**2)
  }
  
  # Muertos acumulados
  muertosAcumul[1]    = muertos[1]
  muertosAcumulInc[1] = muertosInc[1]
  
  for(i in seq(2,length(muertos))){
    muertosAcumul[i] = muertos[i] + muertosAcumul[i-1]
    muertosAcumulInc[i] = sqrt( muertosInc[i]**2 + muertosAcumulInc[i-1]**2)
  }
    
  
  Original_Casos = data.frame(diascasos, 
                              casos, casosInc, 
                              casosAcumul, casosAcumulInc)
  Original_Muertes  = data.frame(diasmuertos, 
                                 muertos, muertosInc, 
                                 muertosAcumul, muertosAcumulInc)
};

# Parametros comunes #### ------------------------------------------------------
# C denota Casos, M denota Muertos
ColorLineaTransparenteC  = rgb(54/255,144/255,192/255,0.05)
ColorLineaOscuroC        = rgb(54/255,144/255,192/255, 1)
ColorLineaTransparenteM  = rgb(222/255,45/255,38/255,0.05)
ColorLineaOscuroM        = rgb(222/255,45/255,38/255, 1)

widthPDFcm = 11 # cm
heightPDFcm = 20 # cm 

# Magins in cm 
MarginLeft   = 2
MarginRight  = 0.3
MarginTop    = 0.3
MarginBottom = 2

xLabel = "Días después de 10 casos reportados"

PosicionLabels = 2.5

ResultadosParametrosMonteCarloC = data.frame()

#### Comienza el proceso de MonteCarlo y Graficar #### -------------------------
{
Graph1Name = paste0(directory,"/Proyection_Results/", fecha)
Graph1Name = paste0(Graph1Name, "_", pais, "_CasesProyection.png")
                
PosicionLabels = 2.5
sizeP = 1

yLabel = paste0('Casos acumulados de COVID-19 reportados al ', fecha)
yLabel = paste0(yLabel, ' en ', sprintf("%i", FactorC), " unidades")

png(Graph1Name, height=heightPDFcm, width=widthPDFcm, units = "cm", res = 300)
par(mai=c(MarginBottom,MarginLeft,MarginTop,MarginRight)/cm(1))

plot(NA, NA, 
     type = 'n',
     xaxt='n', yaxt='n', ann=FALSE, 
     xlim = xLimit, ylim = yLimit,
     xaxs = "i", yaxs = 'i', 
     lwd = 5)

grid(nx = NULL, ny = NULL, col = "lightgray", lty = 19)

axis(1, las = 1, cex.axis=sizeP, line = 0)
axis(2, las = 1, cex.axis=sizeP, line = 0)
mtext(xLabel, side=1, line=PosicionLabels, cex=sizeP)
mtext(yLabel, side=2, line=PosicionLabels, cex=sizeP)

points(Original_Casos$diascasos, Original_Casos$casosAcumul/FactorC)

x = diascasos
y = casos
xFit<-c(min(xLimit):max(xLimit)) 

# Comenzando Monte Carlo
for(j in seq(1,nMonteCarlos)){
  yAleatorio = rnorm(length(casosAcumul),casosAcumul, casosAcumulInc)/FactorC
  
  coef(lm(log(y)~x))
  
  # We need initial parameters
  # phi1 : valor de la gráfica cuando t tiende a infinito 
  wilson<-nlsLM(yAleatorio~phi1/(1+exp(-(phi2+phi3*x))),
              start=list(phi1=6,phi2=-0.2,phi3=.1),trace=TRUE)

  #Extrayendo parameters
  variable1<-summary(wilson)$coef[1,1]; sig1<-summary(wilson)$coef[1,2]
  variable2<-summary(wilson)$coef[2,1];  sig2<-summary(wilson)$coef[3,2]
  variable3<-summary(wilson)$coef[3,1]; sig3<-summary(wilson)$coef[3,2]
  
  phi1<-rnorm(1, mean = variable1, sd = sig1)
  phi2<-rnorm(1, mean = variable2, sd = sig2)
  phi3<-rnorm(1, mean = variable3, sd = sig3)
  
  yFit          <-phi1/(1+exp(-(phi2+phi3*xFit)))
  yFitDerivate  <- phi1*phi3*(1+exp(-(phi2+phi3*xFit)))**(-2)*
                    exp(-(phi2+phi3*xFit))
  
  lines(xFit, yFit, col = ColorLineaTransparenteC)
  print('MonteCarlo')
  print(j)
  
  ResultadosParametrosMonteCarloC[j,1] = variable1
  ResultadosParametrosMonteCarloC[j,2] = sig1  
  ResultadosParametrosMonteCarloC[j,3] = variable2
  ResultadosParametrosMonteCarloC[j,4] = sig2   
  ResultadosParametrosMonteCarloC[j,5] = variable3
  ResultadosParametrosMonteCarloC[j,6] = sig2 
};

points(Original_Casos$diascasos, Original_Casos$casosAcumul/FactorC)
legend("topleft",bg="white", legend = paises, 
       col = ColorLineaOscuroC, lty = 19, cex=sizeP, horiz = F)

dev.off(); graphics.off()
}

# Muertes #### 
{
Graph1Name = paste0(directory,"/Proyection_Results/", fecha)
Graph1Name = paste0(Graph1Name, "_", pais, "_DeathsProyection.png")

yLabel = paste0('Muertes acumulados de COVID-19 reportados al ', fecha)
yLabel = paste0(yLabel, ' en ', sprintf("%i", FactorM), " unidades")

png(Graph1Name, height=heightPDFcm, width=widthPDFcm, units = "cm", res = 300)
par(mai=c(MarginBottom,MarginLeft,MarginTop,MarginRight)/cm(1))

plot(NA, NA, 
     type = 'n',
     xaxt='n', yaxt='n', ann=FALSE, 
     xlim = xLimitMuertes, ylim = yLimitMuertes,
     xaxs = "i", yaxs = 'i', 
     lwd = 5)

grid(nx = NULL, ny = NULL, col = "lightgray", lty = 19)

axis(1, las = 1, cex.axis=sizeP, line = 0)
axis(2, las = 1, cex.axis=sizeP, line = 0)
mtext(xLabel, side=1, line=PosicionLabels, cex=sizeP)
mtext(yLabel, side=2, line=PosicionLabels, cex=sizeP)


points(Original_Muertes$diasmuertos,
       Original_Muertes$muertosAcumul/FactorM)

x = diasmuertos
y = muertosAcumul/FactorM
xFit<-c(min(xLimitMuertes):max(xLimitMuertes)) 

# Comenzando Monte Carlo
for(j in seq(1,nMonteCarlos)){
  yAleatorio = rnorm(length(muertosAcumul),muertosAcumul, 
                     muertosAcumulInc)/FactorM
  x = diasmuertos
  
  coef(lm(log(y)~x))
  
  # We need initial parameters
  # phi1 : valor de la gráfica cuando t tiende a infinito 
  wilson<-nlsLM(yAleatorio~phi1/(1+exp(-(phi2+phi3*x))),
                start=list(phi1=5,phi2=-17,phi3=1.2),trace=TRUE)
  
  #Extrayendo parameters
  variable1<-summary(wilson)$coef[1,1]; sig1<-summary(wilson)$coef[1,2]
  variable2<-summary(wilson)$coef[2,1];  sig2<-summary(wilson)$coef[3,2]
  variable3<-summary(wilson)$coef[3,1]; sig3<-summary(wilson)$coef[3,2]
  
  phi1<-rnorm(1, mean = variable1, sd = sig1)
  phi2<-rnorm(1, mean = variable2, sd = sig2)
  phi3<-rnorm(1, mean = variable3, sd = sig3)
  
  yFit          <-phi1/(1+exp(-(phi2+phi3*xFit)))
  yFitDerivate  <- phi1*phi3*(1+exp(-(phi2+phi3*xFit)))**(-2)*
    exp(-(phi2+phi3*xFit))
  
  lines(xFit, yFit, col = ColorLineaTransparenteM)
  print('MonteCarlo')
  print(j)
}

points(Original_Muertes$diasmuertos, Original_Muertes$muertosAcumul/FactorM)
legend("topleft",bg="white", legend = paises, 
       col = ColorLineaOscuroM, lty = 19, cex=sizeP, horiz = F)

dev.off(); graphics.off()
}
 
