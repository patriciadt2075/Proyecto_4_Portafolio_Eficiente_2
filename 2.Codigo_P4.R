#Paquetes Requeridos
library(readxl)
library(ggplot2)
library(timeSeries)
library(fPortfolio)

#Data
Walmart <- read_excel("~/Proyecto_4_Portafolio_Eficiente_2/4.Data_Walmart_P4.xlsx")
Walmart$Fecha <- as.Date(Walmart$Fecha) #Transformando la columna "Fecha", del data frame "Walmart", a tipo de dato "Date" 

Costco <- read_excel("~/Proyecto_4_Portafolio_Eficiente_2/5.Data_Costco_P4.xlsx")
Costco$Fecha <- as.Date(Costco$Fecha) #Transformando la columna "Fecha", del data frame "Costco", a tipo de dato "Date" 

US10Y<- read_excel("~/Proyecto_4_Portafolio_Eficiente_2/6.Data_US10Y_P4.xlsx")
US10Y$Fecha <- as.Date(US10Y$Fecha) #Transformando la columna "Fecha", del data frame "US10Y", a tipo de dato "Date" 





#1 GRÁFICO DE PRECIOS DIARIOS DE WALMART Y COSTCO

#Gráfico de Precios Diarios de Walmart
ggplot(Walmart, aes(x = Fecha, y = Precio_de_Cierre_Ajustado_WMT_USD)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Fecha", y = "Precio (USD)", title = "Gráfico de Precios Diarios de Walmart (Del 01/01/2019 al 12/31/2023)")

#Gráfico de Precios Diarios de Costco
ggplot(Costco, aes(x = Fecha, y = Precio_de_Cierre_Ajustado_COST_USD)) + geom_bar(stat = "identity", fill = "red") + labs(x = "Fecha", y = "Precio (USD)", title = "Gráfico de Precios Diarios de Costco (Del 01/01/2019 al 12/31/2023)")






#2 CÁLCULO Y GRÁFICO DE LOS RENDIMIENTOS LOGARÍTMICOS DIARIOS DE WALMART Y COSTCO

#Columnas del Data Frame "Rendimientos_Logaritmicos_Walmart"
Fecha = Walmart$Fecha
Rendimientos_Logaritmicos_WMT_Porcentaje = c(NA, diff(log(Walmart$Precio_de_Cierre_Ajustado_WMT_USD)) * 100)

##Columnas del Data Frame "Rendimientos_Logaritmicos_Costco"
Fecha = Costco$Fecha
Rendimientos_Logaritmicos_COST_Porcentaje = c(NA, diff(log(Costco$Precio_de_Cierre_Ajustado_COST_USD)) * 100)

#Data Frame "Rendimientos_Logaritmicos_Walmart"
Rendimientos_Logaritmicos_Walmart <- na.omit(data.frame(Fecha, Rendimientos_Logaritmicos_WMT_Porcentaje))

#Data Frame "Rendimientos_Logaritmicos_Costco"
Rendimientos_Logaritmicos_Costco <- na.omit(data.frame(Fecha, Rendimientos_Logaritmicos_COST_Porcentaje))

#Gráfico de Rendimientos Logarítmicos de Walmart
ggplot(Rendimientos_Logaritmicos_Walmart, aes(x = Fecha, y = Rendimientos_Logaritmicos_WMT_Porcentaje)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Fecha", y = "Rendimientos Logarítmicos (%)", title = "Gráfico de Rendimientos Logarítmicos de Walmart (Del 01/01/2019 al 31/12/2023)")

#Gráfico de Rendimientos Logarítmicos de Costco
ggplot(Rendimientos_Logaritmicos_Costco, aes(x = Fecha, y = Rendimientos_Logaritmicos_COST_Porcentaje)) + geom_bar(stat = "identity", fill = "red") + labs(x = "Fecha", y = "Rendimientos Logarítmicos (%)", title = "Gráfico de Rendimientos Logarítmicos de Costco (Del 01/01/2019 al 31/12/2023)")





#3 CÁLCULO Y GRÁFICO DE LA FRONTERA EFICIENTE PARA 16 PORTAFOLIOS

#Promedio de los Rendimientos Logarítmicos de Walmart
μ1 <- mean(Rendimientos_Logaritmicos_Walmart$Rendimientos_Logaritmicos_WMT_Porcentaje)

#Promedio de los Rendimientos Logarítmicos de Costco
μ2 <- mean(Rendimientos_Logaritmicos_Costco$Rendimientos_Logaritmicos_COST_Porcentaje)

#Desviación Estandar de los Rendimientos Logarítmicos de Walmart
σ1 <- sd(Rendimientos_Logaritmicos_Walmart$Rendimientos_Logaritmicos_WMT_Porcentaje)

#Desviación Estandar de los Rendimientos Logarítmicos de Costco
σ2 <- sd(Rendimientos_Logaritmicos_Costco$Rendimientos_Logaritmicos_COST_Porcentaje)

#Correlación de los Rendimientos Logarítmicos de Walmart y Costco
ρ <- cor(Rendimientos_Logaritmicos_Walmart$Rendimientos_Logaritmicos_WMT_Porcentaje, Rendimientos_Logaritmicos_Costco$Rendimientos_Logaritmicos_COST_Porcentaje)

#Columnas del Data Frame "Portafolios"
Num_Portafolio = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
W_WMT = c(2.0, 1.8, 1.6, 1.4, 1.2, 1.0, 0.8, 0.6, 0.4, 0.2, 0.0, -0.2, -0.4, -0.6, -0.8, -1.0) #Pesos_de_Walmart
W_COST = c(-1.0, -0.8, -0.6, -0.4, -0.2, 0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0) #Pesos_de_Costco
μp  =  ((μ1*W_WMT) + (μ2*W_COST))
σp = sqrt(((W_WMT^2)*(σ1^2))+((W_COST^2)*(σ2^2))+(2*ρ*W_WMT*W_COST*σ1*σ2))

#Data Frame "Portafolios"
Portafolios <- data.frame(Num_Portafolio,W_WMT,W_COST,μp,σp)

#Gráfico de la Frontera Eficiente
ggplot(Portafolios, aes(x = σp, y = μp)) + geom_point() + labs(x = "Riesgo-σp (%)", y = "Retorno-μp (%)", title = "Gráfico de Frontera Eficiente para los 16 Portafolios")





#4 CÁLCULO Y GRÁFICO DEL PORTAFOLIO DE MÍNIMA VARIANZA (PMV)

#Data Frame "Rendimientos"
Rendimientos <- na.omit(data.frame(Fecha,Rendimientos_Logaritmicos_WMT_Porcentaje,Rendimientos_Logaritmicos_COST_Porcentaje))
Rendimientos_ST <- as.timeSeries(Rendimientos)#Rendimientos en series de tempo
                                           
#Tasa Libre de Riesgo
Tasa_Libre_de_Riesgo_Porcentaje<- mean(US10Y$Precio_de_Cierre_Ajustado_US10Y_USD)

#PMV
PMV <- minvariancePortfolio(Rendimientos_ST, `setRiskFreeRate<-`(portfolioSpec(),2.272104))

#Pesos del PMV
W_PMV <- round(getWeights(PMV),1)
print(W_PMV)
#El portafolio de mínima varianza (PMV) coincide con el portafolio número 8 de los 16 portafolios del data frame ""Portafolios"

#Gráfico del PMV
Frontera_Eficiente <- portfolioFrontier(Rendimientos_ST)
plot(Frontera_Eficiente, c(1))
plot(Frontera_Eficiente, c(2)) #PMV