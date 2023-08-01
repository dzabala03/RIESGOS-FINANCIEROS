setwd("C:/Users/david/OneDrive - unicolmayor.edu.co/UCMC/OCTAVO SEMESTRE/RIESGOS FINANCIEROS")
getwd()
data_amzn=read.csv("AMZN.csv", header=TRUE)
head(data_amzn)
str(data_amzn) ##estructura de los datos = str
fecha = as.Date(data_amzn$Date, format="%Y-%m-%d")
str(fecha)
data_amzn[ , -1]
data_amzn2 = cbind(fecha, data_amzn[, -1])
head(data_amzn2)
str(data_amzn2)

library(xts)
library(quantmod)

data_amzn3=xts(data_amzn2[, 2:7], order.by = data_amzn2[, 1])
head(data_amzn3)
str(data_amzn3)
class(data_amzn3)
names(data_amzn3) = c("AMZN.Open", "AMZN.High", "AMZN.Low", 
                       "AMZN.Close", "AMZN.Adjusted", "AMZN.Volume")
head(data_amzn3)
head(data_amzn3[, 2:5])
data_amzn4=cbind(data_amzn3[, 1:4], data_amzn3[, 6], data_amzn3[, 5])
head(data_amzn4)

####
library(quantmod)
data_amzn5=getSymbols("AMZN",
                      from ="2014-12-31",
                      to = "2020-01-01", auto.assign = FALSE)
head(data_amzn5)                    
class(data_amzn5)
plot(data_amzn5$AMZN.Close,
     main="Amazon Inc")
dim(data_amzn5)
##los dias de transacción son 252 al año
summary(data_amzn5$AMZN.Close)

##Manipulación de datos
data_amzn5[1, ]
data_amzn5[5:8, ]
data_amzn5[c(1,4,5), ]
data_amzn5[c(1,4, nrow(data_amzn5) )]

###columnas

head(data_amzn5[, 4])
head(data_amzn5[ , c(1,4,6)])

###imprimir en pantalla los últimos 6 datos de las columnas 2, 3 y 4
data
tail(data_amzn5[, c(2,3,4)], 6)
data_amzn5[1254:1259, 2:4]

##subconjuntos
amzn_2018=data_amzn5["2018-01-01/2019-01-01"]
head(amzn_2018)
tail(amzn_2018)
index(amzn_2018)

amzn_2018b=subset(data_amzn5,
                 index(data_amzn5) >="2018-01-01" &
                   index(data_amzn5) >"2019-01-01")
head(amzn_2018b)
tail(amzn_2018b)

amzn_close_mayor100=subset(data_amzn5, data_amzn5$AMZN.Close>=100)
dim(amzn_close_mayor100)

amzn_close50_vol54671000=subset(data_amzn5, data_amzn5$AMZN.Close<=50 &
                                  data_amzn5$AMZN.Volume<=54671000)
dim(amzn_close50_vol54671000)


###cambio de frecuencias
wk=data_amzn5
to.weekly(wk)
no=data_amzn5
to.monthly(no)
?to.monthly

###gráficas de precios normalizados
head(data_amzn5$AMZN.Close)
norm_price_amzn=data_amzn5$AMZN.Close
head(norm_price_amzn)
data_amzn5$AMZN.Close[1, ]
primer_valor=as.numeric(data_amzn5$AMZN.Close[1, ])
norm_price_amzn=data_amzn5$AMZN.Close / primer_valor
head(norm_price_amzn)
tail(norm_price_amzn)
plot(norm_price_amzn) ##si inv 1 unidad se multiplica 6 veces en 5 años

###Varios activos: AMZ, GOOG, AAPL, SPY
data_amzn = getSymbols("AMZN", 
                       from = "2014-12-31", 
                       to = "2020-01-01", auto.assign = FALSE)
data_goog = getSymbols("GOOG", 
                       from = "2014-12-31", 
                       to = "2020-01-01", auto.assign = FALSE)
data_aapl = getSymbols("AAPL", 
                       from = "2014-12-31", 
                       to = "2020-01-01", auto.assign = FALSE)
data_spy = getSymbols("SPY", 
                      from = "2014-12-31",
                      to = "2020-01-01", auto.assign = FALSE)
close_data=cbind(data_amzn$AMZN.Close, data_goog$GOOG.Close, 
                 data_aapl$AAPL.Close, data_spy$SPY.Close)
head(close_data)
normaliced_price=close_data
normaliced_price$AMZN.Close=close_data$AMZN.Close/as.numeric(normaliced_price[1,1])
normaliced_price$GOOG.Close=close_data$GOOG.Close/as.numeric(normaliced_price[1,2])
normaliced_price$AAPL.Close=close_data$AAPL.Close/as.numeric(normaliced_price[1,3])
normaliced_price$SPY.Close=close_data$SPY.Close/as.numeric(normaliced_price[1,4])

head(normaliced_price)
tiempo=index(normaliced_price)
plot(x=tiempo,
     y=normaliced_price$AMZN.Close,
     xlab="Fecha",
     ylab="Comportamiento inversión",
     type='l',
     main="Valor de una inversión de $1"
     )
lines(x=tiempo, y=normaliced_price$GOOG.Close, col="blue")
lines(x=tiempo, y=normaliced_price$AAPL.Close, col="red")
lines(x=tiempo, y=normaliced_price$SPY.Close, col="darkgreen")
grid(col = "gray")
legend("topleft",
       c("AMZN", "GOOG", "AAPL", "SPY"),
       lwd = c(1, 1, 1, 1),
       col = c("black", "blue", "red", "darkgreen"))


####Velas Japonesas
ohlc = to.monthly(data_amzn)
head(ohlc)
ohlc=ohlc[-1, -6]
amzn_ohlc=as.quantmod.OHLC(ohlc, col.names = c("Open", "High", "Low",
                                               "Close", "Volume"))
chartSeries(amzn_ohlc,
             theme = "white")

###Quiz: cuatro acciones (diferentes) yahoo finance
###Graficar los valores normalizados
##Anexar R, y el gráfico en un Word
##El título del gráfico debe ser su nombre

data_uber = getSymbols("UBER", 
                       from = "2018-12-31", 
                       to = "2023-01-01", auto.assign = FALSE)