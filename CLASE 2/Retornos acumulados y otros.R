library(quantmod)
data_apple=getSymbols("AAPL",
                      from="2014-12-31",
                      to="2019-12-31", auto.assign=FALSE)
head(data_apple)
plot(data_apple$AAPL.Close)
aapl_close=data_apple$AAPL.Close
aapl_close2=data_apple$AAPL.Close

head(aapl_close)
#reescribir nombre data
names(aapl_close)="price"
aapl_close$lagPrice=lag(aapl_close$price, k=1)
head(aapl_close)

##calcular retornos
aapl_close$retorno=(aapl_close$price/aapl_close$lagPrice-1)
head(aapl_close)
options(scipen=9) #quita notacion cientifica

aapl_close=aapl_close[-1,] #quitar el NA
head(aapl_close)

##Otra opcion para calcular
#funcion delta
total_returns=Delt(aapl_close2$AAPL.Close)
head(total_returns)
total_returns=total_returns[-1, ]##quitar la primera fila

cbind(aapl_close$retorno, total_returns)

####retorno logaritmico
aaple_log_returns=diff(log(data_apple$AAPL.Close), k=1)
head(aaple_log_returns)
aaple_log_returns=aaple_log_returns[-1, ]
head(cbind(total_returns, aaple_log_returns))
plot(total_returns)
lines(aaple_log_returns, col="red")
##son preferibles los retornos logaritmicos
###retornos acumulados
head(total_returns)
gross_return=1+total_returns #retornos gruesos: sumarle 1 a los retornos
head(gross_return)
gross_return=gross_return[-1, ]
cum_return=cumprod(gross_return)
head(cum_return)
tail(cum_return) ##me muestra el rendimiento al final del año o periodo t
plot(cum_return)

##retornos logaritmicos acumulados
head(aaple_log_returns)
cum_log_returns=sum(aaple_log_returns)

cum_log_returns
exp(cum_log_returns)-1 ##el mismo valor que cumpro-1
exp(cum_log_returns)
#2.64=rendimiento al final del periodo de inversion