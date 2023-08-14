###Quiz: cuatro acciones (diferentes) yahoo finance
###Graficar los valores normalizados
##Anexar R, y el gráfico en un Word
##El título del gráfico debe ser su nombre

data_dal = getSymbols("DAL", 
                      from = "2018-12-31", 
                      to = "2023-01-01", auto.assign = FALSE)
data_nflx = getSymbols("NFLX", 
                       from = "2018-12-31", 
                       to = "2023-01-01", auto.assign = FALSE)
data_cvna = getSymbols("CVNA", 
                       from = "2018-12-31", 
                       to = "2023-01-01", auto.assign = FALSE)
data_aal = getSymbols("AAL", 
                      from = "2018-12-31", 
                      to = "2023-01-01", auto.assign = FALSE)
close_data=cbind(data_dal$DAL.Close, data_nflx$NFLX.Close, 
                 data_cvna$CVNA.Close, data_aal$AAL.Close)

head(close_data)
normaliced_price=close_data
normaliced_price$DAL.Close=close_data$DAL.Close/as.numeric(normaliced_price[1,1])
normaliced_price$NFLX.Close=close_data$NFLX.Close/as.numeric(normaliced_price[1,2])
normaliced_price$CVNA.Close=close_data$CVNA.Close/as.numeric(normaliced_price[1,3])
normaliced_price$AAL.Close=close_data$AAL.Close/as.numeric(normaliced_price[1,4])

head(normaliced_price)
tiempo=index(normaliced_price)
plot(x=tiempo,
     y=normaliced_price$DAL.Close,
     ylim = c(0, 11.5),
     xlab="Fecha",
     ylab="Comportamiento inversión",
     type='l',
     main="David Felipe Zabala Castañeda"
)
lines(x=tiempo, y=normaliced_price$NFLX.Close, col="blue")
lines(x=tiempo, y=normaliced_price$CVNA.Close, col="red")
lines(x=tiempo, y=normaliced_price$AAL.Close, col="darkgreen")
grid(col = "gray")
legend("topleft",
       c("DLA", "NFLX", "CVNA", "AAL"),
       lwd = c(1, 1, 1, 1),
       col = c("black", "blue", "red", "darkgreen"))
