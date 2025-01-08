# source("import_dados_sheets.R") Arrumar script primeiro
x11()

par(mfrow = c(1,1), bty ="n", bg = "grey99" )

plot (CO2 ~ DateTime, 
      data=pipae7_days_20_31, type = "n", 
      xlab="Days", 
      ylab = "CO\u2082 ppm", xaxt="n", 
      ylim= c(0, 600))

axis.POSIXct(1, 
             at =seq(min(pipae7_days_20_31$DateTime), 
                     max(pipae7_days_20_31$DateTime), 
                     by = "day"), 
             format = "%D", 
             las = 1)

#lines(media_temperatura~nivel,
#      data=pipae_mediatemperatura, lty = 5,
#      lwd = 4, col = "darkorange")

colnames(pipae7_days_20_31)

lines(CO2 ~ DateTime, data=pipae7_days_20_31, 
      lty=2, lwd = 4)
lines(CO2 ~ Date, data=pipae2_days_20_31, 
      lty=2, lwd = 4)
lines(CO2 ~ H, data=pipae1_days_20_31, lty=2)

dev.off ()
