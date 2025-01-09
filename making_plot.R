# source("import_dados_sheets.R") Arrumar script primeiro

jpeg(filename = "pipaes_co2.jpg",
      width = 800, height = 600)
par(mfrow = c(1,2), bty ="n", bg = "grey99" )

plot (CO2 ~ DateTime, 
      data=pipae7_days_20_31, type = "n", 
      xlab="Days", 
      ylab = "CO\u2082 ppm", xaxt="n", 
      ylim= c(250, 450))

axis.POSIXct(1, 
             at =seq(min(pipae7_days_20_31$DateTime), 
                     max(pipae7_days_20_31$DateTime), 
                     by = "day"), 
             format = "%D", 
             las = 1)

lines(CO2 ~ DateTime, data=pipae7_days_20_31, 
      lty=1, lwd = 2, col = "red")
lines(CO2 ~ DateTime, data=pipae2_days_20_31, 
      lty=1, lwd = 2, col = "blue")
lines(CO2 ~ DateTime, data=pipae1_days_20_31, 
      lty=1, lwd=2, col = "purple")

legend("topright", c("Pipae1","Pipae2","Pipae7")
       , lty= 1,
       col =c("purple", "blue", "red"), bty ="n")

plot (mean_co2 ~ D, 
      data=mean_pipae1_co2, type = "n", 
      xlab="Days", 
      ylab = "mean CO\u2082 ppm", 
      ylim= c(250, 500))

lines(mean_co2 ~ D, data=mean_pipae7_co2, 
      lty=1, lwd = 2, col = "red")
lines(mean_co2 ~ D, data=mean_pipae2_co2, 
      lty=1, lwd = 2, col = "blue")
lines(mean_co2 ~ D, data=mean_pipae1_co2, 
      lty=1, lwd=2, col = "purple")

legend("topright", c("Pipae1","Pipae2","Pipae7")
       , lty= 1,
       col =c("purple", "blue", "red"), bty ="n")


dev.off ()

jpeg(filename = "pipaes_temp.jpg",
     width = 800, height = 600)
par(mfrow = c(1,2), bty ="n", bg = "grey99" )

plot (Temperature ~ DateTime, 
      data=pipae7_days_20_31_temp, type = "n", 
      xlab="Days", 
      ylab = "Temprature ºC", xaxt="n", 
      ylim= c(15, 45))

axis.POSIXct(1, 
             at =seq(min(pipae7_days_20_31_temp$DateTime), 
                     max(pipae7_days_20_31_temp$DateTime), 
                     by = "day"), 
             format = "%D", 
             las = 1)


lines(Temperature ~ DateTime, data=pipae7_days_20_31_temp, 
      lty=1, lwd = 2, col = "red")
lines(Temperature ~ DateTime, data=pipae2_days_20_31_temp, 
      lty=1, lwd = 2, col = "blue")
lines(Temperature ~ DateTime, data=pipae1_days_20_31_temp, 
      lty=1, lwd=2, col = "purple")

legend("topright", c("Pipae1","Pipae2","Pipae7")
       , lty= 1,
       col =c("purple", "blue", "red"), bty ="n")


plot (mean_temp ~ D, 
      data=mean_pipae7_temp, type = "n", 
      xlab="Days", 
      ylab = "mean temperature ºC", 
      ylim = c(10,40))

lines(mean_temp ~ D, data=mean_pipae7_temp, 
      lty=1, lwd = 2, col = "red")
lines(mean_temp ~ D, data=mean_pipae2_temp, 
      lty=1, lwd = 2, col = "blue")
lines(mean_temp ~ D, data=mean_pipae1_temp, 
      lty=1, lwd=2, col = "purple")

legend("topright", c("Pipae1","Pipae2","Pipae7")
       , lty= 1,
       col =c("purple", "blue", "red"), bty ="n")



dev.off()


jpeg(filename = "pipaes_hum.jpg",
     width = 800, height = 600)
par(mfrow = c(1,2), bty ="n", bg = "grey99" )

plot (Humidity ~ DateTime, 
      data=pipae7_days_20_31_hum, type = "n", 
      xlab="Days", 
      ylab = "Moisture %", xaxt="n")

axis.POSIXct(1, 
             at =seq(min(pipae7_days_20_31_hum$DateTime), 
                     max(pipae7_days_20_31_hum$DateTime), 
                     by = "day"), 
             format = "%D", 
             las = 1)


lines(Humidity ~ DateTime, data=pipae7_days_20_31_hum, 
      lty=1, lwd = 2, col = "red")
lines(Humidity ~ DateTime, data=pipae2_days_20_31_hum, 
      lty=1, lwd = 2, col = "blue")
lines(Humidity ~ DateTime, data=pipae1_days_20_31_hum, 
      lty=1, lwd=2, col = "purple")

legend("bottomright", c("Pipae1","Pipae2","Pipae7")
       , lty= 1,
       col =c("purple", "blue", "red"), bty ="n")


plot (mean_hum ~ D, 
      data=mean_pipae7_umi, type = "n", 
      xlab="Days", 
      ylab = "mean moisture  %", 
      ylim= c(70, 100))

lines(mean_hum ~ D, data=mean_pipae7_umi, 
      lty=1, lwd = 2, col = "red")
lines(mean_hum ~ D, data=mean_pipae2_umi, 
      lty=1, lwd = 2, col = "blue")
lines(mean_hum ~ D, data=mean_pipae1_umi, 
      lty=1, lwd=2, col = "purple")

legend("bottomright", c("Pipae1","Pipae2","Pipae7")
       , lty= 1,
       col =c("purple", "blue", "red"), bty ="n")



dev.off()