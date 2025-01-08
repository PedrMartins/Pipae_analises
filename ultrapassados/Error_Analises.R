########################################################
############Read google sheets data into R##############

pi_raw7 <- read_sheet("https://docs.google.com/spreadsheets/d/12JiclFJ9luvW9JPee77TJh1zCGnaPTcfv_RN6pl7hRk/edit?usp=sharing")


pi_raw3 <- read_sheet("https://docs.google.com/spreadsheets/d/1phwbkcWpC61vraF0FrI5hI7Q9098EAz1LRYmzf5lUhg/edit?usp=sharing")

pi_raw2 <- read_sheet("https://docs.google.com/spreadsheets/d/1ZUQM3cBFW3cFwtEljVy1w7bbDDyWC0evN_AgbfR_7Bg/edit?usp=sharing")	

#pi_raw1 <- read_sheet("<falta link>")	

########################################################
#####################Tratando dados####################

############ pipae7  ##########
pi_d7 <- pi_raw7
#head (pi_d3)
pi_d7 <- pi_d7 |> 
  mutate(H=hour(pi_d7$Hora),
         D=day(pi_d7$Data),
         M= month(pi_d7$Data),
         Y= year(pi_d7$Data))
##

#####pegando dados com erro#######
error <- pi_d7  [pi_d7$CO2 >= 1000 & pi_d7$CO2 >= 100,] 
error |>
  head ()
error |>
  tail()
error |>
  View()


error |> 
  group_by(M) |>
  View()
##

############Plotando erros ##################
pi_er13 = pi_d7 [pi_d7$D == 13 & pi_d7$M == 6 & pi_d7$Y == 2024,]

par(mfrow = c(1,2), bty ="n", bg = "grey99" )
plot (CO2~Hora, type="n"
      , data=pi_er13, ylim = NULL,
      main = "pipae 7 \n 13/06", xaxt="n")
points(CO2~Hora, data=pi_er13,
       col= rgb(0,0,0,0.1), pch=20,
       cex=1.7 )
hora <- pi_er13 [pi_er13$H == 1,]
date_line <- hora$Hora[100]
abline(v = date_line, col = "red", lwd = 2, lty = 2)  # Red dashed line
Sys.setenv(TZ = "UTC")
axis.POSIXct(1, at =seq(min(pi_d7$Hora), 
                        max(pi_d7$Hora), 
                        by = "hour"), 
             format = "%H", las = 1)
legend("topleft", legend = "fim da bateria", 
       lwd =2, col = "red" ,lty=2, bty ="n")



plot (CO2~Hora, type="n"
      , data=pi_er13, ylim = c(400, 500),
      main = "pipae 7 \n 13/06", xaxt="n")
points(CO2~Hora, data=pi_er13,
       col= rgb(0,0,0,0.1), pch=20,
       cex=1.7 )
hora = pi_er13 [pi_er13$H == 1,]
date_line <- hora$Hora[100]
abline(v = date_line, col = "red", lwd = 2, lty = 2)  # Red dashed line
Sys.setenv(TZ = "UTC")
axis.POSIXct(1, at =seq(min(pi_d7$Hora), 
                        max(pi_d7$Hora), 
                        by = "hour"), 
             format = "%H", las = 1)
legend("topleft", legend = "fim da bateria", 
       lwd =2, col = "red" ,lty=2, bty ="n")


pi_er18 = pi_d7 [pi_d7$D == 18& pi_d7$M == 6 & pi_d7$Y == 2024,]

par(mfrow = c(1,2), bty ="l", bg = "grey99" )
plot (CO2~Hora, type="n"
      , data=pi_er18, ylim = NULL,
      main = "pipae 7 \n 18/06", xaxt="n")
points(CO2~Hora, data=pi_er18,
       col= rgb(0,0,0,0.1), pch=20,
       cex=1.7 )
hora = pi_er18 [pi_er18$H == 19,]
date_line <- hora$Hora[100]
abline(v = date_line, col = "red", lwd = 2, lty = 2)  # Red dashed line
Sys.setenv(TZ = "UTC")
axis.POSIXct(1, at =seq(min(pi_d7$Hora), 
                        max(pi_d7$Hora), 
                        by = "hour"), 
             format = "%H", las = 1)
legend("topleft", legend = "fim da bateria", 
       lwd =2, col = "red" ,lty=2, bty ="n")

plot (CO2~Hora, type="n"
      , data=pi_er18, ylim = c(400, 800),
      main = "pipae 7 \n 18/06", xaxt="n")
points(CO2~Hora, data=pi_er18,
       col= rgb(0,0,0,0.1), pch=20,
       cex=1.7 )
hora = pi_er18 [pi_er18$H == 19,]
date_line <- hora$Hora[100]
abline(v = date_line, col = "red", lwd = 2, lty = 2)  # Red dashed line
Sys.setenv(TZ = "UTC")
axis.POSIXct(1, at =seq(min(pi_d7$Hora), 
                        max(pi_d7$Hora), 
                        by = "hour"), 
             format = "%H", las = 1)
legend("topleft", legend = "fim da bateria", 
       lwd =2, col = "red" ,lty=2, bty ="n")



pi_er19 = pi_d7 [pi_d7$D == 19& pi_d7$M == 6 & pi_d7$Y == 2024,]

par(mfrow = c(1,2), bty ="n", bg = "grey99" )
plot (CO2~Hora, type="n"
      , data=pi_er19, ylim = NULL,
      main = "pipae 7 \n 19/06", xaxt="n")
points(CO2~Hora, data=pi_er19,
       col= rgb(0,0,0,0.1), pch=20,
       cex=1.7 )
hora = pi_er19 [pi_er19$H == 19,]
date_line <- hora$Hora[100]
abline(v = date_line, col = "red", lwd = 2, lty = 2)  # Red dashed line
Sys.setenv(TZ = "UTC")
axis.POSIXct(1, at =seq(min(pi_d7$Hora), 
                        max(pi_d7$Hora), 
                        by = "hour"), 
             format = "%H", las = 1)
legend("topleft", legend = "fim da bateria", 
       lwd =2, col = "red" ,lty=2, bty ="n")

plot (CO2~Hora, type="n"
      , data=pi_er19, ylim = c(400, 800),
      main = "pipae 7 \n 19/06", xaxt="n")
points(CO2~Hora, data=pi_er19,
       col= rgb(0,0,0,0.1), pch=20,
       cex=1.7 )
hora = pi_er19 [pi_er19$H == 18,]
date_line <- hora$Hora[350]
abline(v = date_line, col = "red", lwd = 2, lty = 2)  # Red dashed line
Sys.setenv(TZ = "UTC")
axis.POSIXct(1, at =seq(min(pi_d7$Hora), 
                        max(pi_d7$Hora), 
                        by = "hour"), 
             format = "%H", las = 1)
legend("topleft", legend = "fim da bateria", 
       lwd =2, col = "red" ,lty=2, bty ="n")


pi_er20 = pi_d7 [pi_d7$D == 20& pi_d7$M == 6 & pi_d7$Y == 2024,]

par(mfrow = c(1,2), bty ="n", bg = "grey99" )
plot (CO2~Hora, type="n"
      , data=pi_er20, ylim = NULL,
      main = "pipae 7 \n 20/06", xaxt="n")
points(CO2~Hora, data=pi_er20,
       col= rgb(0,0,0,0.1), pch=20,
       cex=1.7 )
hora = pi_er20 [pi_er20$H == 14,]
date_line <- hora$Hora[130]
abline(v = date_line, col = "red", lwd = 2, lty = 2)  # Red dashed line
Sys.setenv(TZ = "UTC")
axis.POSIXct(1, at =seq(min(pi_d7$Hora), 
                        max(pi_d7$Hora), 
                        by = "hour"), 
             format = "%H", las = 1)
legend("topleft", legend = "fim da bateria", 
       lwd =2, col = "red" ,lty=2, bty ="n")

plot (CO2~Hora, type="n", data=pi_er20, ylim = c(400, 600),
      main = "pipae 7 \n 20/06", xaxt="n")
points(CO2~Hora, data=pi_er20,
       col= rgb(0,0,0,0.1), pch=20,
       cex=1.7 )
hora = pi_er20 [pi_er20$H == 14,]
date_line <- hora$Hora[130]
abline(v = date_line, col = "red", lwd = 2, lty = 2)  # Red dashed line
Sys.setenv(TZ = "UTC")
axis.POSIXct(1, at =seq(min(pi_d7$Hora), 
                        max(pi_d7$Hora), 
                        by = "hour"), 
             format = "%H", las = 1)
legend("topleft", legend = "fim da bateria", 
       lwd =2, col = "red" ,lty=2, bty ="n")

dev.off()


######################################################