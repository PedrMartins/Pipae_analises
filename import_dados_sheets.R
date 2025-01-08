#### carregando pacotes ####

source("Loadpackges.R")
source("Function_tratamento_pipae.R")

# Visualizar dados
########################################################
#####################Tratando dados####################


#######Analise Exploratória####### 

############ pipae7 Tidy  ##########
pipae7 <-import_pipae ("pipae7")

#pipae8 <-  import_pipae ("pipae8")

pipae2 <-  import_pipae ("pipae2")

pipae1 <-  import_pipae ("pipae1")



##

pipae1_co2 = separate_variable(pipae1, 
                               var= "co2",
                               na.rm = TRUE, 
                               duplicated = TRUE)

pipae2_co2 = separate_variable(pipae2, 
                               var= "co2",
                               na.rm = TRUE, 
                               duplicated = TRUE)

pipae7_co2 = separate_variable(pipae7, 
                               var= "co2",
                               na.rm = TRUE, 
                               duplicated = TRUE)

#pipae8_dia26$CO2 <- c(pipae8_dia26$CO2 +200)
pipae1_days_20_31 <- get_data_by_month(pipae1_co2,
                                          month=12, 
                                          days= c(20:31), 
                                          order = TRUE)

pipae2_days_20_31 <- get_data_by_month(pipae2_co2,
                                       month=12, 
                                       days= c(20:31), 
                                       order = TRUE)

pipae7_days_20_31 <- get_data_by_month(pipae7_co2,
                                       month=12, 
                                       days= c(20:31), 
                                       order = TRUE)


#write.table(pipae1_days_20_31, "pipae1_december_20_31.csv",
#          sep = "\t", row.names = FALSE, dec= ",")



colnames(pipae1_days_20_31)[1] <- "CO2"
colnames(pipae2_days_20_31)[1] <- "CO2"
colnames(pipae7_days_20_31)[1] <- "CO2"


plot (CO2 ~ DateTime, 
      data=pipae7_dia27, type = "p", 
      xlab="Hour", 
      ylab = "CO\u2082", xaxt="n",
      main="Pipae7 dia 27\nburacos obstruídos",
      ylim = c(320,420))

axis.POSIXct(1, 
             at =seq(min(pipae7_dia27$DateTime), 
                     max(pipae7_dia27$DateTime), 
                     by = "hour"), 
             format = "%H", 
             las = 1)


plot (CO2 ~ DateTime, 
      data=pipae8_dia26, type = "p", 
      xlab="Hour", 
      ylab = "CO\u2082", xaxt="n",
      main="pipae8 dia 26\nburacos desobstruídos",
      ylim = c(100,160))


axis.POSIXct(1, 
             at =seq(min(pipae8_dia26$DateTime), 
                     max(pipae8_dia26$DateTime), 
                     by = "hour"), 
             format = "%H", 
             las = 1)

plot (CO2 ~ DateTime, 
      data=pipae8_dia27, type = "p", 
      xlab="Hour", 
      ylab = "CO\u2082", xaxt="n",
      main="pipae8 dia 27\nburacos desobstruídos",
      ylim = c(100,200))


axis.POSIXct(1, 
             at =seq(min(pipae8_dia27$DateTime), 
                     max(pipae8_dia27$DateTime), 
                     by = "hour"), 
             format = "%H", 
             las = 1)


piape7_26_27 <- rbind(pipae7_dia26,pipae7_dia27)
piape8_26_27 <- rbind(pipae8_dia26, pipae8_dia27)

length (piape7_26_27$CO2)
piape7_26_27 <- piape7_26_27 [!duplicated(piape7_26_27$DateTime),]
length(piape8_26_27$CO2)
piape8_26_27 <- piape8_26_27 [!duplicated(piape8_26_27$DateTime),]

piape7_26_27 <- piape7_26_27 [-length(piape7_26_27$CO2),]

par (mfrow=c(1,2))
boxplot(piape7_26_27$CO2)
boxplot(piape8_26_27$CO2)

sd (piape7_26_27$CO2)
sd (piape8_26_27$CO2)

anova (piape7_26_27$CO2
       , piape8_26_27$CO2)

# Gráfico de Bland-Altman
dev.off()
a=bland.altman.plot(piape7_26_27$CO2, piape8_26_27$CO2, 
                  main = "Gráfico de Bland-Altman", 
                  graph.sys = "base", silent =FALSE )


concordância_Pipae7_e_8 <-  data.frame(data=seq(1:46))
concordância_Pipae7_e_8$mean<- a$means
concordância_Pipae7_e_8$diff<- a$diffs
concordância_Pipae7_e_8$pipae7<- a$groups[1]
concordância_Pipae7_e_8$pipae8<- a$groups[2]


head (concordância_Pipae7_e_8)


# Calcular o CCC
CCC(piape7_26_27$CO2, piape8_26_27$CO2)$rho.c


############################################
#################Análises ####################

############Plotando pipae 7 ##################
#pi_d7$D_M <- paste(pi_d7$D, pi_d7$M, sep = "-") 
#grou= unique(pi_d7$D_M)


head (pipae2)
get_mean_by_day(pipae7
                ,variavel ="co2",
                day =26 ,
                month = 11, year =2024)



l_p=lm (medtem~medco2, data= med_pd)
par(mfrow = c(1,1), bty ="n", bg = "grey99" )
plot (CO2 ~ Hora, data=pi_d7, ylim= c(300,800), type = "n", xaxt="n", xlab="Dia")
lines (medc~Hora, data= med_pd7, lty= 3, col="blue",lwd=4)
lines (CO2 ~ Hora, data=pi_dd1, lwd =2, col= rgb(1,0,0,0.5) )
lines (CO2 ~ Hora, data=pi_dd2, lwd =2, col= rgb(0,0,1,0.5) )
lines (CO2 ~ Hora, data=pi_dd3, lwd =2, col= rgb(0,1,0,0.5) )

Sys.setenv(TZ = "UTC")
axis.POSIXct(1, at =seq(min(pi_d7$Data), 
                        max(pi_d7$Data), 
                        by = "day"), 
             format = "%H", las = 1) 

#strptime


legend ("topright",legend=c(1:3), col= c(rgb(1,0,0,0.5),rgb(0,0,1,0.5),rgb(0,1,0,0.5))
        , lty=1, bty = "n")



dev.off()

head (pi_d7)



####
##


###################################################

pi_n7= pi_d7 [pi_d7$D == 04 & pi_d7$M == 6 & pi_d7$Y == 2024,]
pi_n7 |>
  head() 

pi_n7$Hora <- with_tz(pi_n7$Hora, tzone = "America/Sao_Paulo")
pi_n7 = pi_n7 [pi_n7$CO2 <= 650,]
pi_n7 = pi_n7 [pi_n7$CO2 >= 400,]

head (pi_n7) [,c(7:10)]

tail (pi_n7)|>
  View ()

dev.off()
par (mfrow = c(1,2))
plot (CO2~Hora, type="n"
      , data=pi_n7, ylim = NULL,
      #main = "sexta 21/06 \n pipae 3",
      xaxt ="n")

axis.POSIXct(1, at = seq(min(pi_n7$Hora), 
                         max(pi_n7$Hora), 
                         by = "hour"), 
             format = "%H", las = 1,
             tz="America/Sao_Paulo")

points(CO2~Hora, data=pi_n7,
       col= rgb(0,0,0,0.1), pch=20,
       cex=1.7 )

plot (CO2~Hora, type ="p"
      , data=pi_n7, 
      main= "pipae7", bty = "l", 
      col = rgb(0,0,0,0.15), pch = 19)

#lines (CO2~Hora, data =pi_n7)

plot (CO2~Hora, type ="p"
      , data=pi_n7, ylim = c(440, 550), 
      main="pipae 7 \n 450-800 ppm", bty = "l")

#lines (CO2~Hora, data =pi_n7)

dev.off()
#colnames(pi_d3) <- c("co2","tem","U","P")

par (mfrow = c(1,3))
plot (Temperatura~Hora
      , data=pi_n7, ylim = NULL)

plot (Umidade~Hora
      , data=pi_n7, ylim = NULL)


plot (Pressão~Hora
      , data=pi_n7, ylim = NULL)
dev.off()




############ pipae3##########
pi_d3 <- pi_raw3 
pi_d3 <- pi_d3 |> 
  mutate(H= hour(pi_d3$Hora),
         D=day(pi_d3$Data),
         M= month(pi_d3$Data),
         Y= year(pi_d3$Data))

pi_d3 |>
 tail (100)|> 
  View()

#pi_d3 = pi_d3 [pi_d3$M == 6 
#                  & pi_d3$Y == 2024 ,]
#
#p= pi_d3 [!duplicated (pi_d3$H),]
#plot (CO2~D, data= p, type="l") 
# para plotar assim precisa da média 


pi_n3_d5 = pi_d3 [pi_d3$D == 6
                  & pi_d3$M == 7
                  & pi_d3$Y == 2024 ,]

pi_m5 = pi_n3_d5 %>% 
  group_by (H) %>% 
  summarise(med_CO2=mean(CO2, na.rm = TRUE))
pi_m5=pi_m5|>
  left_join(pi_n3_d5,pi_m5, by ="H")
pi_m5 <- pi_m5 [!duplicated(pi_m5$H),]
#pi2_mt = pi2_mt [,-c(3:8)]

#########
pi_n3 = pi_n3 [pi_n3$H == c(14:16) ,]
pi_n3 = pi_n3 [pi_n3$CO2 >= 390,]
range (pi_n3$CO2, na.rm=TRUE)



range (pi_n3_d5$CO2)
#plot a day
plot (CO2~Hora, type="n"
      , data=pi_n3_d5, ylim = NULL, 
      main= "", xaxt= "n", bty="n")
points(CO2~Hora, data=pi_n3_d5, col= rgb(0,0,0,0.1), pch= 20,
       cex=1.2)
lines (CO2~Hora, data = pi_n3_d5, lty = 3, ltw=4, col ="gray40")
#lines(med_CO2~Hora, data=pi_m5,
#       col= "darkred", lty= 3 )
Sys.setenv(TZ = "UTC")
axis.POSIXct(1, at =seq(min(pi_n3_d5$Hora), 
                        max(pi_n3_d5$Hora), 
                        by = "hour"), 
             format = "%H", las = 1) 

#anotar tirada do carregador
#carregador tirado as 12:40
#coloquei o carregador 16:40
#original_locale <- Sys.getlocale("LC_TIME") #para manter a localização original
#Sys.setlocale("LC_TIME", original_locale)
#Sys.setlocale("LC_TIME", "fr_FR.UTF-8") 
#muda a localização do sistemaError in `read_cells_impl_()
##############
dev.off()

############ pipae2##########
pi_d2 <- pi_raw2
pi_d2 <- pi_d2 |> 
  mutate(H=hour(pi_d2$Hora),
         D=day(pi_d2$Data),
         M= month(pi_d2$Data),
         Y= year(pi_d2$Data))

pipae2_médias_co2 = pi_d2 |>
  group_by(D)|>
  summarize (medco2=mean(CO2, na.rm=TRUE))

unique (pi_d2$Data)

pipae2_médias_co2=pipae2_médias_co2|>
  left_join(pi_d2,pipae2_médias_co2, by ="D")

pipae2_médias_co2=pipae2_médias_co2 [!duplicated(pipae2_médias_co2$D),]

pipae2_dia = pi_d2 [pi_d2$D == 2 & pi_d2$M == 7,]
#dia 29/06 problema sensores pipae 2
#dia 02/07 teste floresta Pipae
plot(Temperatura ~ Hora, data=pipae2_dia)

#, xaxt="n" retira dados escala eixo x
par (mfrow= c(1,1), xaxt="n")
plot (medco2~Data
      , data=pipae2_médias_co2, pch = "*", cex=2)
lines (medco2~Data
       , data=pipae2_médias_co2, lty=5)

axis(side = 3, at = 12, 
     labels="24h",
     las=1 ) 

dev.off()

############Estrutura gráfica########################

par (mar=c(5,5,2,2),bty = "l", las= 1,	
     cex.lab=1.4, cex =1.2, tcl=0.2)	

plot (medco2~Data ,type="n", data= pipae2_médias_co2)
lines (medco2~Data, data = pipae2_médias_co2) #plota linhas 
abline (l_p) #plot modelo linear
c_p=cor(as.data.frame(med_p[c(3:5)]))
corrplot (c_p)    

plot (medco2~H, data=med_ph, type="p")
lines (medco2~H, data=med_ph) #plota linhas 
lines(medtem~H, data=med_ph)
lines(medpre~H, data=med_ph)

plot (CO2~Temperatura, type="n",data=pi_d)

points(CO2~Temperatura,
       col="gray70" ,pch =19,
       data=pi_d)


plot (medco2~medtem ,data=med_ph)

#######################################################

x <- y <- ymd_hms("2012-03-26 10:10:00", tz = "America/Sao_Paulo")
tz(x)

tz(y) <- "America/Sao_Paulo"

x-y

####################PIPAE 1##################################


pi_d1 <- pi_raw1 
pi_d1 <- pi_d1 |> 
  mutate(H= hour(pi_d1$Hora),
         D=day(pi_d1$Data),
         M= month(pi_d1$Data),
         Y= year(pi_d1$Data))

pi_d1 %>% 
  head ()
pi_d1 %>% 
  tail ()

######### remove ALL ##############

obj<- c(ls ()) 


obj   

ls.str()
rm (list= obj )
dev.off()


Sys.setenv(TZ = "UTC")

#he Sys.setenv(TZ = "UTC") command 
#sets the timed_ph = pi_d |>

summarize_by_time (.date_var=Hora,
                   by="hours", 
                   medco2=mean(CO2),
                   medtem=mean(Temperatura),
                   medpre=mean(Pressão)
)

#med_phme zone to UTC for the current R session. 
#This affects how dates and times are displayed and processed.

#Dividir os dados, pode ser importante colocar médias diárias (vai reduzir os dados)
#dividir em dia e noite pode ser uma boa idéia
#limpar outliers, mas devo ver os dados primeiro / preciso fazer uma análise exploratória




##############################################
#Média ṕor dia
med_p = pi_d |>
  summarize_by_time (.date_var=D_M_A,
                     by="day", 
                     medco2=mean(CO2),
                     medtem=mean(Temperatura),
                     medpre=mean(Pressão)
  )

l_p=lm (mco2_p$med~mco2_p$D_M_A)


###Estrutura gráfica

plot (medtem~D_M_A ,type="n", data= med_p)
lines (medtem~D_M_A, data = med_p) #plota linhas 
abline (l_p) #plot modelo linear
med_p <- med_p [-15,]
c_p=cor(as.data.frame(med_p[c(3:5)]))
corrplot (c_p)    

##############################################

##############################################
##############################################


#tem que fazer a média por hora <--


med_ph = pi_d |>
  summarize_by_time (.date_var=Hora,
                     by="hours", 
                     medco2=mean(CO2),
                     medtem=mean(Temperatura),
                     medpre=mean(Pressão)
  )
med_ph

plot (medco2~medtem ,data=med_ph)