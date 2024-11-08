
source ("import_dados_sheets.R")
source("Dados bio.R")




###########Comparação Temperatura###############

pi_d1$D_M = paste (pi_d1$D, pi_d1$M, sep= "-") 
t_bi3$D_M = paste (t_bi3$D, t_bi3$M, sep= "-")
bio_t= t_bi3 
pi_temp=pi_d1 
pi_dtem3 <- pi_d3
pi_temp$tag <- c(rep("pulga1",length(pi_temp$CO2)))
pi_dtem3$tag <- c(rep("pulga3",length(pi_dtem3$CO2)))
bio_t$tag <- c(rep("cam", length(bio_t$DMY)))
pi_temp = pi_temp[,-c(1,3:13)]
pi_dtem3 = pi_dtem3[,-c(1,3:12)]
bio_t = bio_t[,-c(1,2,4:8)]
colnames(pi_temp) <- c("tem","tag")
colnames(pi_dtem3) <- c("tem","tag")
colnames(bio_t) <- c("tem","tag")
b_p_tem= rbind (pi_dtem3,pi_temp,bio_t)

##

tbi_pi1 = t_bi3 [ t_bi3$D_M %in% pi_d1$D_M,]
tpi1_bi = pi_d1 [ pi_d1$D_M %in% t_bi3$D_M,]
tbi_pi1$tag <- c(rep("cam",length(tbi_pi1$temp)))
tpi1_bi$tag <- c(rep("pulga1",length(tpi1_bi$CO2)))
tbi_pi1 = tbi_pi1[,-c(1,2,4:8)]
tpi1_bi = tpi1_bi[,-c(1,3:13)]
colnames(tbi_pi1) <- c("tem","tag")
colnames(tpi1_bi) <- c("tem","tag")
b_pb_tem = rbind (tbi_pi1,tpi1_bi)
tail(b_pb_tem)
head(b_pb_tem)
###########Comparação Umidade##############

ur_bi3$D_M = paste (ur_bi3$D, ur_bi3$M, sep= "-")
pi_du3 <- pi_d3
bio_u= ur_bi3 
pi_du1 <- pi_d1
pi_du3 <- pi_d3
pi_du1$tag <- c(rep("pulga1",length(pi_du1$CO2)))
pi_du3$tag <- c(rep("pulga3",length(pi_du3$CO2)))
bio_u$tag <- c(rep("cam", length(bio_u$DMY)))
pi_du1 = pi_du1[,-c(1:2,4:13)]
pi_du3 = pi_du3[,-c(1:2,4:12)]
bio_u = bio_u[,-c(1,2,4:8)]
colnames(pi_du1) <- c("umi","tag")
colnames(bio_u) <- c("umi","tag")
colnames(pi_du3) <- c("umi","tag")
b_p_umi= rbind (pi_du3,pi_du1,bio_u)

##

bi_pi1 = ur_bi3 [ur_bi3$D_M %in% pi_d1$D_M,]
pi1_bi = pi_d1 [pi_d1$D_M %in% ur_bi3$D_M,]
bi_pi1$tag <- c(rep("cam",length(bi_pi1$D_M)))
pi1_bi$tag <- c(rep("pulga1",length(pi1_bi$CO2)))
bi_pi1 = bi_pi1[,-c(1:2,4:8)]
pi1_bi = pi1_bi[,-c(1:2,4:13)]
colnames(bi_pi1) <- c("umi","tag")
colnames(pi1_bi) <- c("umi","tag")
b_pb_umi= rbind (bi_pi1,pi1_bi)
tail(b_pb_umi)
head(b_pb_umi)

#####

###############todos os dados #############

par(mfrow = c(1,2), bty ="n", bg = "grey99", las =1)
cols <- colorRampPalette (c("yellowgreen","lightblue"), alpha=TRUE)


boxplot (tem ~tag, data = b_p_tem, ylab="Temp ºC", xlab="Sensores",
         col = cols (3),range = 0, main="Sensores bio \n X \n Sensores pulga", 
         xaxt="n")
mtext(c("Câmera \n Sbio", "Pulga \n Pipae1", "Pulga \n Pipae3"), 
      side = 1,at =c(1,2,3), line=1)




boxplot (umi ~tag, data = b_p_umi, ylab="Umi %", xlab="Sensores",
         col = cols (3),range = 0, main="Sensores bio \n X \n Sensores pulga", 
         xaxt="n")
mtext(c("Câmera \n Sbio", "Pulga \n Pipae1", "Pulga \n Pipae3"), 
      side = 1,at =c(1,2,3), line=1)


###############pipae 1 na bio#############

par(mfrow = c(1,2), bty ="n", bg = "grey99", las =1)
cols <- colorRampPalette (c("yellowgreen","lightblue"), alpha=TRUE)

boxplot (tem ~tag, data = b_pb_tem, ylab="Temp ºC", xlab="Sensores",
         col = cols (2),range = 0, main="Sensores bio \n X \n Sensores pulga", xaxt="n")
mtext(c("Câmera \n Sbio", "Pulga \n Pipae1"), 
      side = 1,at =c(1,2), line=1)



boxplot (umi ~tag, data = b_pb_umi, ylab="Umi %", xlab="Sensores",
         col = cols (2),range = 0, main="Sensores bio \n X \n Sensores pulga", xaxt="n")
mtext(c("Câmera \n Sbio", "Pulga \n Pipae1"), 
      side = 1,at =c(1,2), line=1)


dev.off ()

r=lm (tem ~tag, data = b_p_tem)
anova(r)

r=lm (umi ~tag, data = b_pb_umi)
anova(r)






all_colors <- colors()
str (all_colors) 

######

############################linhas###########


##temperatura
c= t_bi3 [t_bi3$M== 8,]
c2 = pi_d1 [pi_d1$D_M %in% c$D_M,]
c2 = c2 [- c(4:7)]
medc = c %>% 
  group_by (D) %>% 
  summarise(med=mean(temp))
medc=medc|>
  left_join(c,medc, by ="D")
medc <- medc [!duplicated(medc$D),]

medc2 = c2 %>% 
  group_by (D) %>% 
  summarise(med=mean(Temperatura))
medc2=medc2|>
  left_join(c2,medc2, by ="D")
medc2 <- medc2 [!duplicated(medc2$D),]

medc %>% 
  head()
medc2 = medc2 [,-c(3,5:7,9)]
colnames(medc2) <- c("D","medt","temp","M", "D_M")

##umidade
cb= ur_bi3 [ur_bi3$M== 8,]
cb2 = pi_d1 [pi_d1$D_M %in% c$D_M,]
cb2 = cb2 [- c(4:7)]
medbc = cb %>% 
  group_by (D) %>% 
  summarise(med=mean(umi))
medbc=medbc|>
  left_join(cb,medc, by ="D")
medbc <- medbc [!duplicated(medbc$D),]

medbc2 = cb2 %>% 
  group_by (D) %>% 
  summarise(med=mean(Umidade))
medbc2=medbc2|>
  left_join(cb2,medbc2, by ="D")
medbc2 <- medbc2 [!duplicated(medbc2$D),]

medbc2 = medbc2 [,-c(3:4,6:7,9)]
colnames(medbc2) <- c("D","medu","umi","M", "D_M")



par(mfrow = c(1,2), bty ="n", bg = "grey99", las =1)
plot (med~D, data=medc, type="n", xlim =c(17,23),
      ylab="Temperatura média ºC", xlab= "Dias",
      main="Medições \n 08/2023")

lines(med~D, data=medc,
      lty = 2, lwd =2,
      col = "darkolivegreen4")

lines(medt ~D, data=medc2 ,
      lty = 1, lwd =2,
      col = "darkblue")

legend("topleft",c("Sensor Bio", "Pipae1"), 
       col= c( "darkolivegreen4","darkblue"),
       lty = c(2,1), bty = "n")

plot (med~D, data=medbc, type="n", xlim =c(17,23),
      ylim=c(30,90),ylab="Umidade média %", xlab= "Dias",
      main="Medições \n 08/2023")

lines(med~D, data=medbc,
      lty = 2, lwd =2,
      col = "darkolivegreen4")

lines(medu ~D, data=medbc2 ,
      lty = 1, lwd =2,
      col = "darkblue")

legend("topleft",c("Sensor Bio", "Pipae1"), 
       col= c( "darkolivegreen4","darkblue"),
       lty = c(2,1), bty = "n")

dev.off()
