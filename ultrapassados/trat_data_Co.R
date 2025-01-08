C_bi3 <- camCo2_bio3_raw
colnames (C_bi3) <- c("N_data","Time","co2")
C_bi3$N_data <- str_replace_all(C_bi3$N_data, "\\*", "")  #Seg arg ver operadores regex retirar "*"
C_bi3$N_data <- str_replace_all(C_bi3$N_data, "@", "")
C_bi3$N_data <- str_replace_all(C_bi3$N_data, "X", "")#Seg arg ver operadores regex retirar "*"
C_bi3$N_data <- str_replace_all(C_bi3$N_data, "-", "")  #Seg arg ver operadores regex 
C_bi3$N_data <- str_replace_all(C_bi3$N_data, "\\s+", " ")#Seg arg ver operadores regex 
C_bi3$N_data <- rm_sp_if_n(C_bi3$N_data)
C_bi3$Time [is.na(C_bi3$Time)] <- 0
C_bi3<-na.omit(C_bi3)
C_bi3=C_bi3 |>
separate(col=N_data, into= c(
  paste(rep("M", 6), seq (1,6), sep="_")
),
sep=" ") 
C_bi3 <- C_bi3 [-c(1,2,4:6)]
colnames(C_bi3) <- c("DMY","HMS","co2")
Sys.setenv(TZ = "UTC")
C_bi3$DMY<- dmy (C_bi3$DMY )
C_bi3 <- C_bi3|> 
  mutate(
    H= hour(C_bi3$HMS),
    D= day(C_bi3$DMY), 
    M= month(C_bi3$DMY))
