ur_bi3 <- camU_bio3_raw
colnames (ur_bi3) <- c("N_data","Time","umi")
ur_bi3$N_data <- str_replace_all(ur_bi3$N_data, "\\*", "")  #Seg arg ver operadores regex retirar "*"
ur_bi3$N_data <- str_replace_all(ur_bi3$N_data, "@", "")
ur_bi3$N_data <- str_replace_all(ur_bi3$N_data, "X", "")#Seg arg ver operadores regex retirar "*"
ur_bi3$N_data <- str_replace_all(ur_bi3$N_data, "-", "")  #Seg arg ver operadores regex 
ur_bi3$N_data <- str_replace_all(ur_bi3$N_data, "\\s+", " ")#Seg arg ver operadores regex 
ur_bi3$N_data <- rm_sp_if_n(ur_bi3$N_data)
ur_bi3$Time [is.na(ur_bi3$Time)] <- 0
ur_bi3<-na.omit(ur_bi3)
ur_bi3=ur_bi3 |>
  separate(col=N_data, into= c(
    paste(rep("M", 6), seq (1,6), sep="_")
  ),
  sep=" ") 
ur_bi3 <- ur_bi3 [-c(1,2,5:7)]
colnames(ur_bi3) <- c("DMY","HMS","umi")
Sys.setenv(TZ = "UTC")
ur_bi3$DMY<- dmy (ur_bi3$DMY)
ur_bi3$HMS<- lubridate::hms (ur_bi3$HMS)
ur_bi3 <- ur_bi3|> 
  mutate(
    D= day(ur_bi3$DMY),
    M= month (ur_bi3$DMY),
    Y=year (ur_bi3$DMY),
    H= hour(ur_bi3$HMS))