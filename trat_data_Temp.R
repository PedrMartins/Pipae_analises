
t_bi3 <- camT_bio3_raw
colnames (t_bi3) <- c("N_data","Time","Temp")
t_bi3$N_data <- str_replace_all(t_bi3$N_data, "\\*", "")  #Seg arg ver operadores regex retirar "*"
t_bi3$N_data <- str_replace_all(t_bi3$N_data, "@", "")
t_bi3$N_data <- str_replace_all(t_bi3$N_data, "X", "")#Seg arg ver operadores regex retirar "*"
t_bi3$N_data <- str_replace_all(t_bi3$N_data, "-", "")  #Seg arg ver operadores regex 
t_bi3$N_data <- str_replace_all(t_bi3$N_data, "\\s+", " ")#Seg arg ver operadores regex 
t_bi3$N_data <- rm_sp_if_n(t_bi3$N_data)
t_bi3$Time [is.na(t_bi3$Time)] <- 0
t_bi3<-na.omit(t_bi3)
t_bi3=t_bi3 |>
  separate(col=N_data, into= c(
    paste(rep("M", 6), seq (1,6), sep="_")
  ),
  sep=" ") 
t_bi3 <- t_bi3 [-c(1,2,5:7)]
colnames(t_bi3) <- c("DMY","HMS","temp")
Sys.setenv(TZ = "UTC")
t_bi3$DMY<- dmy (t_bi3$DMY)
t_bi3$HMS<- lubridate::hms (t_bi3$HMS)
t_bi3 <- t_bi3|> 
  mutate(
        D= day(t_bi3$DMY),
        M= month (t_bi3$DMY),
        Y=year (t_bi3$DMY),
        H= hour(t_bi3$HMS))

