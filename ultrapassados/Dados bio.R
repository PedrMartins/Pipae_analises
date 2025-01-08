
############Read google sheets data into R##############

source("Loadpackges.R")
source("Function_tratamento_pipae.R") #funções t_dpi () teste dos dados 
#rm_sp_if_n () retirar espaços entre strinsgs numéricas 

camCo2_bio3_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1IGluS1hxDTKgR_VnW3YDGFtv8WeFyr5niAkVW_Tm0Fc/edit?usp=sharing",
                            col_types = "?Ti")
source("trat_data_Co.R")

##############plot dados ##########


c= C_bi3 [C_bi3$M== 7,]
c = c [!duplicated(c$D),]

View()

plot (co2 ~D, data=c, type="l")


head (C_bi3)

#############


################Tempe#############


camT_bio3_raw <- read_sheet("https://docs.google.com/spreadsheets/d/138s9fglwY0Ssp8M1-RT4byxKh_hilnY2GEibpsufpLs/edit?usp=sharing",
                            col_types = "?cd")

source("trat_data_Temp.R")

t_bi3

##############plot dados ##########


c= t_bi3 [t_bi3$M== 7,]
c = c [!duplicated(c$D),]

View()

plot (co2 ~D, data=c, type="p")
lines(co2 ~D, data=c)
c$tag <- c(rep(cam))

boxplot (co2 ~M, data = c)
head (C_bi3)

#############

sh <- "https://docs.google.com/spreadsheets/d/1uuSefa8-uphxql42bljG3UQZbBa_AHs-YiMKdKxusA8/edit?usp=sharing"


camU_bio3_raw <- read_sheet(sh, sheet = "ur2", col_types = "?ci")

source("trat_data_ur.R")

ur_bi3


#####
obj <- ls()


rm(list=obj)
