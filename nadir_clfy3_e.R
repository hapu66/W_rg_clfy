######################### -------------------------------    Tool libraries
library(tidyverse)
library(lubridate)
library(gamlss)
# library(ellipse)
library(magrittr)
library(gtsummary)
# library(rapwhale)  # local
# library(pointblank)
#
##################################################################### purpose
#       create canonical set-up
##################################################################### read data
grunnmappe = "\\\\ihelse.net\\kvalitetsregister\\HBE\\2013-1189\\"
#
#
ddmappe = paste0(grunnmappe, "datadumpar\\")
anlzmappe = paste0(grunnmappe, "Hannu\\")

# Hent datoen til siste tilgjengelege uttrekk
dato_uttrekk = list.dirs(ddmappe, recursive = FALSE, full.names = FALSE) %>%
  sort %>%
  last %>% 
  as.Date
# Mappe til dei siste datafila
#
dato_uttrekk <- as.Date("2023-08-03")      ####    overstyr datadump-date
datamappe = paste0(ddmappe, dato_uttrekk, "\\")
avnmappe = paste0(datamappe, "AlleVarNum")
#####  1. read in data       #####                                     ----- 80
#
prc <- function(x) round( 100*x, 1)
pc <- function(x) round( 100*x, 2)

#setwd(avnmappe)
setwd(r'(\\Ihelse.net\kvalitetsregister\hbe\2013-1189\datadumpar\2023-08-03\AlleVarNum)')

fil_PBV = paste0("PatBasVarNum.csv")         ## dd-dato = 3. 3.2023                    
fil_Opr = paste0("OperasjonsVarNum.csv")
fil_u6k = paste0("SeksUkerOppfNum.csv")
#k_bok = paste0("SOReg_klokeboken_2023-08-07.csv") # kloke-boka

PBV <-  read_csv2(fil_PBV) # 13344
Opr <-  read_csv2(fil_Opr)
u6k <-  read_csv2(fil_u6k)
#kb <- read_csv2(k_bok)     
AVN <- PBV %>%             # gjenskape AlleVarNum
  left_join(Opr, by = c("p_pasientid", "ForlopsID", "p_opid")) %>% 
  left_join(u6k, by =c("p_pasientid", "ForlopsID", "p_opid"))

fil_Ars =   paste0("SOReg_DatadumpArsrapport_datadump_", dato_uttrekk,".csv")
fil_1 = paste0("SOReg_Arskontrollar1_datadump_", dato_uttrekk,".csv") 
fil_2 = paste0("SOReg_Arskontrollar2_datadump_", dato_uttrekk,".csv") 
fil_5 = paste0("SOReg_Arskontrollar5_datadump_", dato_uttrekk,".csv") 
#klke = read_csv2(k_bok)

# setwd(datamappe)
setwd(r'(\\Ihelse.net\kvalitetsregister\hbe\2013-1189\datadumpar\2023-08-03)')
# ------------------------------------- read in Avn data
# AVN  <- read_csv2(fil_AVN)
# Ars  <- read_csv2(fil_Ars)   ## alternativ dd Årsrapport
# ---------------------------## ----------------- read in årskontrolldata -- 80
A1 <- read_csv2(fil_1)
A2 <- read_csv2(fil_2)
A5 <- read_csv2(fil_5)
#
# setwd(anlzmappe) 
setwd(r'(C:\Users\hanlyy\OneDrive - Helse Vest\W_regain)')
################################################################### join tables
df<-AVN %>% 
  left_join(A1, by=c("p_pasientid","ForlopsID","p_opid") ) %>% 
  left_join(A2, by=c("p_pasientid","ForlopsID","p_opid") ) %>% 
  left_join(A5, by=c("p_pasientid","ForlopsID","p_opid") )
alle_sh <-unique(df$o_sykehus)

# dg<-Ars %>% 
#   left_join(A1, by=c("PasientID"="p_pasientid","ForlopsID") ) %>%
#   left_join(A2, by=c("PasientID"="p_pasientid","ForlopsID") )

# Legg til info om operasjonsC%r, BMI, fedmerel, etc
df  <- df %>% 
  mutate(op_aar = year(o_dato_op),
         op_mnd = month(o_dato_op),
         op_primar = (o_tidl_fedmeop == 0),
         Sex = ifelse( p_kjonn==2, "F", "M")) %>%
  dplyr::select(-p_kjonn)
#  re-kode fedmerelaterte sykdommar,  pakke=dplyr !!
df %<>% mutate(  a1_beh  =  dplyr::recode(a1_beh , "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_diare =  dplyr::recode(a1_beh_diare, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_ann_sykd =  dplyr::recode(a1_ann_sykd, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh  =  dplyr::recode(a2_beh , "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_diare =  dplyr::recode(a2_beh_diare, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_ann_sykd =  dplyr::recode(a2_ann_sykd, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_sovnap =  dplyr::recode(a1_beh_sovnap, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_hypert = dplyr::recode(a1_beh_hypert, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_diab = dplyr::recode(a1_beh_diab, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_dyslip = dplyr::recode(a1_beh_dyslip, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_dyspepsi = dplyr::recode(a1_beh_dyspepsi, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_depr = dplyr::recode(a1_beh_depr, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a1_beh_musk_skjsm = dplyr::recode(a1_beh_musk_skjsm, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_sovnap = dplyr::recode(a2_beh_sovnap, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_hypert = dplyr::recode(a2_beh_hypert, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_diab = dplyr::recode(a2_beh_diab, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_dyslip = dplyr::recode(a2_beh_dyslip, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_dyspepsi = dplyr::recode(a2_beh_dyspepsi, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_depr = dplyr::recode(a2_beh_depr, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a2_beh_musk_skjsm = dplyr::recode(a2_beh_musk_skjsm, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_)
)
#--------------------------------------------  BMI, implisitte fedmerelaterte
df %<>% mutate(bmi_baseline = b_ant_vekt/(b_ant_hoyde/100)^2,
               bmi_op = o_ant_vekt/(b_ant_hoyde/100)^2,
               bmi_6v = u6_ant_vekt/(u6_ant_hoyde/100)^2,
               bmi_1a = a1_ant_vekt/(a1_ant_hoyde/100)^2,
               bmi_2a = a2_ant_vekt/(a2_ant_hoyde/100)^2,
               u6_TWL =  100*(b_ant_vekt - u6_ant_vekt)/b_ant_vekt, 
               a1_TWL =  100*(b_ant_vekt - a1_ant_vekt)/b_ant_vekt,  
               a2_TWL =  100*(b_ant_vekt - a2_ant_vekt)/b_ant_vekt, 
               a5_TWL =  100*(b_ant_vekt - a5_ant_vekt)/b_ant_vekt, 
               u6_AWL = (b_ant_kmi - u6_ant_kmi)/ (b_ant_kmi-13)*100  ,
               a1_AWL = (b_ant_kmi - a1_ant_kmi)/ (b_ant_kmi-13)*100  ,
               a2_AWL = (b_ant_kmi - a2_ant_kmi)/ (b_ant_kmi-13)*100  ,
               a5_AWL = (b_ant_kmi - a5_ant_kmi)/ (b_ant_kmi-13)*100  ,
               b_beh_sovnap  =   ifelse(b_beh==0, 0, b_beh_sovnap),    
               b_beh_hypert  =   ifelse(b_beh==0, 0, b_beh_hypert),
               b_beh_diab  =   ifelse(b_beh==0, 0, b_beh_diab),
               b_beh_dyslip  =   ifelse(b_beh==0, 0, b_beh_dyslip),
               b_beh_dyspepsi  =   ifelse(b_beh==0, 0, b_beh_dyspepsi),
               b_beh_depr  =   ifelse(b_beh==0, 0, b_beh_depr),
               b_beh_musk_skjsm  =   ifelse(b_beh==0, 0, b_beh_musk_skjsm),
               a1_beh_sovnap  =   ifelse(a1_beh==0, 0, a1_beh_sovnap),   
               a1_beh_hypert  =   ifelse(a1_beh==0, 0, a1_beh_hypert),
               a1_beh_diab  =   ifelse(a1_beh==0, 0, a1_beh_diab),
               a1_beh_dyslip  =   ifelse(a1_beh==0, 0, a1_beh_dyslip),
               a1_beh_dyspepsi  =   ifelse(a1_beh==0, 0, a1_beh_dyspepsi),
               a1_beh_diare  =   ifelse(a1_beh==0, 0, a1_beh_diare),
               a1_beh_depr  =   ifelse(a1_beh==0, 0, a1_beh_depr),
               a1_beh_musk_skjsm =   ifelse(a1_beh==0, 0, a1_beh_musk_skjsm),
               a2_beh_sovnap  =   ifelse(a2_beh==0, 0, a2_beh_sovnap),   
               a2_beh_hypert  =   ifelse(a2_beh==0, 0, a2_beh_hypert),
               a2_beh_diab  =   ifelse(a2_beh==0, 0, a2_beh_diab),
               a2_beh_dyslip  =   ifelse(a2_beh==0, 0, a2_beh_dyslip),
               a2_beh_dyspepsi  =   ifelse(a2_beh==0, 0, a2_beh_dyspepsi),
               a2_beh_diare  =   ifelse(a2_beh==0, 0, a2_beh_diare),
               a2_beh_depr  =   ifelse(a2_beh==0, 0, a2_beh_depr),
               a2_beh_musk_skjsm  =   ifelse(a2_beh==0, 0, a2_beh_musk_skjsm)
)

df<-   df %>% mutate(sc_op1 = as.numeric( a1_dato_oppf- o_dato_op),
                     sc_op2 = as.numeric( a2_dato_oppf- o_dato_op),
                     sc_op5 = as.numeric( a5_dato_oppf- o_dato_op),
                     sc_u6 = as.numeric(u6_oppf_dato-o_dato_op)) 

df <- df %>% mutate(a5_beh_diab = recode(a5_beh_diab, Ja = 1, Nei = 0), 
                    a5_beh_hypert = recode(a5_beh_hypert, Ja = 1, Nei = 0), 
                    a5_beh_dyslip = recode(a5_beh_dyslip, Ja = 1, Nei = 0), 
                    a5_beh_dyspepsi= recode(a5_beh_dyspepsi, Ja = 1, Nei = 0), 
                    a5_beh_musk_skjsm = recode(a5_beh_musk_skjsm, Ja = 1, Nei = 0), 
                    a5_beh_sovnap = recode(a5_beh_sovnap, Ja = 1, Nei = 0), 
                    a5_beh_depr = recode(a5_beh_depr, Ja = 1, Nei = 0), )  %>% 
  filter(a5_TWL <80)

dat_a1 <- df %>% dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_op1, a1_TWL, a1_AWL)   %>% mutate(ktrl=2) 
dat_a2 <- df %>% dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_op2, a2_TWL, a2_AWL)   %>% mutate(ktrl=3)
dat_a5 <- df %>% dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_op5, a5_TWL, a5_AWL)   %>% mutate(ktrl=5)
dat_u6 <- df %>% dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_u6, u6_ant_kmi_red, u6_AWL)  %>% mutate(ktrl=1)

d_prim <- df %>% filter(op_primar)

d <- df %>% 
  dplyr::select(p_pasientid, ForlopsID, op_primar, o_sykehus, o_dato_op, sc_u6, sc_op1, sc_op2, sc_op5, 
                u6_TWL, a1_TWL, a2_TWL, a5_TWL, a1_AWL, a2_AWL, a5_AWL, contains("beh"),
                o_opmetode, o_gbp_type, p_alder_v_op, Sex, bmi_baseline) %>% 
  filter(op_primar, !is.na(a5_TWL))  

d <- d %>% mutate(nadir = pmax(a1_TWL, a2_TWL), nd = ifelse(a1_TWL > a2_TWL, 1, 2))

d_0 <-  d %>% filter(is.na(a1_TWL),is.na(a2_TWL)) %>% mutate(st_12 = "neither 1,2 yr fu")  # A tibble: 22 × 65

d_1 <-   d %>% filter(!is.na(a1_TWL), is.na(a2_TWL)) %>% mutate(nadir=a1_TWL, st_12 = "only 1 yr fu")  # 133
d_2 <-   d %>% filter(is.na(a1_TWL), !is.na(a2_TWL)) %>% mutate(nadir=a2_TWL, st_12 = "only 2 yr fu")  #  44
d_b <-    d %>% filter(!is.na(a1_TWL), !is.na(a2_TWL)) %>% mutate(st_12 = "both 1, 2 yr fu")  # 2892 (prim)/ 2989

Dt <-  bind_rows(d_1, d_2, d_b)                         # 3069 (prim)/ 3181

d_c =  bind_rows(d_0, d_1, d_2, d_b)   # data for consort diagram

      d_c = d_c %>% mutate(exl1 = ifelse(st_12 == "neither 1,2 yr fu", st_12, NA_character_),
                           fu1 = ifelse(st_12 == "only 1 yr fu", st_12, NA_character_),
                           fu2 = ifelse(st_12 == "only 2 yr fu", st_12, NA_character_),
                           fu12 = ifelse(st_12 == "both 1, 2 yr fu", st_12, NA_character_))

Dt  =  Dt %>% filter(a5_TWL > -5)  # 3064 (prim)


# Dt %>% filter(is.na(u6_TWL))
# A tibble: 20 × 65

# Dt %>% filter(u6_TWL > nadir)
# A tibble: 22 × 65

# ------------------------------------------------------------------------------
#  Baseline characteristics
base_chr <- function(df){
  df %>% mutate(F = Sex=="F", 
                S = o_opmetode==6, 
                B = (o_opmetode==1 & o_gbp_type == 1)|(o_opmetode==1 & is.na(o_gbp_type)) , 
                O =  o_opmetode==1 & o_gbp_type == 2) %>%  summarise(n= n(),
                                             Female = pc( mean(F)), 
                                             Age = mean(p_alder_v_op, na.rm = T), 
                                             bmi_b =   mean(bmi_baseline),
                                             GS = pc(mean(S, na.rm = T)),
                                             GB = pc(mean(B, na.rm = T)),
                                             OA = pc(mean(O, na.rm = T)))
}

#   First, we want to give some context for our cohort of included patients. 
# How many patients in the SOReg-N 2023-8-3 dataset are there in total? 
# Maybe with percentage female, mean age, mean initial BMI. 

nrow(df)  # 

base_chr(df)

base_chr(d_prim)

# Second, we have to give some information on lost to follow up. 
# How many of the TOTAL amount of patients in the SOReg-N 2023-8-3 dataset that were 
# eligible for 5yr follow up did have data at 5yr? Same for 1 and 2 years.

d_elig_a5 =  d_prim %>% filter(o_dato_op <  dato_uttrekk - years(5) -months(6))
d_elig_a2 =  d_prim %>% filter(o_dato_op <  dato_uttrekk - years(2) -months(3))
d_elig_a1 =  d_prim %>% filter(o_dato_op <  dato_uttrekk - years(1) -months(3))

d_act_a5 = d_elig_a5 %>% filter(a5_ferdigstill == 1, 
                                a5_dato_oppf - o_dato_op < 2007,
                                a5_dato_oppf - o_dato_op > 1642,
                                a5_oppf_type  %in%  c("Frammøte", "Per telefon eller via nettmøte",  "Per brev/mail eller på annen måte"),
                                !is.na(a5_ant_vekt),
                                a5_ant_vekt > 0)

d_act_a2 = d_elig_a2 %>% filter(a2_ferdigstill == 1, 
                                a2_dato_oppf - o_dato_op < 2*365+90,
                                a2_dato_oppf - o_dato_op > 2*365-90,
                                a2_oppf_type  %in%  c(1,2,3))

d_act_a1 = d_elig_a1 %>% filter(a1_ferdigstill == 1, 
                                a1_dato_oppf - o_dato_op < 365+90,
                                a1_dato_oppf - o_dato_op > 365-90,
                                a1_oppf_type  %in%  c("Frammøte", "Per telefon eller via nettmøte",  "Per brev/mail eller på annen måte"))


 
#  Poor result:  data-frame, time    call eg.  > poor(Dt, "a1_TWL")
poor <- function(df, tm){
  switch(tm,
         nadir =
           {sd_m = median(df$nadir, na.rm = T) -     sd(df$nadir, na.rm = T)
           sd_2m =median(df$nadir, na.rm = T) -  2* sd(df$nadir, na.rm = T)},
         
         u6_TWL =
           {sd_m = median(df$u6_TWL, na.rm = T) -     sd(df$u6_TWL, na.rm = T)
           sd_2m =median(df$u6_TWL, na.rm = T) -  2* sd(df$u6_TWL, na.rm = T)},
  a1_TWL =
  {sd_m = median(df$a1_TWL, na.rm = T) -     sd(df$a1_TWL, na.rm = T)
  sd_2m =median(df$a1_TWL, na.rm = T) -  2* sd(df$a1_TWL, na.rm = T)},
  a2_TWL =
    {sd_m = median(df$a2_TWL, na.rm = T) -     sd(df$a2_TWL, na.rm = T)
    sd_2m =median(df$a2_TWL, na.rm = T) -  2* sd(df$a2_TWL, na.rm = T)},
  a5_TWL =
    {sd_m = median(df$a5_TWL, na.rm = T) -     sd(df$a5_TWL, na.rm = T)
    sd_2m =median(df$a5_TWL, na.rm = T) -  2* sd(df$a5_TWL, na.rm = T)})
  
  c(sd_2m, sd_m)
}


pr <- d %>% filter(a1_TWL < poor(d,"a1_TWL")[2], a1_TWL > poor(d,"a1_TWL")[1]) %>% nrow()
isf <- d %>% filter(a1_TWL < poor(d,"a1_TWL")[1]) %>% nrow()
tot <- d %>% nrow()

# https://stackoverflow.com/questions/2641653/pass-a-data-frame-column-name-to-a-function


#  Analyze result:  data-frame, time    call eg.  > poor(Dt, "a2_TWL")

alz <-  function(df, str){

pr <-  df %>% filter( df[[str]] < poor(df, str)[2], df[[str]] > poor(df, str)[1]) %>% nrow()
isf <- df %>% filter( df[[str]] < poor(df, str)[1]) %>% nrow()
tot <- df %>% nrow()

       c(isf , pr,  tot)  # insufficient, poor, total
}


adq <- function(df, str){
  md = median(df[[str]], na.rm = TRUE)
  pr  = median(df[[str]], na.rm = TRUE) -   sd(df[[str]], na.rm = TRUE)
  isf = median(df[[str]], na.rm = TRUE) -  2*sd(df[[str]], na.rm = TRUE)
  c(isf, pr, md)
}



alz_pr <-  function(df, str){
  
  pr <-  df %>% filter( df[[str]] < poor(df, str)[2], df[[str]] > poor(df, str)[1]) %>% nrow()
  isf <- df %>% filter( df[[str]] < poor(df, str)[1]) %>% nrow()
  tot <- df %>% nrow()
  
  c(round(100*isf/tot, 1) , round(100*pr/tot,1), tot)
}

pck_ave <-  function(df, str){
  ave <-  df %>% filter( df[[str]] > poor(df, str)[2] ) 
  ave 
}

pck_pr <-  function(df, str){
  pr <-  df %>% filter( df[[str]] < poor(df, str)[2], df[[str]] > poor(df, str)[1]) 
  pr 
}

pck_isf <-  function(df, str){
   isf <- df %>% filter( df[[str]] < poor(df, str)[1])  
  isf 
}

# number
n_bad <- function(df, str){
  n =   df[[str]] %>% na.omit %>% length() 
  isf = df %>% filter(df[[str]]  < adq(df, str)[1]) %>% nrow()
  pr  = df %>% filter(  !(df[[str]]  < adq(df, str)[1]) & 
                          df[[str]]  < adq(df, str)[2] ) %>% nrow()
  c(isf, pr, n )
}
# per cent
p_bad <- function(df, str){
  round( 100 * n_bad(df, str) / n_bad(df, str)[3], 2)
}

rp <- function(df, str){
  w = tibble(adq(df, str), n_bad(df, str), p_bad(df, str) )
  names(w) <- c("st", "n", "%")
    w
}

# classify data frame
clfy <-function(df){
  ave_u6 =  pck_ave(Dt, "u6_TWL") 
  ave_a1 = pck_ave(Dt, "a1_TWL")
  ave_ndr = pck_ave(Dt, "nadir")
  ave_a2 = pck_ave(Dt, "a2_TWL")
  ave_a5 = pck_ave(Dt, "a5_TWL")
  
  pr_u6 =  pck_pr(Dt, "u6_TWL")
  pr_a1 = pck_pr(Dt, "a1_TWL")
  pr_ndr = pck_pr(Dt, "nadir")
  pr_a2 = pck_pr(Dt, "a2_TWL")
  pr_a5 = pck_pr(Dt, "a5_TWL")
  
  isf_u6 =  pck_isf(Dt, "u6_TWL")
  isf_a1 = pck_isf(Dt, "a1_TWL")
  isf_ndr = pck_isf(Dt, "nadir")
  isf_a2 = pck_isf(Dt, "a2_TWL")
  isf_a5 = pck_isf(Dt, "a5_TWL")
  
 df <- df %>% mutate( cl = case_when(
    p_pasientid %in% intersect(ave_ndr$p_pasientid, ave_a5$p_pasientid)  ~ "1",
    p_pasientid %in% intersect(ave_ndr$p_pasientid, pr_a5$p_pasientid)  ~  "2b",
    p_pasientid %in% intersect(ave_ndr$p_pasientid, isf_a5$p_pasientid)  ~ "3b",
    
    p_pasientid %in% intersect(pr_ndr$p_pasientid, ave_a5$p_pasientid)  ~ "0",
    p_pasientid %in% intersect(pr_ndr$p_pasientid, pr_a5$p_pasientid)  ~  "2a",
    p_pasientid %in% intersect(pr_ndr$p_pasientid, isf_a5$p_pasientid)  ~ "3a",

    p_pasientid %in% intersect(isf_ndr$p_pasientid, ave_a5$p_pasientid)  ~ "0",
    p_pasientid %in% intersect(isf_ndr$p_pasientid, pr_a5$p_pasientid)  ~  "2a",
    p_pasientid %in% intersect(isf_ndr$p_pasientid, isf_a5$p_pasientid)  ~ "3a",
  ))
    df$cl
}
 table(clfy(Dt))
 sum(table(clfy(Dt)))
 
pc( table(clfy(Dt))/  sum(table(clfy(Dt))) )
sum( pc( table(clfy(Dt))/  sum(table(clfy(Dt))) ))

#  2023-09-08
du = Dt %>% mutate(klass= clfy(Dt))

K0 = du %>% filter(klass == "0")
K1 = du %>% filter(klass == "1")
K2a = du %>% filter(klass == "2a")
K2b = du %>% filter(klass == "2b")
K3a = du %>% filter(klass == "3a")
K3b = du %>% filter(klass == "3b")

base_chr(K0)
base_chr(K1)
base_chr(K2a)
base_chr(K2b)
base_chr(K3a)
base_chr(K3b)


# ave_a5 %>% anti_join( ave_ndr, by = c("p_pasientid", "ForlopsID"))

# > cnt_cmm(ave_ndr, ave_a5)   2343   cl = 1
# > cnt_cmm(ave_ndr, pr_a5)    192          2b
# > cnt_cmm(ave_ndr, isf_a5)  16            3b    

  #  cnt_cmm(pr_ndr, ave_a5)   237           0
#   cnt_cmm(pr_ndr, pr_a5)       178          2a
# > cnt_cmm(pr_ndr, isf_a5)   34             3a

  #    cnt_cmm(isf_ndr, ave_a5)  11       0
#    cnt_cmm(isf_ndr, pr_a5)   38     2a
# > cnt_cmm(isf_ndr, isf_a5)   20     3a

# ave_ndr %>% filter(p_pasientid %in% ave_a5$p_pasientid) %>% pull(p_pasientid)
# ave_ndr %>% filter(p_pasientid %in% pr_a5$p_pasientid) %>% pull(p_pasientid)
# ave_ndr %>% filter(p_pasientid %in% isf_a5$p_pasientid) %>% pull(p_pasientid)

#                        Histogram 
#   grph(Dt, "u6_TWL")
grph <- function(dt, str){

dt_isf = adq(dt, str)[1]
dt_pr  = adq(dt, str)[2]

dt <- dt %>% mutate(cl = ifelse( .data[[str]] < dt_isf, "red", ifelse(.data[[str]] < dt_pr, "yellow", "green")))

tm = substr(str,1,2)
ttl = paste0("Weight loss distribution at ", tm)

ggplot(data = dt, aes(x=.data[[str]])) + 
  geom_histogram(bins = 120, colour="black", aes(fill = cl) ) + 
  scale_fill_manual(labels = c("> average-SD", "insufficient", "poor"), values = c("green", "red", "yellow" )) +
#  scale_fill_discrete(name = "Class", labels = c("> average-SD", "insufficient", "poor")) +
  ggtitle(ttl) +labs(fill = "Class")+
  theme_light()
}


Dt_2 = Dt %>% filter(a5_TWL > -5)



gr <- function(dt, str){
  
  dt_isf = adq(dt, str)[1]
  dt_pr  = adq(dt, str)[2]
  
  dt <- dt %>% mutate(cl = ifelse( .data[[str]] < dt_isf, "red", 
                                   ifelse(.data[[str]] < dt_pr, "yellow", 
                                          "green"))  )
  
  tm = substr(str,1,2)
  ttl = paste0("Weight loss distribution at ", tm)
 
  ggplot(data = dt, aes(x= .data[[str]])) + 
    geom_histogram(bins = 35, colour="black", aes(fill = cl) ) + 
    geom_vline(aes(  xintercept =  median(.data[[str]], na.rm = T)),  linewidth=1, colour= "black") +
    geom_vline(aes(  xintercept =  mean(.data[[str]], na.rm = T)),  linewidth=1, colour= "darkgrey") +
    scale_fill_manual(labels = c("> average-SD", "insufficient", "poor"), values = c("white", "black", "grey" )) +
    #  scale_fill_discrete(name = "Class", labels = c("> average-SD", "insufficient", "poor")) +
    ggtitle(ttl) +labs(fill = "Class")+
    theme_light()
}


 # -	For the histograms, could we replot these with the new sample, thus starting at -5TWL% or 0?
 #  
 #  -	Also, is it possible to show these with bins of 5TWL instead of 1TWL? 
 #  -	Also, can provide mean/median?
 #  -	Could you do the figures in black and white? Colors are not allowed in most journals unfortunately. 




p_u6 = gr(Dt_2, "u6_TWL")
p_u6 + scale_color_grey() + theme_classic()

#  geom_function(fun = function(x)  
#    2000 * dnorm(x, mean = median(dbbmi$bmi), sd =  sd(dbbmi$bmi)), colour="green") +





# -	For attached table, is it possible to provide significance for every grade? Baseline compared to 
# entire population and 5yr FU numbers compared to baseline? 
  
clc_cmbd <- function(dt, str){
  
  tm = ifelse(substr(str, 1, 1)=="b"  , "b", substr(str, 1, 2))
   s1 = paste0(tm, "_beh_diab")
   s2 = paste0(tm, "_beh_hypert")
   s3 = paste0(tm, "_beh_dyslip")
   s4 = paste0(tm, "_beh_dyspepsi")
   s5 = paste0(tm, "_beh_musk_skjsm")
   s6 = paste0(tm, "_beh_sovnap")
   s7 = paste0(tm, "_beh_depr")
   # dt[[s1]]
   dt %>% summarise(n = n(),
                    "Db" =  pc(mean( dt[[s1]], na.rm = TRUE)),
                    "Ht" =  pc(mean( dt[[s2]], na.rm = TRUE)),
                    "Dl" =  pc(mean( dt[[s3]], na.rm = TRUE)),
                    "Dp" =  pc(mean( dt[[s4]], na.rm = TRUE)),
                    "MS" =  pc(mean( dt[[s5]], na.rm = TRUE)),
                    "SA" =  pc(mean( dt[[s6]], na.rm = TRUE)),
                    "Dr" =  pc(mean( dt[[s7]], na.rm = TRUE)),
                    )
             
}

clc_cmbd(K0, "b")
clc_cmbd(K0, "a2")
clc_cmbd(K0, "a5")

clc_cmbd(K1, "b")
clc_cmbd(K1, "a2")
clc_cmbd(K1, "a5")

clc_cmbd(K2a, "b")
clc_cmbd(K2a, "a2")
clc_cmbd(K2a, "a5")

clc_cmbd(K2b, "b")
clc_cmbd(K2b, "a2")
clc_cmbd(K2b, "a5")

clc_cmbd(K3a, "b")
clc_cmbd(K3a, "a2")
clc_cmbd(K3a, "a5")

clc_cmbd(K3b, "b")
clc_cmbd(K3b, "a2")
clc_cmbd(K3b, "a5")

P_b = clc_cmbd(Dt, "b")
P_5 = clc_cmbd(Dt, "a5")

# Why you need to use lists more in R
#  https://www.youtube.com/watch?v=ckjSaNOcerA

idx_tm =  list("b", "a5") 
idx_cmb = list("_beh_diab", "_beh_hypert", "_beh_dyslip", "_beh_sovnap")
paste0(idx_tm, idx_cmb)

L = tidyr::expand_grid(l1 = idx_tm, l2 = idx_cmb ) 
cmbds = paste0(L$l1, L$l2)
cmbds_b = as.list( cmbds[1:4] )
cmbds_a5 = as.list( cmbds[5:8] )
gps = c("K0","K1","K2a","K2b","K3a","K3b")

tbl_merge( 
 tbls= list(
Dt |> select(all_of(cmbds)) |> tbl_summary(),
K0 |> select(all_of(cmbds)) |> tbl_summary(),
K1  |> select(all_of(cmbds)) |> tbl_summary(),
K2a |> select(all_of(cmbds)) |> tbl_summary(),
K2b |> select(all_of(cmbds)) |> tbl_summary(),
K3a |> select(all_of(cmbds)) |> tbl_summary(),
K3b |> select(all_of(cmbds)) |> tbl_summary()), 
tab_spanner = c("Dt","K0","K1","K2a","K2b","K3a","K3b")
)

cpr = function(cmb, krp){
  
  h_tot =  Dt |> select(all_of(cmb)) |> sum()
  h_cmb = krp |> select(all_of(cmb)) |> sum()
  n_tot =  Dt |> select(all_of(cmb)) |> nrow()
  n_cmb = krp |> select(all_of(cmb)) |> nrow()
  
     print(cmb)
     print(c(n_tot, h_tot, n_cmb, h_cmb))
     M = matrix(c(n_tot, h_tot, n_cmb, h_cmb), nrow = 2)
     print(fisher.test(M))
     print("----------------------")
     # print(chisq.test(M))
}



cmpr <- function(cmb, dt){
  map(.x= cmb, ~cpr(.x, dt))
}


#  cpr(cmbds_b[[3]], K2a)
# Dt |> select(cmb)


function(grp) {grp |> select(all_of(cmbds)) |> tbl_summary()}


mb = Dt %>% 
  dplyr::select(contains("beh")) %>% 
  summarise(n=n(), 
            "Db" =  pc(mean(a2_beh_diab, na.rm = TRUE)), 
            "Ht" = pc(mean(a2_beh_hypert, na.rm = TRUE)), 
            "Dl" = pc(mean(a2_beh_dyslip, na.rm = TRUE)),
            "Dp" = pc(mean(a2_beh_dyspepsi, na.rm = TRUE)),
            "MS" = pc(mean(a2_beh_musk_skjsm, na.rm = TRUE)),
            "SA" = pc(mean(a2_beh_sovnap, na.rm = TRUE)),
            "Dr" = pc(mean(a2_beh_depr, na.rm = TRUE)))



ave_u6 =  pck_ave(Dt, "u6_TWL") 
ave_a1 = pck_ave(Dt, "a1_TWL")
ave_ndr = pck_ave(Dt, "nadir")
ave_a2 = pck_ave(Dt, "a2_TWL")
ave_a5 = pck_ave(Dt, "a5_TWL")

pr_u6 =  pck_pr(Dt, "u6_TWL")
pr_a1 = pck_pr(Dt, "a1_TWL")
pr_ndr = pck_pr(Dt, "nadir")
pr_a2 = pck_pr(Dt, "a2_TWL")
pr_a5 = pck_pr(Dt, "a5_TWL")

isf_u6 =  pck_isf(Dt, "u6_TWL")
isf_a1 = pck_isf(Dt, "a1_TWL")
isf_ndr = pck_isf(Dt, "nadir")
isf_a2 = pck_isf(Dt, "a2_TWL")
isf_a5 = pck_isf(Dt, "a5_TWL")


 
isf_a5 %>% left_join(ave_ndr, by =c("p_pasientid", "ForlopsID"))


cnt_cmm <- function(df, dg){
  intersect( df[["p_pasientid"]],  dg[["p_pasientid"]]) %>% length()
}

 
c1 =  cnt_cmm(ave_ndr, ave_a5)    # 2470  / 3178   = 77.7%
c2b = cnt_cmm(ave_ndr, pr_a5)  #  202  = 6.4%
c3b = cnt_cmm(ave_ndr, isf_a5)  #  20 = 0.6%


blw_ave_ndr =  Dt %>% anti_join(ave_ndr, by = c("p_pasientid","ForlopsID"))  # 498
blw_ave_a1 =  Dt %>% anti_join(ave_a1, by = c("p_pasientid","ForlopsID"))  # 535
blw_ave_a2 =  Dt %>% anti_join(ave_a2, by = c("p_pasientid","ForlopsID"))  # 635


c2a = cnt_cmm(blw_ave_ndr, pr_a5)
c3a = cnt_cmm(blw_ave_ndr, isf_a5)


intersect(ave_ndr$p_pasientid,  isf_a5$p_pasientid) %>% length()
intersect(ave_ndr$p_pasientid,  pr_a5$p_pasientid)  %>% length()

avg_lw  =  median(d$nadir, na.rm = T) -      sd(d$nadir, na.rm = T)

avg_u6 =  median(d$u6_TWL, na.rm = T) -     sd(d$u6_TWL, na.rm = T)
ins_u6 =  median(d$u6_TWL, na.rm = T) - 2 * sd(d$u6_TWL, na.rm = T)

avg_a1 =  median(d$a1_TWL, na.rm = T) -     sd(d$a1_TWL, na.rm = T)
ins_a1 =  median(d$a1_TWL, na.rm = T) - 2 * sd(d$a1_TWL, na.rm = T)

avg_a2 =  median(d$a2_TWL, na.rm = T) -     sd(d$a2_TWL, na.rm = T)
ins_a2 =  median(d$a2_TWL, na.rm = T) - 2 * sd(d$a2_TWL, na.rm = T)

avg_a5 =  median(d$a5_TWL, na.rm = T) -     sd(d$a5_TWL, na.rm = T)
ins_a5 =  median(d$a5_TWL, na.rm = T) - 2 * sd(d$a5_TWL, na.rm = T)

cl1 <- d %>% mutate( abv1 = nadir > avg_lw, abv2 =  a5_TWL >  avg_a5 )  %>% filter(abv1, abv2)

cl2b <- d %>% mutate( abv = nadir > avg_lw, 
                      pr =  (a5_TWL <  avg_a5) & (a5_TWL > ins_a5)  )  %>% filter(abv, pr)

cl3b <- d %>% mutate( abv = nadir > avg_lw, isf =  a5_TWL <  ins_a5 )  %>% filter(abv, isf)
                      
                       
dat_WL <- d %>% dplyr::select(contains("sc"), contains("TWL"))

   
d1 = tibble(dat_WL$sc_u6, dat_WL$u6_TWL)  
d2 = tibble(dat_WL$sc_op1, dat_WL$a1_TWL) 
d3 = tibble(dat_WL$sc_op2, dat_WL$a2_TWL) 
d4 = tibble(dat_WL$sc_op5, dat_WL$a5_TWL) 
names(d1) <- names(d2) <- names(d3) <- names(d4) <- c("sc","WL")
# 
# D <- na.omit(bind_rows(d1, d2,d3,d4)) %>% filter(WL>10, WL<70)
# 

#  IND<-sample.int(7040, 1000, replace=FALSE)
#  dbbmi1 <- dbbmi[IND,]

# ggplot(data = D, aes(x=sc,y=WL)) +geom_point()
# # 
# md_0 <- lms(WL, sc, data = D, trans.x = TRUE, c.crit = 0.01, mu.df = 6, sigma.df = 6, nu.df = 4, tau.df = 4,
#             method.pb = "ML", k = 2.4, control = gamlss.control(n.cyc = 60, inter=8))
# 
# mod<-gamlss(WL~pb(sc), sigma.formula=~pb(sc, method=mixed(1,20), k=4),
#                        nu.formula=~pb(sc, method=mixed(1,20), k=4),
#             family=BCCGo,          data=D, method=mixed(1,20))
# plot(mod)
# 
# model_lms <- lms(WL, sc, data = D)            
#The default families arguments is LMS=c("BCCGo",  "BCPEo", "BCTo") that is the
# LMS class of distributions.
#Note that this class is only appropriate when y is positive (with no zeros). 
# If the response variable contains negative values and/or zeros then use 
# the argument theSHASH theSHASH <-  c("NO", "SHASHo") or add any other 
#  distribution which you think is appropriate
# m_dbbmi0 <- lms(bmi,age, data=dbbmi, trans.x=TRUE, k=2)

#
D <- na.omit(bind_rows( d2,d3,d4)) %>% filter(WL>10, WL<70)
# md_1 <- lms(WL, sc, data = D, trans.x = TRUE, k=2)


# , nd_ix = ifelse(nadir == a1_TWL, 1, 2)


dat_a1 %>% left_join(dat_a2, by=c("p_pasientid","ForlopsID",  "o_sykehus", "o_dato_op"))

# cnames <- c("p_id", "F_id", "sh", "d_opr", "sc_op", "TWL", "AWL", "ktrl")
# names(dat_u6) <- cnames
# names(dat_a1) <- cnames
# names(dat_a2) <- cnames
# names(dat_a5) <- cnames
#? datall <- bind_rows(dat_u6, dat_a1, dat_a2, dat_a5) %>% mutate(nadir= max(a1_TWL, a2_TWL), nd_ind=ifelse(nadir ==a1_TWL,1,2))

# ks <-16.95
# data1 <- data1 %>% mutate(above = crv(sc_op1,k=ks) < AWL1)
# data2 <- data2 %>% mutate(above = crv(sc_op2,k=ks) < AWL2)
# data3 <- data5 %>% mutate(above = crv(sc_op5,k=ks) < AWL5)
# datau6 <-datau6 %>% mutate(above = crv(sc_u6,k=ks) < AWLu6)
# 
# abv <- sum(data1$above+data2$above+data3$above, na.rm = TRUE) #+data5$above
# tot <- nrow(data1)+nrow(data2)+nrow(data3) #  +nrow(data5)
# abv/tot



#  mod <- gamlss(formula = TBMI L~ sc_op, data=na.omit(datall))     


####################################     FILTER ------------------------------
########################################    select variables  

#####################################     ANALYZE ----------------------------

 #####################################     graph ------------------------------

ggplot(data=dat_a5, aes(x=sc_op1, y=a5_TWL) ) + 
  geom_point(data=dat_a1, aes(x = sc_op1, y = a1_TWL), col='blue') +  
  geom_point(data=dat_a2, aes(x = sc_op2, y = a2_TWL), col='green') +
  geom_point(data=dat_a5, aes(x = sc_op5, y = a5_TWL), col='black') +
  geom_point(data=dat_u6, aes(x = sc_u6,  y = u6_ant_kmi_red), col='red') +
  theme_bw()
#####################################     communicate ------------------------
#
#####   results: tables  
#

#
#####   results: graphs    
