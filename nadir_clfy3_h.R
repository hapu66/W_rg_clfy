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
df <- AVN %>%
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
                 a5_beh  =  dplyr::recode(a5_beh , "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),        # 2023-12-03
                 a5_beh_diare =  dplyr::recode(a5_beh_diare, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_ann_sykd =  dplyr::recode(a5_ann_sykd, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
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
                 a2_beh_musk_skjsm = dplyr::recode(a2_beh_musk_skjsm, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_sovnap =  dplyr::recode(a5_beh_sovnap, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_hypert = dplyr::recode(a5_beh_hypert, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_diab = dplyr::recode(a5_beh_diab, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_dyslip = dplyr::recode(a5_beh_dyslip, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_dyspepsi = dplyr::recode(a5_beh_dyspepsi, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_depr = dplyr::recode(a5_beh_depr, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
                 a5_beh_musk_skjsm = dplyr::recode(a5_beh_musk_skjsm, "Nei" = 0L,  "Ja" = 1L, .default=NA_integer_),
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
               a2_beh_musk_skjsm  =   ifelse(a2_beh==0, 0, a2_beh_musk_skjsm),
               a5_beh_sovnap  =   ifelse(a5_beh==0, 0, a5_beh_sovnap),
               a5_beh_hypert  =   ifelse(a5_beh==0, 0, a5_beh_hypert),
               a5_beh_diab  =   ifelse(a5_beh==0, 0, a5_beh_diab),
               a5_beh_dyslip  =   ifelse(a5_beh==0, 0, a5_beh_dyslip),
               a5_beh_dyspepsi  =   ifelse(a5_beh==0, 0, a5_beh_dyspepsi),
               a5_beh_diare  =   ifelse(a5_beh==0, 0, a5_beh_diare),
               a5_beh_depr  =   ifelse(a5_beh==0, 0, a5_beh_depr),
               a5_beh_musk_skjsm  =   ifelse(a5_beh==0, 0, a5_beh_musk_skjsm),
               a5_nt =  a5_ferdigstill == 1 & a5_dato_oppf - o_dato_op < 2007 & a5_dato_oppf - o_dato_op > 1642 &
                 a5_oppf_type %in%  c(1,2,3) &
                 !is.na(a5_ant_vekt)  & a5_ant_vekt > 0   # fu a5 i normtid,   3 st wrong with a5_ant_vekt == 0
)  # 3 pas have a5_TWL == 100 ??

df<-   df %>% mutate(sc_op1 = as.numeric( a1_dato_oppf- o_dato_op),
                     sc_op2 = as.numeric( a2_dato_oppf- o_dato_op),
                     sc_op5 = as.numeric( a5_dato_oppf- o_dato_op),
                     sc_u6 = as.numeric(u6_oppf_dato-o_dato_op))


# a5_ferdigstill == 1 & a5_dato_oppf - o_dato_op < 2007 & a5_dato_oppf - o_dato_op > 1642 &
#  a5_oppf_type %in%  c(1,2,3) &


dat_a1 <- df %>% dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_op1, a1_TWL, a1_AWL)   %>% mutate(ktrl=2)
dat_a2 <- df %>% dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_op2, a2_TWL, a2_AWL)   %>% mutate(ktrl=3)
dat_a5 <- df %>% dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_op5, a5_TWL, a5_AWL)   %>% mutate(ktrl=5)
dat_u6 <- df %>% dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_u6, u6_ant_kmi_red, u6_AWL)  %>% mutate(ktrl=1)

d_prim <- df |> filter(op_primar) |> dplyr::mutate(
  a1_nt =  a1_ferdigstill == 1 & a1_dato_oppf - o_dato_op < months(15) & a1_dato_oppf - o_dato_op > months(9) &
    a1_oppf_type %in%  c("Frammøte", "Per brev/mail eller på annen måte", "Per telefon eller via nettmøte") &
    !is.na(a1_ant_vekt)  ,
  a2_nt =  a2_ferdigstill == 1 & a2_dato_oppf - o_dato_op < months(27) & a2_dato_oppf - o_dato_op > months(21) &
    a2_oppf_type %in%  c(1,2,3) &
    !is.na(a2_ant_vekt)  ,
  a5_nt =  a5_ferdigstill == 1 & a5_dato_oppf - o_dato_op < 2007 & a5_dato_oppf - o_dato_op > 1642 &
    a5_oppf_type %in%  c("Frammøte","Per brev/mail eller på annen måte","Per telefon eller via nettmøte") &
    !is.na(a5_ant_vekt)  & a5_ant_vekt > 0,
)

d_elig_a5 <- d_prim |> filter(o_dato_op < as.Date(dato_uttrekk) - years(5) - months(6))
d_elig_a2 <- d_prim |> filter(o_dato_op < as.Date(dato_uttrekk) - years(2) - months(3))
d_elig_a1 <- d_prim |> filter(o_dato_op < as.Date(dato_uttrekk) - years(1) - months(3))


nitti_m  <-  function(yr=1, dag= today(), l=90) { dag-ddays(l)+years(yr) }      #   nitti_m(today())
nitti_p  <-  function(yr=1, dag= today(), l=90) { dag+ddays(l)+years(yr) }
#  nitti    <-  function(yr=1, dag= today(), l=90) { c(dag-ddays(l)+years(yr),
#                                                      dag+ddays(l)+years(yr)) }
#
#####     new variables
d_nt  <- d_prim  %>%  mutate( et_nor_m = nitti_m(dag= o_dato_op),
                             et_nor_p = nitti_p(dag= o_dato_op),
                             to_nor_m = nitti_m(yr=2, dag= o_dato_op),
                             to_nor_p = nitti_p(yr=2, dag= o_dato_op)  )

aars_ktrls <- function(dt){                                        # 30.07.2021
  dmx_1ar  <- dt      %>% filter(a1_ferdigstill==1)                # n = 7756
  dmx_2ar  <- dmx_1ar %>% filter(a2_ferdigstill==1)                # n = 5747
  #####     new variables
  dn1 <-  dmx_1ar %>% mutate(et_nt= a1_dato_oppf %within% interval( et_nor_m, et_nor_p ),
                             et_b4= a1_dato_oppf %within% interval( o_dato_op +1, et_nor_m-1 ),
                             et_lt= a1_dato_oppf > et_nor_p,
                             fs1_gj_kt = `a1_oppf_type` == 4 ,
                             fs1_u_kt  = `a1_oppf_type` == 5  )
   d_ett <- dn1 %>%  select(  c("p_pasientid", "Sex", "o_sykehus", "o_dato_op", "p_dod",
                                "o_opmetode", "o_gbp_type", "p_alder_v_op", "bmi_baseline",
                               "et_b4", "et_nt", "et_lt",
                               "fs1_gj_kt", "fs1_u_kt",
                               "p_pas_utg" ))
  #####     new variables
  dn2 <- dn1 %>% mutate( to_nt= a2_dato_oppf %within% interval( to_nor_m, to_nor_p ),
                         to_b4= a2_dato_oppf %within% interval( o_dato_op +1, to_nor_m-1 ),
                         to_lt= a2_dato_oppf > to_nor_p ,
                         fs2_gj_kt = `a2_oppf_type` == 4 ,
                         fs2_u_kt  = `a2_oppf_type` == 5)
  d_to <- dn2 %>%  select(  c("p_pasientid", "Sex", "o_sykehus", "o_dato_op", "p_dod",
                              "o_opmetode", "o_gbp_type", "p_alder_v_op", "bmi_baseline",
                              "et_b4", "et_nt", "et_lt",  "to_b4",  "to_nt", "to_lt",
                              "fs1_gj_kt", "fs1_u_kt",    "fs2_gj_kt", "fs2_u_kt",
                              "p_pas_utg" ))
  dn   <- d_ett %>% left_join(d_to, by = names(d_ett) )  #
  dn
}

d2_nt <- aars_ktrls(d_nt)         # alle sjukehus

d2_nt |> summarise(a1_nt = sum(et_nt, na.rm = T), a2_nt = sum(to_nt, na.rm = T) )


d <- d_prim %>%  filter(a5_TWL <80) %>%
  dplyr::select(p_pasientid, ForlopsID, op_primar, o_sykehus, o_dato_op, sc_u6, sc_op1, sc_op2, sc_op5,
  b_ant_vekt, a1_ant_vekt, a2_ant_vekt, a5_ant_vekt, u6_TWL, a1_TWL, a2_TWL, a5_TWL, a5_nt, a5_dato_oppf,
  a1_AWL, a2_AWL, a5_AWL, contains("beh"), contains("ferdig"),
  o_opmetode, o_gbp_type, p_alder_v_op, Sex, bmi_baseline) %>%
  filter(op_primar, !is.na(a5_TWL))

# d |> filter(a5_ferdigstill == 0)  |> pull(a5_TWL)

d <- d %>% mutate(nadir = pmax(a1_TWL, a2_TWL), nd = ifelse(a1_TWL > a2_TWL, 1, 2),
    nd_ant_vekt = ifelse(nd==1, a1_ant_vekt, ifelse(nd==2, a2_ant_vekt, NA_real_)))

d_0 <-  d %>% filter(is.na(a1_TWL),is.na(a2_TWL)) %>% mutate(st_12 = "neither 1,2 yr fu")  # A tibble: 22 × 65

d_1 <-   d %>% filter(!is.na(a1_TWL), is.na(a2_TWL)) %>% mutate(nadir=a1_TWL, st_12 = "only 1 yr fu")  # 133
d_2 <-   d %>% filter(is.na(a1_TWL), !is.na(a2_TWL)) %>% mutate(nadir=a2_TWL, st_12 = "only 2 yr fu")  #  44
d_b <-    d %>% filter(!is.na(a1_TWL), !is.na(a2_TWL)) %>% mutate(st_12 = "both 1, 2 yr fu")  # 2892 (prim)/ 2989

Dt <-  bind_rows(d_1, d_2, d_b)                         # 3069 (prim)/ 3181

d_c =  bind_rows(d_0, d_1, d_2, d_b)   # data for consort diagram

d_c = d_c %>%
  mutate(exl1 = ifelse(st_12 == "neither 1,2 yr fu", st_12, NA_character_),
   fu1 = ifelse(st_12 == "only 1 yr fu", st_12, NA_character_),
   fu2 = ifelse(st_12 == "only 2 yr fu", st_12, NA_character_),
   fu12 = ifelse(st_12 == "both 1, 2 yr fu", st_12, NA_character_))

Dt  =  Dt %>% filter(a5_TWL > -5)  # 3064 (prim)

#
# Dt |> filter(!a5_nt) |> dplyr::select(o_dato_op,  a5_dato_oppf ) |> mutate(a5_dato_oppf-o_dato_op)


# Dt %>% filter(is.na(u6_TWL))
# A tibble: 20 × 65

# Dt %>% filter(u6_TWL > nadir)
# A tibble: 22 × 65

# ------------------------------------------------------------------------------
#  Baseline characteristics
base_chr <- function(dt){
  dt %>% mutate(Fm = Sex == "F",
                S = o_opmetode==6,
                B = (o_opmetode==1 & o_gbp_type == 1)|(o_opmetode==1 & is.na(o_gbp_type)) ,
                O =  o_opmetode==1 & o_gbp_type == 2) %>%  summarise(n= n(),
               Female =  100*mean(Fm),
               WRG = mean(a5_ant_vekt - nd_ant_vekt, na.rm = TRUE),  # kg
               WRG_sd = sd(a5_ant_vekt - nd_ant_vekt, na.rm = TRUE),
               Age = mean(p_alder_v_op, na.rm = T),
               bmi_b =   mean(bmi_baseline),
                GS =  100*mean(S, na.rm = T),
                GB =  100*mean(B, na.rm = T),
                OA =  100*mean(O, na.rm = T))
}

#  Poor result:  data-frame, time    call eg.  > poor(Dt, "a1_TWL")

#  DATO -classifiering:  NADIR -1SD = 25,  a5  -2SD =10 -1SD=20
poor <- function(df, tm){
  switch(tm,
         nadir =
           {sd_m = 25 
           # median(df$nadir, na.rm = T) -     sd(df$nadir, na.rm = T)
           sd_2m = median(df$nadir, na.rm = T) -  2* sd(df$nadir, na.rm = T)},

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
    {sd_m = 20 
    # median(df$a5_TWL, na.rm = T) -     sd(df$a5_TWL, na.rm = T)
    sd_2m = 10}) 
    #  median(df$a5_TWL, na.rm = T) -  2* sd(df$a5_TWL, na.rm = T)})

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

adq2 <- function(df, str){

  p_pr  = median(df[[str]], na.rm = TRUE) +   sd(df[[str]], na.rm = TRUE)
  p_isf = median(df[[str]], na.rm = TRUE) +  2*sd(df[[str]], na.rm = TRUE)
  md = median(df[[str]], na.rm = TRUE)
  pr  = median(df[[str]], na.rm = TRUE) -   sd(df[[str]], na.rm = TRUE)
  isf = median(df[[str]], na.rm = TRUE) -  2*sd(df[[str]], na.rm = TRUE)
  c(isf, pr, md, p_pr, p_isf)
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

    a5_TWL >= nadir & p_pasientid %in% intersect(pr_ndr$p_pasientid, ave_a5$p_pasientid)  ~ "0",
    p_pasientid %in% intersect(pr_ndr$p_pasientid, pr_a5$p_pasientid)  ~  "2a",
    p_pasientid %in% intersect(pr_ndr$p_pasientid, isf_a5$p_pasientid)  ~ "3a",

    a5_TWL >= nadir & p_pasientid %in% intersect(isf_ndr$p_pasientid, ave_a5$p_pasientid) ~ "0",
    p_pasientid %in% intersect(isf_ndr$p_pasientid, pr_a5$p_pasientid)  ~  "2a",
    p_pasientid %in% intersect(isf_ndr$p_pasientid, isf_a5$p_pasientid)  ~ "3a",

    TRUE ~ "1"   # -SD nadir < 25.8,  a5 > 17.2
  ))
    df$cl
}
 table(clfy(Dt))  # a5_TWL >= nadir ~ "0",

pc( table(clfy(Dt))/  sum(table(clfy(Dt)))  )  
sum( pc( table(clfy(Dt))/  sum(table(clfy(Dt))) ))


#  2023-09-08
du = Dt %>% mutate(klass= clfy(Dt))  # 3064
  #---------------------------------------------
K0 = du %>% filter(klass == "0")     # 239        109
K1 = du %>% filter(klass == "1")     # 2335     2465
K2a = du %>% filter(klass == "2a")   # 222        222
K2b = du %>% filter(klass == "2b")   # 199       199
K3a = du %>% filter(klass == "3a")   #  54         54
K3b = du %>% filter(klass == "3b")   #  15          15

KR = du %>% filter(klass == "R")      # 130  ## -SD nadir < 25.8,  a5 > 17.2 -> K1

base_chr(Dt)

base_chr(K0)
base_chr(K1)
base_chr(K2a)
base_chr(K2b)
base_chr(K3a)
base_chr(K3b)

# 2024-02-02
W_regain = K0  %>% mutate(WR = nadir-a5_TWL) %>% dplyr::select(WR)

mean(W_regain$WR)

W_regain %>% ggplot( aes(x=WR, fill= factor(sign(WR)))) +
  geom_histogram( ) + theme_minimal() + ggtitle("Weight regain/loss in %TWL points from nadir to a5, K0, n = 109") +
  xlab("Weight regain") + ylab("Patients") +  labs(fill = "loss/gain")

pc <- function(x,d)  round(x, d)
fm <- function(x) format(x, digits = 3)

bs_vs_all <- function(gr, al = Dt, cmbd = "b_beh_diab"){

  M = matrix( nrow = 2, ncol = 2,
             c(sum( gr[[cmbd]]), nrow(gr),
               sum( al[[cmbd]]), nrow(al)) )
  print(M)
  chi =  chisq.test( M)
  fis = fisher.test( M)

  str = paste( fm(100*M[1,1]/M[2,1]), fm(100*M[1,2]/M[2,2]),"p =", fm(chi$p.value), fm(fis$p.value))
 str
}


 bs_vs_all(K0, cmbd = "b_beh_diab")
bs_vs_all(K1, cmbd = "b_beh_diab")
bs_vs_all(K2a, cmbd = "b_beh_diab")
bs_vs_all(K2b, cmbd = "b_beh_diab")
bs_vs_all(K3a, cmbd = "b_beh_diab")
bs_vs_all(K3b, cmbd = "b_beh_diab")

bs_vs_all(K0, cmbd = "b_beh_hypert")
bs_vs_all(K1, cmbd = "b_beh_hypert")
bs_vs_all(K2a, cmbd = "b_beh_hypert")
bs_vs_all(K2b, cmbd = "b_beh_hypert")
bs_vs_all(K3a, cmbd = "b_beh_hypert")
bs_vs_all(K3b, cmbd = "b_beh_hypert")

bs_vs_all(K0, cmbd = "b_beh_dyslip")
bs_vs_all(K1, cmbd = "b_beh_dyslip")
bs_vs_all(K2a, cmbd = "b_beh_dyslip")
bs_vs_all(K2b, cmbd = "b_beh_dyslip")
bs_vs_all(K3a, cmbd = "b_beh_dyslip")
bs_vs_all(K3b, cmbd = "b_beh_dyslip")

bs_vs_all(K0, cmbd = "b_beh_sovnap")
bs_vs_all(K1, cmbd = "b_beh_sovnap")
bs_vs_all(K2a, cmbd = "b_beh_sovnap")
bs_vs_all(K2b, cmbd = "b_beh_sovnap")
bs_vs_all(K3a, cmbd = "b_beh_sovnap")
bs_vs_all(K3b, cmbd = "b_beh_sovnap")


bs_vs_a5 <- function(gr, cmbd = "_beh_diab"){
  cmbd_b =  paste0("b", cmbd)
  cmbd_a5 = paste0("a5", cmbd)

  M = matrix( nrow = 2, ncol = 2,
              c(sum( gr[[cmbd_b]]),  nrow(gr),
                sum( gr[[cmbd_a5]], na.rm = T ), nrow(gr)))
  print(M)
  chi =  chisq.test( M)
  fis = fisher.test( M)

  str = paste( fm(100*M[1,1]/M[2,1]), fm(100*M[1,2]/M[2,2]),"p =", fm(chi$p.value), fm(fis$p.value))
  str
}

bs_vs_a5(K0, cmbd = "_beh_diab")
bs_vs_a5(K1, cmbd = "_beh_diab")
bs_vs_a5(K2a, cmbd = "_beh_diab")
bs_vs_a5(K2b, cmbd = "_beh_diab")
bs_vs_a5(K3a, cmbd = "_beh_diab")
bs_vs_a5(K3b, cmbd = "_beh_diab")

bs_vs_a5(K0, cmbd = "_beh_hypert")
bs_vs_a5(K1, cmbd = "_beh_hypert")
bs_vs_a5(K2a, cmbd = "_beh_hypert")
bs_vs_a5(K2b, cmbd = "_beh_hypert")
bs_vs_a5(K3a, cmbd = "_beh_hypert")
bs_vs_a5(K3b, cmbd = "_beh_hypert")

bs_vs_a5(K0, cmbd = "_beh_dyslip")
bs_vs_a5(K1, cmbd = "_beh_dyslip")
bs_vs_a5(K2a, cmbd = "_beh_dyslip")
bs_vs_a5(K2b, cmbd = "_beh_dyslip")
bs_vs_a5(K3a, cmbd = "_beh_dyslip")
bs_vs_a5(K3b, cmbd = "_beh_dyslip")

bs_vs_a5(K0, cmbd = "_beh_sovnap")
bs_vs_a5(K1, cmbd = "_beh_sovnap")
bs_vs_a5(K2a, cmbd = "_beh_sovnap")
bs_vs_a5(K2b, cmbd = "_beh_sovnap")
bs_vs_a5(K3a, cmbd = "_beh_sovnap")
bs_vs_a5(K3b, cmbd = "_beh_sovnap")


rems <- function(dt, cmbd){
  bl = paste0("b_beh_", cmbd)
  a5 = paste0("a5_beh_", cmbd)

  res = round(
  c(sum(dt[[bl]], na.rm = T)  / nrow(dt) *100,
  sum(dt[[a5]], na.rm = T) / nrow(dt) *100,
  -(sum(dt[[bl]], na.rm = T) - sum(dt[[a5]], na.rm = T)) / nrow(dt) *100), 1)

  res
}
# rems(Dt, "diab")
# [1] 13.64  5.29  8.36

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
grph <- function(dt, str, bn = 34){

dt_isf = adq(dt, str)[1]
dt_pr  = adq(dt, str)[2]

dt <- dt %>% mutate(cl = ifelse( .data[[str]] < dt_isf, "black", ifelse(.data[[str]] < dt_pr, "grey", "white")))

tm = substr(str,1,2)
ttl = paste0("Weight loss distribution at ", tm)

ggplot(data = dt, aes(x=.data[[str]])) +
  geom_histogram(bins = bn, colour="black", aes(fill = cl) ) +
  scale_fill_manual(labels = c("> average-SD", "insufficient", "poor"), values = c("white", "black", "grey" )) +
#  scale_fill_discrete(name = "Class", labels = c("> average-SD", "insufficient", "poor")) +
  ggtitle(ttl) +labs(fill = "Class")+
  theme_light()
}


Dt_2 = Dt %>% filter(a5_TWL > -5)








lB = round(adq2(Dt_2, "a5_TWL"),1)
a1 = paste0("-2SD: ", lB[1])
a2 = paste0("-SD: ", lB[2])
a3 = paste0("median: ", lB[3])
a4 = paste0("+SD: ", lB[4])
a5 = paste0("+2SD: ", lB[5])

# xL = paste0( c("-2SD = ", "-SD = ", "median = ",  "+SD = ","+2SD = "),
# round(adq2(Dt_2, "a5_TWL"),1)

isf=   adq(Dt_2, "a5_TWL")[1]
pr = adq(Dt_2, "a5_TWL")[2]
med = adq(Dt_2, "a5_TWL")[3]


Dt_2 <- Dt_2 |> mutate(cl = ifelse(a5_TWL < isf, "black", ifelse( a5_TWL < pr, "grey", "white")))
mn5 =  mean(Dt_2$a5_TWL, na.rm = T)
md5 =   median(Dt_2$a5_TWL, na.rm = T)


# position = 'identity'
ggplot(data = Dt_2, aes(x=a5_TWL)) + geom_histogram(bins=34,  boundary= isf,  colour="black",
                                                    aes(fill=cl), position = 'identity',
                                                    show.legend = FALSE) +
  ggtitle("Late weight loss")+ xlab("Five years %TWL")+ ylab("patients") +
  geom_vline(aes(  xintercept =  mn5),  linewidth=1, colour= "black") +
  geom_vline(aes(  xintercept =  md5),  linewidth=1, colour= "darkgrey") +
  geom_vline(aes(  xintercept =  pr),  linewidth=1, colour= "black") +
  geom_vline(aes(  xintercept =  isf),  linewidth=1, colour= "black") +
  scale_fill_manual(labels = c("> average-SD", "insufficient", "poor"), values = c( "black", "grey", "white")) +
  scale_x_continuous(breaks =  adq2(Dt_2, "a5_TWL"), labels = c(a1,a2,a3,a4,a5)) + # theme(legend.position = "none") +
  theme_minimal( )

# -----------------------------------------------
Dt_3 <- Dt_2 |> mutate(cl =    ifelse( a2_TWL < pr, "grey", "white"))
mn =  mean(Dt_3$a2_TWL, na.rm = T)
md =   median(Dt_3$a2_TWL, na.rm = T)

lB2 = round(adq2(Dt_3, "a2_TWL"),1)
b1 = paste0("-2SD: ", lB2[1])
b2 = paste0("-SD: ", lB2[2])
b3 = paste0("median: ", lB2[3])
b4 = paste0("+SD: ", lB2[4])
b5 = paste0("+2SD: ", lB2[5])



# position = 'identity'
ggplot(data = Dt_3, aes(x=a2_TWL)) + geom_histogram(bins=34,  boundary= pr,  colour="black",
                                                    aes(fill=cl), position = 'identity',
                                                    show.legend = FALSE) +
  ggtitle("Early weight loss")+ xlab("Two years %TWL")+ ylab("patients") +
  geom_vline(aes(  xintercept =  mn),  linewidth=1, colour= "black") +
  geom_vline(aes(  xintercept =  md),  linewidth=1, colour= "darkgrey") +
  geom_vline(aes(  xintercept =  pr),  linewidth=1, colour= "black") +
 # geom_vline(aes(  xintercept =  isf),  linewidth=1, colour= "black") +
  scale_fill_manual(labels = c("> average-SD",   "poor"), values = c( "grey", "white" )) +
  scale_x_continuous(breaks =  adq2(Dt_2, "a5_TWL"), labels = c(b1,b2,b3,b4,b5)) + # theme(legend.position = "none") +
  theme_minimal( )


# 2024-01-26  nadir
lB3 = round(adq2(Dt_3, "nadir"),1)
f1 = paste0("-2SD: ", lB3[1])
f2 = paste0("-SD: ", lB3[2])
f3 = paste0("median: ", lB3[3])
f4 = paste0("+SD: ", lB3[4])
f5 = paste0("+2SD: ", lB3[5])


Dt_4 <- Dt_2 |> mutate(cl =    ifelse( nadir < pr, "grey", "white"))
Dt_4 <- Dt_2 |> mutate(cl = ifelse(a5_TWL < isf, "black", ifelse( a5_TWL < pr, "grey", "white")))


mn =  mean(Dt_4$nadir, na.rm = T)
md =   median(Dt_4$nadir, na.rm = T)
pr = adq(Dt_4, "nadir")[2]

# position = 'identity'
ggplot(data = Dt_4, aes(x=nadir)) + geom_histogram(bins=34,  boundary= pr,  colour="black",
                                                    aes(fill=cl),   position = 'identity',
                                                    show.legend = FALSE) +
  ggtitle("Highest early weight loss")+ xlab("NADIR")+ ylab("patients") +
  geom_vline(aes(  xintercept =  mn),  linewidth=1, colour= "black") +
  geom_vline(aes(  xintercept =  md),  linewidth=1, colour= "darkgrey") +
  geom_vline(aes(  xintercept =  pr),  linewidth=1, colour= "black") +
# geom_vline(aes(  xintercept =  isf),  linewidth=1, colour= "black") +
#  scale_fill_manual(labels = c("> average-SD",   "poor"), values = c( "grey", "white" )) +
  scale_fill_manual(labels = c("> average-SD", "insufficient", "poor"), values = c( "black", "grey", "white")) +
  scale_x_continuous(breaks =  adq2(Dt_4, "nadir"), labels = c(f1,f2,f3,f4,f5 )) + # theme(legend.position = "none") +
  theme_minimal( )

gr <- function(dt, str, bn=34){

  dt_isf = adq(dt, str)[1]
  dt_pr  = adq(dt, str)[2]
  dt <- dt %>% mutate(cl = ifelse( .data[[str]] < dt_isf, "red",
                                   ifelse(.data[[str]] < dt_pr, "yellow", "green"))  )
  tm = substr(str,1,2)
  ttl = paste0("Highest early weight loss, NADIR")
  clo = c(rep("black",10), rep("grey",4), rep("white", 20)) # grey grey white
  ggplot(data = dt, aes(x= .data[[str]])) +
    geom_histogram(bins = bn, colour="black",  fill = clo, show.legend = FALSE) + #  aes(fill=cl)
    geom_vline(aes(  xintercept = dt_pr),  linewidth=1, colour= "black") +
    geom_vline(aes(  xintercept =  median(.data[[str]], na.rm = T)),  linewidth=1, colour= "black") +
    geom_vline(aes(  xintercept =  mean(.data[[str]], na.rm = T)),  linewidth=1, colour= "darkgrey") +
    scale_fill_manual(labels = c("> average-SD", "insufficient", "poor"),
                      values = c( "green"= "white", "red"=  "black","yellow"=   "grey" )) +
    scale_x_continuous(breaks =  adq2(Dt_4, "nadir"), labels = c(f1,f2,f3,f4,f5 )) + # theme(legend.position = "none") +
    #  scale_fill_discrete(name = "Class", labels = c("> average-SD", "insufficient", "poor")) +
    ggtitle(ttl) +  # labs(fill = "Class")+
    xlab("NADIR")+ ylab("patients") +
    theme_minimal()
}

  gr(Dt_4, "nadir", bn=34)


D_trj =  Dt_2 |> select(p_pasientid, contains("sc"), contains("TWL"))

du6 = D_trj |> select(p_pasientid, contains("u6"))
da1 = D_trj |> select(p_pasientid, contains("op1"),  contains("a1"))
da2 = D_trj |> select(p_pasientid, contains("op2"),  contains("a2"))
da5 = D_trj |> select(p_pasientid, contains("op5"),  contains("a5"))

names(du6) <- c("id", "sc", "WL")
names(da1) <- c("id", "sc", "WL")
names(da2) <- c("id", "sc", "WL")
names(da5) <- c("id", "sc", "WL")

d_tj =  bind_rows(du6, da1, da2, da5)

d_mn = tribble( ~sc, ~WL,
30, mean(du6$WL, na.rm = T),
365, mean(da1$WL, na.rm = T),
2*365, mean(da2$WL, na.rm = T),
5*365, mean(da5$WL, na.rm = T)
)

gjennomsikt = .5 * 100/n_distinct(d_tj$id)

d_tj |> group_by(id) |> ggplot(aes(x=sc/365, y=WL, group=id)) +
  geom_line(aes(x=sc/365, y=WL), colour = alpha("black", gjennomsikt), linewidth = .4) +
  geom_point(size = .95, colour = alpha("black", gjennomsikt)) +
  geom_point(aes(x=sc/365, y=WL), size = 3, data = d_mn, group=1, colour="black") +
  geom_line(aes(x=sc/365, y=WL), linewidth = 0.9, data = d_mn, group=1, colour="black") +
  ggtitle("Weight loss individually")+ xlab("Years")+ ylab("%TWL") +
  theme_minimal()
# D_trj |> ggplot(aes(x= , y =))

# gjennomsikt = .5 * 100/n_distinct(d_bmi_lang_5$PasientID)
# ggplot(d_bmi_lang_5, aes(x = tidspunkt, y = bmi, group = PasientID)) +
#   scale_x_discrete(expand=c(0, 0.3)) + # coord_cartesian(ylim=c(0, 60))+
#   # theme(axis.t)+
#   scale_y_continuous(breaks = c(0,20,30,40,50,60))+
#   geom_line(colour = alpha(colPrim[3], gjennomsikt), linewidth = .5) +
#   geom_point(size = .75, colour = alpha(colPrim[3], gjennomsikt)) +
#   geom_point(size = 2, data = d_bmi_lang_snitt_5, group=1, colour=colKontr) +
#   geom_line(linewidth = 1, data = d_bmi_lang_snitt_5, group=1, colour=colKontr) +
#   geom_hline(yintercept=24.9, linetype=3)+  geom_hline(yintercept=18.5, linetype=3)+
#   xlab(NULL) + ylab("KMI\n(kg/m²)") +
#   #  scale_y_discrete( limits=c("20","30","40","50","60")) +
#   fjern_x

 # -	For the histograms, could we replot these with the new sample, thus starting at -5TWL% or 0?
 #
 #  -	Also, is it possible to show these with bins of 5TWL instead of 1TWL?
 #  -	Also, can provide mean/median?
 #  -	Could you do the figures in black and white? Colors are not allowed in most journals unfortunately.




p_u6 = gr(Dt_2, "u6_TWL")
p_u6 + scale_color_grey() + theme_classic()




p_a5 = gr(Dt_2, "a5_TWL")
p_a5 + scale_color_grey() + theme_classic()


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


clc_cmbd_2 <- function(dt, str){

  tm = ifelse(substr(str, 1, 1)=="b"  , "b", substr(str, 1, 2))
  s1 = paste0(tm, "_beh_diab")
  s2 = paste0(tm, "_beh_hypert")
  s3 = paste0(tm, "_beh_dyslip")
 # s4 = paste0(tm, "_beh_dyspepsi")
 # s5 = paste0(tm, "_beh_musk_skjsm")
  s6 = paste0(tm, "_beh_sovnap")
 # s7 = paste0(tm, "_beh_depr")
  # dt[[s1]]
  dt %>% summarise(n = n(),
                   "Db" =  pc(mean( dt[[s1]], na.rm = TRUE)),
                   "Ht" =  pc(mean( dt[[s2]], na.rm = TRUE)),
                   "Dl" =  pc(mean( dt[[s3]], na.rm = TRUE)),
   #                "Dp" =  pc(mean( dt[[s4]], na.rm = TRUE)),
   #                "MS" =  pc(mean( dt[[s5]], na.rm = TRUE)),
                   "SA" =  pc(mean( dt[[s6]], na.rm = TRUE)),
   #                "Dr" =  pc(mean( dt[[s7]], na.rm = TRUE)),
  )

}
clc_cmbd_2(K0, "b")
clc_cmbd_2(K0, "a5")
clc_cmbd_2(K1, "b")
clc_cmbd_2(K1, "a5")
clc_cmbd_2(K2a, "b")
clc_cmbd_2(K2a, "a5")
clc_cmbd_2(K2b, "b")
clc_cmbd_2(K2b, "a5")

clc_cmbd_2(K3a, "b")
clc_cmbd_2(K3a, "a5")
clc_cmbd_2(K3b, "b")
clc_cmbd_2(K3b, "a5")

clc_cmbd_2(Dt, "b")
clc_cmbd_2(Dt, "a5")



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
