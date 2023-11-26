######################### -------------------------------    Tool libraries
library(tidyverse)
library(lubridate)
library(gamlss)
library(mgcv)
# library(ellipse)
library(magrittr)
library(rapwhale)  # local
library(pointblank)
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
#
dato_uttrekk <- as.Date("2023-08-03")     ####    overstyr datadump-date
datamappe = paste0(ddmappe, dato_uttrekk, "\\")
avnmappe = paste0(datamappe, "AlleVarNum")
#####  1. read in data       #####                                     ----- 80
#
prc <- function(x) round( 100*x, 1)
setwd(avnmappe)
fil_PBV = paste0("PatBasVarNum.csv")         ## dd-dato = 3. 3.2023                    
fil_Opr = paste0("OperasjonsVarNum.csv")
fil_u6k = paste0("SeksUkerOppfNum.csv")
# k_bok = paste0("SOReg_klokeboken_2023-08-07.csv") # kloke-boka

PBV <-  read_csv2(fil_PBV) # 13344
Opr <-  read_csv2(fil_Opr)
u6k <-  read_csv2(fil_u6k)
# kb <- read_csv2(k_bok)     
AVN <- PBV %>%             # gjenskape AlleVarNum
  left_join(Opr, by = c("p_pasientid", "ForlopsID", "p_opid")) %>% 
  left_join(u6k, by =c("p_pasientid", "ForlopsID", "p_opid"))

fil_Ars =   paste0("SOReg_DatadumpArsrapport_datadump_", dato_uttrekk,".csv")
fil_1 = paste0("SOReg_Arskontrollar1_datadump_", dato_uttrekk,".csv") 
fil_2 = paste0("SOReg_Arskontrollar2_datadump_", dato_uttrekk,".csv") 
fil_5 = paste0("SOReg_Arskontrollar5_datadump_", dato_uttrekk,".csv") 
# klke = read_csv2(k_bok)

setwd(datamappe)
# ------------------------------------- read in Avn data
# AVN  <- read_csv2(fil_AVN)
Ars  <- read_csv2(fil_Ars)   ## alternativ dd Årsrapport
# ---------------------------## ----------------- read in årskontrolldata -- 80
A1 <- read_csv2(fil_1)
A2 <- read_csv2(fil_2)
A5 <- read_csv2(fil_5)
#
setwd(anlzmappe) 
################################################################### join tables
df<-AVN %>% 
  left_join(A1, by=c("p_pasientid","ForlopsID","p_opid") ) %>% 
  left_join(A2, by=c("p_pasientid","ForlopsID","p_opid") ) %>% 
  left_join(A5, by=c("p_pasientid","ForlopsID","p_opid") )
alle_sh <-unique(df$o_sykehus)

dg<-Ars %>% 
  left_join(A1, by=c("PasientID"="p_pasientid","ForlopsID") ) %>%
  left_join(A2, by=c("PasientID"="p_pasientid","ForlopsID") )

# Legg til info om operasjonsC%r, BMI, fedmerel, etc
df  <- df %>% 
  mutate(op_aar = year(o_dato_op),
         op_mnd = month(o_dato_op),
         op_primar = (o_tidl_fedmeop == 0),
         Sex = ifelse( p_kjonn==2, "F", "M")) %>%
  dplyr::select(-p_kjonn)
#  re-kode fedmerelaterte sykdommar,  pakke=dplyr !!   WHICH dd?
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

df$a1_dato_oppf  = as.Date(df$a1_dato_oppf, format(c("%d.%m.%Y")))
df$a2_dato_oppf  = as.Date(df$a2_dato_oppf, format(c("%d.%m.%Y")))
df$a5_dato_oppf  = as.Date(df$a5_dato_oppf, format(c("%d.%m.%Y")))


df<-   df %>% mutate(sc_op1 = as.numeric( a1_dato_oppf- o_dato_op),
                     sc_op2 = as.numeric( a2_dato_oppf- o_dato_op),
                     sc_op5 = as.numeric( a5_dato_oppf- o_dato_op),
                     sc_u6 = as.numeric(u6_oppf_dato-o_dato_op)) 

dat_a1 <- df %>% dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_op1, a1_TWL, a1_AWL)   %>% mutate(ktrl=2) 
dat_a2 <- df %>% dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_op2, a2_TWL, a2_AWL)   %>% mutate(ktrl=3)
dat_a5 <- df %>% dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_op5, a5_TWL, a5_AWL)   %>% mutate(ktrl=5)
dat_u6 <- df %>% dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_u6, u6_ant_kmi_red, u6_AWL)  %>% mutate(ktrl=1)


d <- df %>% 
  dplyr::select(p_pasientid, ForlopsID, o_sykehus, o_dato_op, sc_u6, sc_op1, sc_op2, sc_op5, 
                u6_TWL, a1_TWL, a2_TWL, a5_TWL, a1_AWL, a2_AWL, a5_AWL) %>% 
  filter(!is.na(a5_TWL))  

d <- d %>% mutate(nadir = pmax(a1_TWL, a2_TWL), nd = ifelse(a1_TWL > a2_TWL, 1, 2))

d_1 <-   d %>% filter(!is.na(a1_TWL), is.na(a2_TWL)) %>% mutate(nadir=a1_TWL)
d_2 <-   d %>% filter(is.na(a1_TWL), !is.na(a2_TWL)) %>% mutate(nadir=a2_TWL)
d_b <-    d %>% filter(!is.na(a1_TWL), !is.na(a2_TWL))  # 2989

Dt <-  bind_rows(d_1, d_2, d_b)   # 3181


                       
dat_WL <- d %>% dplyr::select(contains("sc"), contains("TWL"))

   
d1 = tibble(dat_WL$sc_u6, dat_WL$u6_TWL)  
d2 = tibble(dat_WL$sc_op1, dat_WL$a1_TWL) 
d3 = tibble(dat_WL$sc_op2, dat_WL$a2_TWL) 
d4 = tibble(dat_WL$sc_op5, dat_WL$a5_TWL) 
names(d1) <- names(d2) <- names(d3) <- names(d4) <- c("sc","WL")
# 


# The style of the plot itself is fine, but now it’s drawn from a 5K sample. Is it possible to 
# include ALL patients from the SOReg-N 2023-8-3 dataset registry for every time point?

# This same plot only with median and -/+ 1 SD and -/+ 2 SD and all patients from the SOReg-N 2023-8-3 dataset.

 D <- na.omit(bind_rows(d1, d2,d3,d4))   %>% filter(WL> 0, WL<70 )  # all data
 
 ggplot(data = D, aes(x=sc/365, y=WL)) + 
   geom_point(colour="blue") + 
   theme_light()
 
 Dtb = D |> dplyr::mutate(sc  = sc/365)  # transform sc into yr:s
 m0 =  lms(WL, sc , families = c("BCTo"), data = Dtb, k=3, cent = c(2.3, 15.9, 50, 84.1, 97.7))
 
 Dtb$Tsc <-(Dtb$sc)^(m0$power)
 
 # ----------
 IND<-sample.int(12474, 5000, replace=FALSE)   # A 5k sample
 Dk <- D[IND,]
 
 ggplot(data = Dk, aes(x=sc/365, y=WL)) + 
   geom_point(colour="blue") + 
   theme_light()
# --------------------------------------------------------------------
# m0 <- lms(WL, sc, families=c("BCCGo","BCPEo","BCTo"), data=D,
#           k=3, calibration=F, trans.x=T)
 
  Dk = Dk |> dplyr::mutate(sc_y = sc/365)
  m0 =  lms(WL, sc_y , families = c("BCPEo"), data = Dk, k=3, cent = c(2.3, 15.9, 84.1, 97.7))
 
  m0  <-  lms(data=Dk,  WL, sc, families=c("BCCGo"),   # , "BCPEo", "BCTo"
              k=3.5, calibration=F, trans.x=T)                                  # lms makes a centile plot
 
    Dk$Tsc <-(Dk$sc)^(m0$power)
#  
  mod <- quote(gamlss(WL~ cs(Tsc, df=p[1]),
                    sigma.formula=~cs(Tsc, df=p[2]),
                    nu.formula=~cs(Tsc, df=p[3]), c.spar=c(-1.5,2.5),
                    family=BCCGo, data=Dk, control=gamlss.control(trace=FALSE,
                                                                      n.cyc=100, gd.tol=Inf)))

  op <- find.hyper(model = mod, other=quote(Tsc <- sc^p[4]),
                 par=c(3, 1.85, 0.074, 0.08), lower=c(0.1,0.1,0.01,0.01),
                 steps=c(0.07, 0.07, 0.01, 0.005), factr=2e9, parscale=c(1,1,1,1), k=3)

  op$par =c( 9.55666, 1.955283, 0.06910335, 0.09732339)  # crit= 34559.03 with pen= 3 
  op$par =c(  3.98964, 2.057088, 0.07699981, 0.075)
  
  
  T <- (Dk$sc)^(op$par[4])
  MD <- gamlss(data = Dk, WL ~ cs(T, df= op$par[1]),
                       sigma.formula = ~cs(T, df = op$par[2]),
                       nu.formula = ~cs(T, df= op$par[3],
                                        c.spar = c(-1.5,2.5)),
               family = BCCGo , control=gamlss.control(trace=T,
                                                      n.cyc= 40, gd.tol=Inf) ) 
  
  centiles(MD, legend = FALSE,
           xvar=Dk$sc_y,
           cent =c(2.3, 15.9, 50, 84.1, 97.7),
         col = "black",
           xlab="Years since operation",
           ylab="%TWL",
           main= "%TWL: md, +-1SD, +-2SD;  using BCCGo") 
  
 par( col = "black")
  
#  The chosen model can now be fitted by
#  Tage<-(dbbmi1$age)^(op$par[4])
#  m0D<-gamlss(bmi~cs(Tage,df=op$par[1]), sigma.formula=~cs(Tage,
#                                                           df=op$par[2]), nu.formula=~cs(Tage,df=op$par[3],
#                                                                                         c.spar=c(-1.5,2.5)), family=BCCGo, data=dbbmi1)
 ##################
  
#   # k=3?
#   # --------
#   

#  
#  mdl <- gamlss(WL~ cs(sc),    #  ~pb  / ~cs         P-splines / cubic splines?
#               sigma.formula = ~cs(sc),
#               nu.formula =   ~cs(sc),
#               data = D,
#               family = BCCG)
#  
#  # centiles.fan   NO data points
#  
#  centiles(mdl, 
#               xvar=D$sc, 
#           cent =c(2.5,5,10,25,50,75,90,95,97.5),
#               # colors = "terrain",
#               xlab="days since operation",
#               ylab="%TWL")
 # ---------
   IND<-sample.int(7040, 1000, replace=FALSE)
   dbbmi1 <- dbbmi[IND,]
 

 # Tage<-(dbbmi1$age)^(op$par[4])
 # m0D <-gamlss(bmi~cs(Tage,df=op$par[1]), 
 #             sigma.formula=~cs(Tage, df=op$par[2]), 
 #             nu.formula=~cs(Tage,df=op$par[3], 
 #             c.spar=c(-1.5,2.5)), family=BCCGo, data=dbbmi1)
 # 
 # centiles(m0D, 
 #          xvar=dbbmi1$age, 
 #          cent =c(2.5,5,10,25,50,75,90,95,97.5),
 #          # colors = "terrain",
 #          xlab="age",
 #          ylab="BMI")
# --------- =================================================================
 #  Dutch boys BMI-centiles  chp 13.2.1
 #
 m_db <- lms(bmi, age, data=dbbmi1, trans.x=TRUE, k=2)
 dbbmi1$Tage <- (dbbmi1$age)^(m_db$power)
 
 modl <-quote(gamlss(bmi~cs(Tage, df=p[1]),
                   sigma.formula=~cs(Tage, df=p[2]),
                   nu.formula=~cs(Tage, df=p[3]), c.spar=c(-1.5,2.5),
                   family=BCCGo, data=dbbmi1, control=gamlss.control(trace=FALSE,
                                                                     n.cyc=1000,gd.tol=Inf)))
 
 op <- find.hyper(model=modl, other=quote(Tage <- age^p[4]),
                par=c(6,2,2,0.1),lower=c(0.1,0.1,0.1,0.001),
                steps=c(0.1,0.1,0.1,0.005),factr=2e9, parscale=c(1,1,1,1), k=4)
 
Tage<-(dbbmi1$age)^(op$par[4])
 
 m0D<-gamlss(bmi~cs(Tage, df=op$par[1]), 
             sigma.formula=~cs(Tage, df=op$par[2]), 
             nu.formula=~cs(Tage, df=op$par[3], 
                            c.spar=c(-1.5,2.5)), family=BCCGo, data=dbbmi1)
 
 centiles(m0D, 
          xvar=dbbmi1$age, 
          cent =c(2.5,5,10,25,50,75,90,95,97.5),
          # colors = "terrain",
          xlab="age",
          ylab="BMI")
 # ---------
 
 
  

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
