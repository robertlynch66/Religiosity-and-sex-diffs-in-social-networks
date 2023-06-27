library(tidyverse)
library(brms)
library(readr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)                             # Install & load scales
library("scales")

husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new2.csv", header = T, sep = ",")

husbands_new$percent_rels_in_NW<- as.numeric(husbands_new$percent_rels_in_NW)

husbands_new$religious_knowledge_scale<- scales::rescale(husbands_new$religious_knowledge_scale,to=c(-1,1))



data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")

## make dataset with sex as (husband or wife) link data 2 to data 3
# data2 <- data2 %>% select (2,3,7:10,25:53,57:59)
# 
# data3 <- data3 %>% select(2,3,7,28:50,54:56,59)
# 
# #rename data 3 file Age_a, "NW_total"   
# 
# data4 <- rename(data3, Age_a_m = Age_a,
#        NW_total_m= NW_total,
#        non_rels_m= non_rels,
#        parents_kids_m=parents_kids,                     
#        pat_rels_m=pat_rels,
#        mat_rels_m=mat_rels,                           
#        in_laws_m=in_laws,
#        far_rels_m=far_rels,                          
#        geo_distance_non_rels_m=geo_distance_non_rels,
#        geo_distance_rels_m=geo_distance_rels,                  
#        rels_in_NW_m=rels_in_NW,
#        percent_rels_in_NW_m=percent_rels_in_NW,
#        rels_econ_help_m=rels_econ_help,               
#        non_rels_econ_help_m=non_rels_econ_help,
#        percent_rels_econ_help_m=percent_rels_econ_help,
#        emot_support_rels_m=emot_support_rels,                  
#        emot_support_non_rels_m=emot_support_non_rels,
#        percent_rels_emot_support_m=percent_rels_emot_support,          
#        childcare_work_help_rels_m=childcare_work_help_rels,
#        childcare_work_help_non_rels_m=childcare_work_help_non_rels,       
#        childcare_work_help_rels_percent_m=childcare_work_help_rels_percent,
#        overall_help_rels_m=overall_help_rels, 
#        overall_help_non_rels_m=overall_help_non_rels,
#        percent_overall_help_rels_m=percent_overall_help_rels,
#        Birth_hh_size_m=Birth_hh_size) 
# 
# ## left join data2 to data4
# combined <- data2 %>% left_join(data4, by=c("idwife_a"="idwife_a"))

### either used combined or data 1 (probably data1) to run models using sex to predict Socil
# NW outcomes (add sex to model below)

library(readr)

d<-data1[c(7,2,28,6,8,9,51,52,54,101:103)]

d$NW_total <- as.numeric(d$NW_total)
### try model

  
  
model1 <- brm(NW_total ~ 
                Kids_a+
                Age_a+
                Mothers_kids_a+
                MI_geo_proximity2+
                MI_economic_capital2+
                MI_human_capital2+
                gender_F0_M1_a*familyBariReligiousAfter+
                gender_F0_M1_a*religious_knowledge_scale+
                (1|religion)+
                (1|idwife_a), 
              data=d, 
              family=negbinomial(link = "log", link_shape = "log"),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model1, prob=0.95,priors=TRUE), digits = 6)
tab_model(model1)

# Population-Level Effects: 
#                                             Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 2.232555  0.507942  1.153635  3.332888 1.005138
# Kids_a                                    0.011145  0.006777 -0.002259  0.024533 0.999983
# Age_a                                    -0.004826  0.001059 -0.006893 -0.002725 1.000122
# Mothers_kids_a                            0.008263  0.003117  0.002120  0.014338 0.999933
# MI_geo_proximity2                        -0.011887  0.013062 -0.038070  0.013151 1.000377
# MI_economic_capital2                      0.046205  0.011965  0.022336  0.069543 1.000424
# MI_human_capital2                         0.031301  0.013989  0.003440  0.058629 1.000124
# gender_F0_M1_a                            0.643607  0.169997  0.312260  0.982335 1.000311
# familyBariReligiousAfter                  0.098878  0.021065  0.057760  0.140105 1.000797
# religious_knowledge_scale                -0.036275  0.129980 -0.291358  0.214557 1.000063
# gender_F0_M1_a:familyBariReligiousAfter  -0.101442  0.029816 -0.159160 -0.043214 1.000087
# gender_F0_M1_a:religious_knowledge_scale  0.275204  0.191638 -0.101750  0.655049 1.000332

# 1) Women have smaller NW's than men
# 2) More religious (SELF-REPORTED) households have larger networks overall -- especially women
setwd("C:/Users/robert/Dropbox/Github/Religiosity_sex_diffs_and_social_networks")
path<- (paste0("Results/"))
filename <- "NW_total_neg_binom_sex.rds"

saveRDS(model1, paste0(path, filename))

############################################################
############################################################
############################################################
############################################################


### Percent relatives run
d <- data1[c(2,7,38,6,8,9,52,51,54,53,101:103)] 
# 
d$percent_rels_in_NW <- as.numeric(d$percent_rels_in_NW)
d <- d[complete.cases(d), ] 
d$percent_rels_in_NW <- (d$percent_rels_in_NW - min(d$percent_rels_in_NW) + 0.001)/(max(d$percent_rels_in_NW) - min(d$percent_rels_in_NW) + 0.002)
### get distributions

## run as log normal  
model2 <- brm(percent_rels_in_NW ~ #religious_belief+
                gender_F0_M1_a*familyBariReligiousAfter+
                gender_F0_M1_a*religious_knowledge_scale+
                Kids_a+
                Age_a+
                Mothers_kids_a+
                MI_geo_proximity2+
                MI_economic_capital2+
                MI_human_capital2+
                (1|religion)+
                (1|idwife_a), 
              data=d, 
              family="beta",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))



print(summary(model2, prob=0.95,priors=TRUE), digits = 6)
tab_model(model2)

# men and women

# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 1.077407  0.740975 -0.608404  2.466678 1.002511
# gender_F0_M1_a                           -2.191917  0.513918 -3.205872 -1.202081 1.000414
# familyBariReligiousAfter                  0.045024  0.066095 -0.088240  0.173408 1.001951
# religious_knowledge_scale                -0.725983  0.406589 -1.502347  0.098543 1.001917
# Kids_a                                    0.028035  0.021130 -0.012952  0.069144 1.001625
# Age_a                                     0.005233  0.003347 -0.001392  0.011796 1.000848
# Mothers_kids_a                           -0.009062  0.010038 -0.028753  0.010406 1.001049
# MI_geo_proximity2                        -0.016822  0.038940 -0.091434  0.058251 1.003787
# MI_economic_capital2                      0.006636  0.038961 -0.068332  0.083912 1.000404
# MI_human_capital2                         0.006525  0.043972 -0.079193  0.091633 1.000812
# gender_F0_M1_a:familyBariReligiousAfter   0.101522  0.095162 -0.085039  0.288501 1.001110
# gender_F0_M1_a:religious_knowledge_scale -0.673133  0.577941 -1.810157  0.446699 1.000420

# 1)  Females have a higher percentage of relatives in their NW's than men
# 2) *Less religiously knowledge people have more kin dense social networks


#  men only
# Percent  Rels in NW:
#         # familyBariReligiousAfter                 0.043178  -0.019192  0.105203 
#         # religious_knowledge_scale_men           -0.009809  -0.027292  0.007367 
#         # MI_geo_proximity                           0.012798  -0.032001  0.057797 
#         # MI_economic_capital                       -0.036178  -0.082384  0.011327 
#         # MI_human_capital                         -0.082990  -0.139924 -0.025870
#   

setwd("C:/Users/robert/Dropbox/Github/Religiosity_sex_diffs_and_social_networks")
path<- (paste0("Results/"))
filename <- "percent_relatives_in_NW_beta_sex.rds"
saveRDS(model2, paste0(path, filename))

# Model 2.1 Relatives in Network

library(tidyverse)
library(brms)
library(readr)

d <- data1[c(2,7,37,6,8,9,52,51,54,53,101:103)] 
d <- d[complete.cases(d), ] 


## run as log normal  
model2.1 <- brm(rels_in_NW ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Age_a+
                  Mothers_kids_a+
                  MI_geo_proximity2+
                  MI_economic_capital2+
                  MI_human_capital2+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family=negbinomial(link = "log", link_shape = "log"),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model2.1, prob=0.95,priors=TRUE), digits = 6)
tab_model(model2.1)

# Population-Level Effects: 
#                                             Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 2.061576  0.539339  0.955766  3.225365 1.027801
# gender_F0_M1_a                           -0.162293  0.245723 -0.625278  0.304518 1.028845
# familyBariReligiousAfter                  0.122151  0.024043  0.074025  0.167996 1.007955
# religious_knowledge_scale                -0.047293  0.154690 -0.352923  0.254851 1.012420
# Kids_a                                    0.014530  0.008272 -0.001269  0.030934 1.005295
# Age_a                                    -0.003451  0.001322 -0.006073 -0.000855 1.007548
# Mothers_kids_a                            0.004528  0.004099 -0.002847  0.012608 1.018624
# MI_geo_proximity2                        -0.004389  0.015801 -0.036282  0.024819 1.009290
# MI_economic_capital2                      0.044978  0.014525  0.016736  0.073861 1.013140
# MI_human_capital2                         0.003579  0.016838 -0.029563  0.037488 1.024701
# gender_F0_M1_a:familyBariReligiousAfter  -0.106397  0.036169 -0.179291 -0.034675 1.014017
# gender_F0_M1_a:religious_knowledge_scale -0.209986  0.276992 -0.732158  0.315686 1.029683

#1) More self-reportedly religious families have more relatives in their networks
#2) More self-reportedly religious women have more relatives in their NW's, no effect for men

path<- (paste0("results/"))
filename <- "relatives_in_NW_neg_binom_sex.rds"

saveRDS(model2.1, paste0(path, filename))


# Model 2.2 Non-relatives in Network
library(tidyverse)
library(brms)
library(readr)


d <- data1[c(2,7,29,6,8,9,52,51,54,53,101:103)] 
d <- d[complete.cases(d), ] 

#d$non_rels<- d$non_rels+0.01
## run as Negative bionomial
model2.2 <- brm(non_rels ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Age_a+
                  Mothers_kids_a+
                  MI_geo_proximity2+
                  MI_economic_capital2+
                  MI_human_capital2+
                  (1|religion)+
                  (1|idwife_a), 
                data=d,
                family = negbinomial(link = "log", link_shape = "identity"),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model2.2, prob=0.95,priors=TRUE), digits = 6)
tab(model2.2)

# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 0.728031  0.618679 -0.548823  2.090851 1.001507
# gender_F0_M1_a                            1.872226  0.394887  1.108197  2.640560 1.000109
# familyBariReligiousAfter                 -0.038659  0.065930 -0.168769  0.091458 1.000723
# religious_knowledge_scale                 0.378294  0.330040 -0.281165  1.023382 1.000602
# Kids_a                                   -0.006205  0.019695 -0.045029  0.032628 1.000555
# Age_a                                    -0.009395  0.003070 -0.015429 -0.003342 0.999932
# Mothers_kids_a                            0.014616  0.008214 -0.001602  0.030516 1.000100
# MI_geo_proximity2                        -0.016415  0.036644 -0.088498  0.054447 0.999987
# MI_economic_capital2                      0.036083  0.033499 -0.029627  0.101420 1.000240
# MI_human_capital2                         0.090954  0.037824  0.017025  0.163702 1.000375
# gender_F0_M1_a:familyBariReligiousAfter   0.013128  0.080087 -0.144792  0.168883 1.000549
# gender_F0_M1_a:religious_knowledge_scale  0.313789  0.445862 -0.556485  1.183233 1.000076

#1) males have more non-relatives in their NW's
#2) more educated people have more non-relatives in their networks

path<- (paste0("results/"))
filename <- "non_relatives_in_NW_neg_bin_sex.rds"

saveRDS(model2.2, paste0(path, filename))

#########################################################################
# Geographic location models
## Distance from non-relatives
#Husband's first
HusbandNW<- read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HusQnetworkDeID.csv")

# key variables are location and relationship
HusbandNW$relationship <- as.numeric(HusbandNW$relationship)
plyr::count(HusbandNW$relationship)


HusbandNW$relationship[is.na(HusbandNW$relationship)]<- 99
HusbandNW$location[HusbandNW$location == 0] <- NA
HusbandNW$location[HusbandNW$location >5 ] <- NA
plyr::count(HusbandNW$location)

## add id_wife to z
z <- HusbandNW %>% dplyr::select (1,2,3,5,7)

# get location of non relatives
nr <- z %>% filter (relationship==0)
r <- z %>% filter (relationship>0 & relationship<8)

# get non relatives
# join non-rels to data3 (d)
non_rels <- nr %>% left_join (data3, by=c("id_Questionaire"="idhusband"))
#non_rels <- non_rels[complete.cases(non_rels),]

non_rels$location[non_rels$location==1|non_rels$location==2] <- 2
non_rels$location[non_rels$location==3] <- 3
non_rels$location[non_rels$location==4|non_rels$location==5] <- 4
# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad
non_rels<- non_rels[c(1,2,4,5,9,10,11,12,13,58,55,105,106,107,56)]

## Wives next
WifeNW <-  read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HHQPeopleinNW.csv")

# key variables are location and relationship
WifeNW$relationship <- as.numeric(WifeNW$relationship)
plyr::count(WifeNW$relationship)
WifeNW$relationship[is.na(WifeNW$relationship)]<- 99
WifeNW$location[WifeNW$location == 0] <- NA
WifeNW$location[WifeNW$location >5 ] <- NA
plyr::count(WifeNW$location)


z <- WifeNW %>% dplyr::select (2,3,6,8)

# get location of non relatives
nr <- z %>% filter (relationship==0)
r <- z %>% filter (relationship>0 & relationship<8)

# get non relatives
# join non-rels to data (d)
non_rels2 <- nr %>% left_join (data2, by=c("id_Questionaire"="idwife_a"))
#non_rels2 <- non_rels2[complete.cases(non_rels2),]

non_rels2$location[non_rels2$location==1|non_rels2$location==2] <- 2
non_rels2$location[non_rels2$location==3] <- 3
non_rels2$location[non_rels2$location==4|non_rels2$location==5] <- 4

non_rels2<- non_rels2[c(1,2,3,4,8,9,10,11,12,57,54,104:106,55)]
### join non-rels to nonrels2 

d <- rbind(non_rels,non_rels2)
# Make model in brms  
library(brms)
model3.1 <- brm(data = d, 
                family = cumulative("logit"),
                location ~ 1+
                  gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  MI_geo_proximity2+
                  MI_economic_capital2+
                  MI_human_capital2 +
                  Age_a+
                  Kids_a+
                  Mothers_kids_a+
                  familyBariReligiousAfter+
                  (1|religion)+
                  (1|id_Questionaire),
                prior = c(prior(normal(0, 1.5), class = Intercept),
                          prior(normal(0, 0.5), class = b)),
                iter = 5000, warmup = 1000, cores = 4, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(model3.1)
# Population-Level Effects: 
#                                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# Intercept[1]                               -10.32      1.05   -12.36    -8.25 1.00     8909
# Intercept[2]                                 1.11      0.90    -0.54     3.02 1.00     8760
# Intercept[3]                                 2.70      0.91     1.05     4.61 1.00     8723
# gender_F0_M1_a                               0.58      0.35    -0.10     1.25 1.00    13168
# familyBariReligiousAfter                     0.50      0.22     0.08     0.93 1.00    10258
# religious_knowledge_scale                    0.10      0.46    -0.81     0.99 1.00    19139
# MI_geo_proximity2                            0.09      0.14    -0.20     0.36 1.00    12880
# MI_economic_capital2                         0.06      0.13    -0.19     0.32 1.00     7116
# MI_human_capital2                            0.43      0.14     0.16     0.70 1.00     5889
# Age_a                                       -0.03      0.01    -0.05    -0.00 1.00     6100
# Kids_a                                      -0.07      0.08    -0.23     0.10 1.00     6871
# Mothers_kids_a                              -0.03      0.03    -0.10     0.03 1.00     6422
# gender_F0_M1_a:familyBariReligiousAfter     -0.23      0.26    -0.74     0.29 1.00     6913
# gender_F0_M1_a:religious_knowledge_scale     0.03      0.38    -0.72     0.76 1.00    13489
path<- (paste0("results/"))
filename <- "Geo_distance_non_relatives_ord_cum_sex.rds"

saveRDS(model3.1, paste0(path, filename))


#1) Religiosity predicts greater distance between non-relatives
#2) Education predicts greater distance between non-relatives
#########################################################################
# Geographic location models
## Distance from relatives
#Husband's first
HusbandNW<- read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HusQnetworkDeID.csv")

# key variables are location and relationship
HusbandNW$relationship <- as.numeric(HusbandNW$relationship)
plyr::count(HusbandNW$relationship)


HusbandNW$relationship[is.na(HusbandNW$relationship)]<- 99
HusbandNW$location[HusbandNW$location == 0] <- NA
HusbandNW$location[HusbandNW$location >5 ] <- NA
plyr::count(HusbandNW$location)

## add id_wife to z
z <- HusbandNW %>% dplyr::select (1,2,3,5,7)

# get location of non relatives
nr <- z %>% filter (relationship==0)
r <- z %>% filter (relationship>0 & relationship<8)

# get non relatives
# join non-rels to data3 (d)
rels <- r %>% left_join (data3, by=c("id_Questionaire"="idhusband"))
#non_rels <- non_rels[complete.cases(non_rels),]

rels$location[rels$location==1|rels$location==2] <- 2
rels$location[rels$location==3] <- 3
rels$location[rels$location==4|rels$location==5] <- 4
# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad
rels<- rels[c(1,2,4,5,9,10,11,12,13,58,55,105:107,56)]

## Wives next
WifeNW <-  read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HHQPeopleinNW.csv")

# key variables are location and relationship
WifeNW$relationship <- as.numeric(WifeNW$relationship)
plyr::count(WifeNW$relationship)
WifeNW$relationship[is.na(WifeNW$relationship)]<- 99
WifeNW$location[WifeNW$location == 0] <- NA
WifeNW$location[WifeNW$location >5 ] <- NA
plyr::count(WifeNW$location)


z <- WifeNW %>% dplyr::select (2,3,6,8)

# get location of non relatives
nr <- z %>% filter (relationship==0)
r <- z %>% filter (relationship>0 & relationship<8)

# get non relatives
# join non-rels to data (d)
rels2 <- r %>% left_join (data2, by=c("id_Questionaire"="idwife_a"))
#non_rels2 <- non_rels2[complete.cases(non_rels2),]

rels2$location[rels2$location==1|rels2$location==2] <- 2
rels2$location[rels2$location==3] <- 3
rels2$location[rels2$location==4|rels2$location==5] <- 4

rels2<- rels2[c(1,2,3,4,8,9,10,11,12,57,54,104:106,55)]
### join non-rels to nonrels2 

d <- rbind(rels,rels2)
# Make model in brms  
library(brms)
model3.2 <- brm(data = d, 
                family = cumulative("logit"),
                location ~ 1+
                  Age_a+
                  gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  MI_geo_proximity2+MI_economic_capital2+
                  MI_human_capital2 +
                  Kids_a+
                  Mothers_kids_a+
                  familyBariReligiousAfter+
                  (1|religion)+
                  (1|id_Questionaire),
                prior = c(prior(normal(0, 1.5), class = Intercept),
                          prior(normal(0, 0.5), class = b)),
                iter = 5000, warmup = 1000, cores = 4, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(model3.2)
# Population-Level Effects: 
#                                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# Intercept[1]                                -8.05      0.90    -9.67    -6.10 1.00     7744
# Intercept[2]                                 1.49      0.82     0.27     3.39 1.00     6208
# Intercept[3]                                 2.25      0.82     1.02     4.15 1.00     6207
# Age_a                                        0.00      0.00    -0.00     0.01 1.00    10609
# gender_F0_M1_a                               0.42      0.30    -0.16     1.01 1.00    10169
# familyBariReligiousAfter                     0.15      0.07     0.01     0.29 1.00     9141
# religious_knowledge_scale                    1.17      0.33     0.53     1.83 1.00    12981
# MI_geo_proximity2                            0.02      0.04    -0.07     0.10 1.00    11885
# MI_economic_capital2                         0.03      0.04    -0.06     0.11 1.00    10492
# MI_human_capital2                            0.07      0.05    -0.03     0.16 1.00     9133
# Kids_a                                       0.05      0.02     0.01     0.10 1.00     9676
# Mothers_kids_a                               0.01      0.01    -0.01     0.04 1.00    10818
# gender_F0_M1_a:familyBariReligiousAfter     -0.12      0.11    -0.33     0.10 1.00     8669
# gender_F0_M1_a:religious_knowledge_scale    -0.01      0.33    -0.66     0.65 1.00    10162
path<- (paste0("results/"))
filename <- "Geo_distance_relatives_ord_cum_sex.rds"

saveRDS(model3.2, paste0(path, filename))

#1) More Religious people live further from kin in their NW's
#2) More religiously knowledgeable people live further from kin in their NW's

#### type of help received models
library(tidyverse)
library(brms)
library(readr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)                             # Install & load scales
library("scales")
husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new2.csv", header = T, sep = ",")

husbands_new$percent_rels_in_NW<- as.numeric(husbands_new$percent_rels_in_NW)

husbands_new$religious_knowledge_scale<- scales::rescale(husbands_new$religious_knowledge_scale,to=c(-1,1))

###HERE!!!

data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")

### Economic help
# model 4 percent relatives who provide economic help
library(tidyverse)
library(brms)
library(readr)


d <- data1[c(41,2,6,7,8,9,52,51,54,53,101:103)] 
# 
d$percent_rels_econ_help <- as.numeric(d$percent_rels_econ_help)
d <- d[complete.cases(d), ] 
d$percent_rels_econ_help  <- (d$percent_rels_econ_help  - min(d$percent_rels_econ_help ) + 0.001)/(max(d$percent_rels_econ_help ) - min(d$percent_rels_econ_help ) + 0.002)

### try model
model4 <- brm(percent_rels_econ_help ~ gender_F0_M1_a*familyBariReligiousAfter+
                gender_F0_M1_a*religious_knowledge_scale+
                Kids_a+
                Age_a+
                Mothers_kids_a+
                MI_geo_proximity2+
                MI_economic_capital2+
                MI_human_capital2+
                (1|religion)+
                (1|idwife_a), 
              data=d, 
              family="beta",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))

print(summary(model4, prob=0.95,priors=TRUE), digits = 6)
# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI u-95% CI     Rhat
# Intercept                                 0.790365  0.614754 -0.474821 2.051806 1.010154
# gender_F0_M1_a                           -0.537491  0.663031 -1.827727 0.748638 1.030828
# familyBariReligiousAfter                  0.058554  0.073529 -0.088524 0.193012 1.010921
# religious_knowledge_scale                -0.542355  0.499443 -1.422590 0.578150 1.036711
# Kids_a                                    0.038681  0.024309 -0.009422 0.084865 1.011842
# Age_a                                    -0.005309  0.004052 -0.013016 0.002561 1.021171
# Mothers_kids_a                           -0.006521  0.011961 -0.030854 0.017226 1.009514
# MI_geo_proximity2                         0.042949  0.043671 -0.042631 0.130657 1.005824
# MI_economic_capital2                     -0.017764  0.043856 -0.103046 0.069900 1.009749
# MI_human_capital2                        -0.078678  0.052407 -0.185467 0.023872 1.012360
# gender_F0_M1_a:familyBariReligiousAfter   0.009748  0.112529 -0.207641 0.238233 1.002923
# gender_F0_M1_a:religious_knowledge_scale -0.217291  0.749504 -1.687128 1.236431 1.031951

path<- (paste0("Results/"))
filename <- "Percent_rels_econ_help_beta_sex.rds"

saveRDS(model4, paste0(path, filename))

## 1) No effect of religion on percent relatives in NW

### Number of relatives economic help
library(tidyverse)
library(brms)
library(readr)

d <- data1[c(2,7,39,6,8,9,52,51,54,53,101:103)] 
d <- d[complete.cases(d), ] 
d$rels_econ_help <- as.integer(d$rels_econ_help)
#d$rels_econ_help<- d$rels_econ_help+0.01
## run as log normal  
model4.1 <- brm(rels_econ_help ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Age_a+
                  Mothers_kids_a+
                  MI_geo_proximity2+
                  MI_economic_capital2+
                  MI_human_capital2+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = negbinomial(link = "log", link_shape = "identity"),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model4.1, prob=0.95,priors=TRUE), digits = 6)

#           Population-Level Effects: 
#                                            Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 2.032263  0.825832  0.238249  3.809848 1.000983
# gender_F0_M1_a                           -0.098006  0.436701 -0.942283  0.753470 1.000066
# familyBariReligiousAfter                 -0.006965  0.047106 -0.099494  0.085700 1.000089
# religious_knowledge_scale                 0.385830  0.328906 -0.261279  1.026183 1.000100
# Kids_a                                    0.011652  0.016519 -0.020288  0.043978 1.000809
# Age_a                                    -0.014479  0.002561 -0.019482 -0.009444 1.000285
# Mothers_kids_a                            0.005466  0.007765 -0.009761  0.020577 1.000018
# MI_geo_proximity2                         0.009775  0.029339 -0.047993  0.066483 1.000287
# MI_economic_capital2                      0.036114  0.029208 -0.021577  0.093495 0.999879
# MI_human_capital2                        -0.069787  0.033342 -0.136366 -0.004443 1.000339
# gender_F0_M1_a:familyBariReligiousAfter   0.090110  0.073257 -0.053046  0.235331 1.000215
# gender_F0_M1_a:religious_knowledge_scale -0.072157  0.491571 -1.017196  0.889102 1.000023


# No effect of religion on  number of rels offering economic help
path<- (paste0("Results/"))
filename <- "econ_help_rels_sex.rds"

saveRDS(model4.1, paste0(path, filename))
### Number of non-relatives economic help
library(tidyverse)
library(brms)
library(readr)

d <- data1[c(2,40,6,7,8,9,52,51,54,53,101:103)] 
d <- d[complete.cases(d), ] 

d$non_rels_econ_help<- as.integer(d$non_rels_econ_help)
## run as log normal  
model4.2 <- brm(non_rels_econ_help ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Age_a+
                  Mothers_kids_a+
                  MI_geo_proximity2+
                  MI_economic_capital2+
                  MI_human_capital2+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = negbinomial(link = "log", link_shape = "identity"),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model4.2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 0.243185  1.057604 -1.976896  2.208258 1.002386
# gender_F0_M1_a                            1.147443  0.821384 -0.449859  2.756206 1.000929
# familyBariReligiousAfter                 -0.216029  0.143983 -0.502489  0.066349 1.000779
# religious_knowledge_scale                 0.934733  0.728154 -0.417184  2.444653 1.000066
# Kids_a                                   -0.081687  0.051249 -0.183260  0.017750 1.001222
# Age_a                                    -0.016781  0.007749 -0.032102 -0.001764 1.000236
# Mothers_kids_a                            0.023770  0.021162 -0.017580  0.065160 1.001666
# MI_geo_proximity2                        -0.096372  0.126484 -0.346742  0.148288 1.000127
# MI_economic_capital2                      0.119001  0.083344 -0.041435  0.281018 1.000690
# MI_human_capital2                         0.158189  0.093112 -0.025446  0.339802 1.000250
# gender_F0_M1_a:familyBariReligiousAfter   0.195022  0.195129 -0.180219  0.585360 1.000462
# gender_F0_M1_a:religious_knowledge_scale -0.352273  0.919489 -2.140596  1.454358 1.000634

path<- (paste0("Results/"))
filename <- "econ_help_non_rels_sex.rds"

saveRDS(model4.2, paste0(path, filename))
###percent relatives who provide childcare/ work help
d <- data1[c(47,2,6,7,8,9,52,51,54,53,101:103)] 
# 
d$childcare_work_help_rels_percent <- as.numeric(d$childcare_work_help_rels_percent)
d <- d[complete.cases(d), ] 
d$childcare_work_help_rels_percent  <- (d$childcare_work_help_rels_percent -min(d$childcare_work_help_rels_percent)+ 0.001)/(max(d$childcare_work_help_rels_percent) - min(d$childcare_work_help_rels_percent)+ 0.002)

### try model
model5 <- brm(childcare_work_help_rels_percent ~ gender_F0_M1_a*familyBariReligiousAfter+
                gender_F0_M1_a*religious_knowledge_scale+
                Age_a+
                Kids_a+
                Mothers_kids_a+
                MI_geo_proximity2+
                MI_economic_capital2+
                MI_human_capital2+
                (1|religion)+
                (1|idwife_a), 
              data=d, 
              family="beta",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))

print(summary(model5, prob=0.95,priors=TRUE), digits = 6)
#         Population-Level Effects: 
#                                            Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 1.047462  0.743347 -0.593217  2.437531 1.001645
# gender_F0_M1_a                           -1.890970  0.658991 -3.202043 -0.603627 1.000327
# familyBariReligiousAfter                  0.049744  0.071903 -0.092283  0.192281 1.000519
# religious_knowledge_scale                -0.151937  0.508139 -1.094183  0.893970 1.000102
# Age_a                                     0.001036  0.004146 -0.007070  0.009106 1.000109
# Kids_a                                    0.013700  0.025950 -0.036877  0.064801 1.000204
# Mothers_kids_a                            0.011216  0.013059 -0.014171  0.036308 1.000242
# MI_geo_proximity2                         0.014872  0.055003 -0.093108  0.125337 1.000746
# MI_economic_capital2                      0.031145  0.046104 -0.058752  0.120661 1.000155
# MI_human_capital2                        -0.162743  0.053509 -0.266851 -0.057331 1.000585
# gender_F0_M1_a:familyBariReligiousAfter  -0.002646  0.126475 -0.250459  0.243842 1.000388
# gender_F0_M1_a:religious_knowledge_scale -1.337133  0.742218 -2.815174  0.105413 1.000396
path<- (paste0("Results/"))
filename <- "Percent_childcare_work_help_sex.rds"

saveRDS(model5, paste0(path, filename))
### Number of relatives childcare/ work help
d <- data1[c(2,7,45,6,8,9,52,51,54,53,101:103)] 
d <- d[complete.cases(d), ] 
d$childcare_work_help_rels <- as.integer(d$childcare_work_help_rels)

## run as neg binom 
model5.1 <- brm(childcare_work_help_rels ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Age_a+
                  Mothers_kids_a+
                  MI_geo_proximity2+
                  MI_economic_capital2+
                  MI_human_capital2+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = negbinomial(link = "log", link_shape = "identity"),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model5.1, prob=0.95,priors=TRUE), digits = 6)
# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 1.577410  0.584876  0.306034  2.797720 1.003306
# gender_F0_M1_a                           -1.560661  0.462870 -2.521493 -0.669213 1.001148
# familyBariReligiousAfter                  0.001195  0.042636 -0.086261  0.084937 1.000500
# religious_knowledge_scale                -0.056092  0.282280 -0.612859  0.489446 1.000208
# Kids_a                                    0.003505  0.015786 -0.027984  0.034393 1.000289
# Age_a                                    -0.010644  0.002527 -0.015615 -0.005705 1.000411
# Mothers_kids_a                            0.015254  0.007834 -0.000062  0.030577 1.000395
# MI_geo_proximity2                        -0.120366  0.042020 -0.205678 -0.040992 1.000399
# MI_economic_capital2                      0.073376  0.027708  0.019974  0.127888 1.000589
# MI_human_capital2                        -0.120076  0.032829 -0.184442 -0.056327 1.000437
# gender_F0_M1_a:familyBariReligiousAfter   0.003084  0.075136 -0.143098  0.157195 1.001346
# gender_F0_M1_a:religious_knowledge_scale -1.026621  0.517131 -2.097020 -0.018862 1.001098
#1) Females get more help for childcare from relatives than males do for work

path<- (paste0("Results/"))
filename <- "Childcare_work_help_rels_sex.rds"

saveRDS(model5.1, paste0(path, filename))

### Number of non-relatives childcare/ work help


d <- data1[c(2,7,46,6,8,9,52,51,54,53,101:103)] 
d <- d[complete.cases(d), ] 
d$childcare_work_help_non_rels<-as.integer(d$childcare_work_help_non_rels)

model5.2 <- brm(childcare_work_help_non_rels ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Age_a+
                  Mothers_kids_a+
                  MI_geo_proximity2+
                  MI_economic_capital2+
                  MI_human_capital2+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = negbinomial(link = "log", link_shape = "identity"),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model5.2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                -0.278431  1.176220 -2.623120  2.080896 1.000560
# gender_F0_M1_a                            2.600678  0.958683  0.724461  4.520219 0.999956
# familyBariReligiousAfter                 -1.004780  0.267484 -1.549458 -0.493105 1.000528
# religious_knowledge_scale                 0.960750  0.924165 -0.807097  2.862872 1.000409
# Kids_a                                    0.066461  0.076209 -0.082140  0.216259 1.000236
# Age_a                                    -0.034536  0.011561 -0.057536 -0.012164 1.000246
# Mothers_kids_a                           -0.007991  0.029733 -0.065735  0.051030 1.000016
# MI_geo_proximity2                        -0.090957  0.196962 -0.485786  0.303616 1.000277
# MI_economic_capital2                      0.004930  0.126437 -0.242358  0.253184 1.000328
# MI_human_capital2                         0.366896  0.133929  0.104785  0.630289 1.000112
# gender_F0_M1_a:familyBariReligiousAfter   0.940508  0.315547  0.322884  1.574924 1.000677
# gender_F0_M1_a:religious_knowledge_scale -0.316940  1.078828 -2.453479  1.831158 1.000025e

#1) Less religious people have more non-rels who help with work and childcare -- especially 
# less religious women (??)
path<- (paste0("Results/"))
filename <- "Childcare_work_help_non_rels_sex.rds"

saveRDS(model5.2, paste0(path, filename))

###percent relatives who provide emotional support
d <- data1[c(44,7,2,6,8,9,52,51,54,53,101:103)] 
# 
d$percent_rels_emot_support <- as.numeric(d$percent_rels_emot_support )
d <- d[complete.cases(d), ] 

d$percent_rels_emot_support  <- (d$percent_rels_emot_support  -min(d$percent_rels_emot_support ) +0.001)/(max(d$percent_rels_emot_support)-min(d$percent_rels_emot_support)+0.002)
                                
### try model
model6 <- brm(percent_rels_emot_support~ gender_F0_M1_a*familyBariReligiousAfter+
                gender_F0_M1_a*religious_knowledge_scale+
                Kids_a+
                Age_a+
                Mothers_kids_a+
                MI_geo_proximity2+
                MI_economic_capital2+
                MI_human_capital2+
                (1|religion)+
                (1|idwife_a), 
              data=d, 
              family="beta",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))

print(summary(model6, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI u-95% CI     Rhat
# Intercept                                 2.017826  0.675997  0.466554 3.258210 1.001050
# gender_F0_M1_a                           -1.093778  0.554063 -2.182337 0.001798 1.000212
# familyBariReligiousAfter                  0.058511  0.064870 -0.067143 0.187384 1.000227
# religious_knowledge_scale                 0.044255  0.381751 -0.679453 0.826985 1.000104
# Kids_a                                   -0.010769  0.021518 -0.052821 0.032031 1.000758
# Age_a                                     0.004953  0.003398 -0.001607 0.011579 0.999959
# Mothers_kids_a                           -0.010968  0.010849 -0.032147 0.010435 1.000616
# MI_geo_proximity2                         0.040061  0.040370 -0.034939 0.123614 1.000082
# MI_economic_capital2                     -0.064354  0.039772 -0.142172 0.013962 1.000101
# MI_human_capital2                        -0.049165  0.045818 -0.138875 0.040057 1.000242
# gender_F0_M1_a:familyBariReligiousAfter  -0.044516  0.098156 -0.238268 0.145907 1.000462
# gender_F0_M1_a:religious_knowledge_scale -0.530315  0.621705 -1.747430 0.692087 1.000251
path<- (paste0("Results/"))
filename <- "Percent_emotional_support_sex.rds"

saveRDS(model6, paste0(path, filename))

#1) Women get a higher percentage of emotional support from relatives

### Number of relatives who provide emotional support
d <- data1[c(42,2,6,7,8,9,52,51,54,53,101:103)] 
d <- d[complete.cases(d), ] 
d$emot_support_rels <- as.integer(d$emot_support_rels)
## run as log normal  
model6.1 <- brm(emot_support_rels~gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Age_a+
                  Mothers_kids_a+
                  MI_geo_proximity2+
                  MI_economic_capital2+
                  MI_human_capital2+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = negbinomial(link = "log", link_shape = "identity"),
                #family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model6.1, prob=0.95,priors=TRUE), digits = 6)
#       Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 1.797467  0.569655  0.470585  3.047227 1.004549
# gender_F0_M1_a                           -0.092283  0.303161 -0.664693  0.496575 1.003238
# familyBariReligiousAfter                  0.177126  0.032227  0.113380  0.240348 1.001215
# religious_knowledge_scale                 0.218027  0.206835 -0.188633  0.627556 1.004850
# Kids_a                                   -0.019816  0.011557 -0.041397  0.002882 1.003424
# Age_a                                     0.001640  0.001798 -0.001832  0.005109 1.003041
# Mothers_kids_a                            0.008577  0.005308 -0.001925  0.018906 1.001143
# MI_geo_proximity2                        -0.002309  0.020828 -0.043400  0.038639 1.002378
# MI_economic_capital2                      0.017164  0.021064 -0.022773  0.065379 1.010405
# MI_human_capital2                        -0.003374  0.023016 -0.047955  0.041498 1.001491
# gender_F0_M1_a:familyBariReligiousAfter  -0.197415  0.050307 -0.292556 -0.097148 1.002968
# gender_F0_M1_a:religious_knowledge_scale -0.149864  0.341486 -0.795143  0.511443 1.003169


#1) More religious people get more emotional support from rels -- especially women
filename <- "emotional_support_rels_sex.rds"

saveRDS(model6.1, paste0(path, filename))
### Number of non-relatives who provide emotional support
d <- data1[c(43,2,6,7,8,9,52,51,54,53,101:103)] 
d <- d[complete.cases(d), ] 
d$emot_support_non_rels <- as.integer(d$emot_support_non_rels)
## run as log normal  
model6.2 <- brm(emot_support_non_rels ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Age_a+
                  Mothers_kids_a+
                  MI_geo_proximity2+
                  MI_economic_capital2+
                  MI_human_capital2+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = negbinomial(link = "log", link_shape = "identity"),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model6.2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                -0.616459  1.086560 -2.846691  1.440953 1.000783
# gender_F0_M1_a                            1.865082  0.859406  0.197170  3.583745 1.000302
# familyBariReligiousAfter                 -0.152356  0.173159 -0.492361  0.191424 1.000099
# religious_knowledge_scale                 0.998332  0.877094 -0.714971  2.717674 1.000300
# Kids_a                                    0.086406  0.052533 -0.017268  0.191356 1.000042
# Age_a                                    -0.022259  0.008782 -0.039722 -0.005024 1.000227
# Mothers_kids_a                            0.039470  0.021520 -0.002899  0.081062 1.000277
# MI_geo_proximity2                        -0.127901  0.135454 -0.408036  0.124709 0.999929
# MI_economic_capital2                      0.126329  0.090516 -0.050623  0.305529 1.000593
# MI_human_capital2                         0.322737  0.099618  0.129974  0.520988 0.999966
# gender_F0_M1_a:familyBariReligiousAfter   0.211169  0.210334 -0.202362  0.621408 0.999895
# gender_F0_M1_a:religious_knowledge_scale -0.584472  0.975548 -2.479978  1.344834 1.000368


#1) Men get more emotional support from no-rels
# no effect of religion
path<- (paste0("Results/"))
filename <- "emotional_support_non_rels_sex.rds"

saveRDS(model6.2, paste0(path, filename))


### get location summary statistics
#%m5 <- non_rels %>% group_by(id_Questionaire) %>% 
  