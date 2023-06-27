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

d1<-data1[c(7,28,29,37,38,6,8,9,51,52,54,101:103)] # all
d2<-data2[c(7,28,29,37,38,6,8,9,51,52,54,101:103)] #women
d3<-data3[c(7,28,29,37,38,6,8,9,51,52,54,101:103)]  # men


#d1 <- d[-2]
all <- d1[complete.cases(d1), ] 
women <- d2[complete.cases(d2), ] 
men <- d3[complete.cases(d3), ] 

### make the main figure from the anti secularization model
library(lavaan)
s1 <- 'familyBariReligiousAfter ~ MI_economic_capital2+MI_human_capital2'


s2 <- 'familyBariReligiousAfter ~ MI_economic_capital2+MI_human_capital2
    rels_in_NW ~familyBariReligiousAfter+MI_economic_capital2+MI_human_capital2'
# women
# Regressions:
#                         Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# familyBariReligiousAfter ~                                                      
#   MI_ecnmc_cptl2              0.221    0.025    8.904    0.000    0.221    0.307
# MI_human_cptl2              0.085    0.025    3.356    0.001    0.085    0.116
# rels_in_NW ~                                                                    
#   fmlyBrRlgsAftr              0.868    0.185    4.686    0.000    0.868    0.177
# MI_ecnmc_cptl2              0.379    0.132    2.864    0.004    0.379    0.107
# MI_human_cptl2              0.070    0.130    0.541    0.589    0.070    0.019

#men
# Regressions:
#                           Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# familyBariReligiousAfter ~                                                      
#   MI_ecnmc_cptl2            0.200    0.032    6.317    0.000    0.200    0.278
# MI_human_cptl2              0.093    0.033    2.864    0.004    0.093    0.126
# rels_in_NW ~                                                                    
#   fmlyBrRlgsAftr            0.055    0.250    0.222    0.824    0.055    0.011
# MI_ecnmc_cptl2              0.252    0.179    1.409    0.159    0.252    0.068
# MI_human_cptl2              0.065    0.177    0.365    0.715    0.065    0.017


s3 <- 'familyBariReligiousAfter ~ MI_economic_capital2+MI_human_capital2
    percent_rels_in_NW ~familyBariReligiousAfter+MI_economic_capital2+MI_human_capital2'
# women
# Regressions:
#                             Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# familyBariReligiousAfter ~                                                      
#   MI_ecnmc_cptl2              0.221    0.025    8.904    0.000    0.221    0.307
# MI_human_cptl2                0.085    0.025    3.356    0.001    0.085    0.116
# percent_rels_in_NW ~                                                            
#   fmlyBrRlgsAftr              0.014    0.009    1.523    0.128    0.014    0.059
# MI_ecnmc_cptl2                0.012    0.007    1.740    0.082    0.012    0.067
# MI_human_cptl2               -0.014    0.007   -2.139    0.032   -0.014   -0.078
# 
# # men
# Regressions:
#                              Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# familyBariReligiousAfter ~                                                      
#   MI_ecnmc_cptl2            0.200    0.032    6.317    0.000    0.200    0.278
# MI_human_cptl2              0.093    0.033    2.864    0.004    0.093    0.126
# percent_rels_in_NW ~                                                            
#   fmlyBrRlgsAftr            0.015    0.015    0.982    0.326    0.015    0.047
# MI_ecnmc_cptl2             -0.001    0.011   -0.120    0.905   -0.001   -0.006
# MI_human_cptl2             -0.038    0.011   -3.585    0.000   -0.038   -0.165

s4 <- 'familyBariReligiousAfter ~ MI_economic_capital2+MI_human_capital2
    non_rels ~familyBariReligiousAfter+MI_economic_capital2+MI_human_capital2'
# women
# Regressions:
#                              Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# familyBariReligiousAfter ~                                                      
#   MI_ecnmc_cptl2             0.221    0.025    8.904    0.000    0.221    0.307
# MI_human_cptl2              0.085    0.025    3.356    0.001    0.085    0.116
# non_rels ~                                                                      
#   fmlyBrRlgsAftr           -0.022    0.084   -0.258    0.797   -0.022   -0.010
# MI_ecnmc_cptl2             -0.060    0.060   -1.009    0.313   -0.060   -0.039
# MI_human_cptl2              0.168    0.059    2.853    0.004    0.168    0.105


# men
# Regressions:
#                             Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# familyBariReligiousAfter ~                                                      
#   MI_ecnmc_cptl2            0.200    0.032    6.317    0.000    0.200    0.278
# MI_human_cptl2              0.093    0.033    2.864    0.004    0.093    0.126
# non_rels ~                                                                      
#   fmlyBrRlgsAftr           -0.242    0.238   -1.016    0.309   -0.242   -0.048
# MI_ecnmc_cptl2              0.271    0.170    1.593    0.111    0.271    0.074
# MI_human_cptl2              0.988    0.169    5.849    0.000    0.988    0.263

s5 <- 'familyBariReligiousAfter ~ MI_economic_capital2+MI_human_capital2
    NW_total ~familyBariReligiousAfter+MI_economic_capital2+MI_human_capital2'
 #women
# Regressions:
#                            Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# familyBariReligiousAfter ~                                                      
#   MI_ecnmc_cptl2            0.221    0.025    8.904    0.000    0.221    0.307
# MI_human_cptl2              0.085    0.025    3.356    0.001    0.085    0.116
# NW_total ~                                                                      
#   fmlyBrRlgsAftr            0.846    0.186    4.554    0.000    0.846    0.172
# MI_ecnmc_cptl2              0.318    0.133    2.398    0.016    0.318    0.090
# MI_human_cptl2              0.238    0.130    1.829    0.067    0.238    0.066

# men
# Regressions:
#                             Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# familyBariReligiousAfter ~                                                      
#   MI_ecnmc_cptl2            0.200    0.032    6.317    0.000    0.200    0.278
# MI_human_cptl2              0.093    0.033    2.864    0.004    0.093    0.126
# NW_total ~                                                                      
#   fmlyBrRlgsAftr           -0.183    0.285   -0.641    0.522   -0.183   -0.030
# MI_ecnmc_cptl2              0.528    0.204    2.588    0.010    0.528    0.120
# MI_human_cptl2              1.084    0.202    5.352    0.000    1.084    0.240

fits2_w <- sem(s5, data=women)
summary(fits2_w, standardized=TRUE)
fits2_m <- sem(s5,data=men)
summary(fits2_m, standardized=TRUE)

## compare models s1 and s2 with NW total by including and not including the MI indices and NW size

s1 <- 'familyBariReligiousAfter ~ MI_economic_capital2+MI_human_capital2
    NW_total ~familyBariReligiousAfter+MI_economic_capital2+MI_human_capital2'

s2 <- 'familyBariReligiousAfter ~ MI_economic_capital2+MI_human_capital2
    NW_total ~familyBariReligiousAfter'
one<- cfa(model=s1,data=women)
two <- cfa(model=s2,data=women)

# compare
anova(one,two)
# Chi-Squared Difference Test (men)
# 
#      Df AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)    
# one  0 3469.4 3498.4  0.000                                  
# two  2 3499.5 3520.2 34.075     34.075       2  3.987e-08 ***

# Chi-Squared Difference Test (women)
# 
# Df    AIC  BIC  Chisq Chisq diff Df diff Pr(>Chisq)   
# one  0 5182.6 5215 0.0000                                 
# two  2 5187.9 5211 9.2877     9.2877       2   0.009621 **

## including the relationship between NW size and MI indices is best for men and women
#
s1 <- 'familyBariReligiousAfter ~ MI_economic_capital2+MI_human_capital2
    rels_in_NW ~familyBariReligiousAfter+MI_economic_capital2+MI_human_capital2'

s2 <- 'familyBariReligiousAfter ~ MI_economic_capital2+MI_human_capital2
    rels_in_NW ~familyBariReligiousAfter'
one<- sem(model=s1,data=women)
two <- sem(model=s2,data=men)

summary(one, standardized=TRUE,fit.measures=TRUE)
# compare
anova(one,two)

# Chi-Squared Difference Test (men)
# 
# Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# one  0 3344.4 3373.4 0.0000                              
# two  2 3342.5 3363.3 2.1134     2.1134       2     0.3476

# Chi-Squared Difference Test (women)
# 
#      Df AIC    BIC  Chisq     Chisq diff Df diff Pr(>Chisq)  
# one  0 5177.7 5210.0 0.0000 
# two  2 5182.2 5205.3 8.5367     8.5367       2    0.01401 *


## No difference between models including the relationship between NW size and MI indices or not for men but including is better for women

# comparing non-nested models # aic and ecvi
fitmeasures(one)