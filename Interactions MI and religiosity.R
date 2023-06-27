### Interactions between Market intergration and slef-reported religiosity for men and for women
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


## do men first
library(readr)

d<-data3[c(7,28,8,9,51,52,54,101:103)]

d$NW_total <- as.numeric(d$NW_total)
### try model


library(MASS)
model1 <- glm.nb(NW_total ~ 
                Kids_a+
                Age_a+
                Mothers_kids_a+
                MI_geo_proximity2*familyBariReligiousAfter+
                MI_economic_capital2*familyBariReligiousAfter+
                MI_human_capital2*familyBariReligiousAfter,
              data=d)

summary(model1)
             
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                    2.760769   0.079730  34.627  < 2e-16 ***
#   Kids_a                                         0.024454   0.010072   2.428 0.015186 *  
#   Age_a                                         -0.006662   0.001594  -4.179 2.92e-05 ***
#   Mothers_kids_a                                 0.007826   0.003927   1.993 0.046310 *  
#   MI_geo_proximity2                             -0.034010   0.021131  -1.609 0.107513    
# familyBariReligiousAfter                      -0.010806   0.022572  -0.479 0.632136    
# MI_economic_capital2                           0.072766   0.018530   3.927 8.60e-05 ***
#   MI_human_capital2                              0.077810   0.020386   3.817 0.000135 ***
#   MI_geo_proximity2:familyBariReligiousAfter    -0.054244   0.028893  -1.877 0.060457 .  
# familyBariReligiousAfter:MI_economic_capital2 -0.041111   0.022373  -1.838 0.066129 .  
# familyBariReligiousAfter:MI_human_capital2    -0.016813   0.024294  -0.692 0.488903 




## then do women
d<-data2[c(7,28,8,9,51,52,54,101:103)]

d$NW_total <- as.numeric(d$NW_total)
### try model



library(MASS)
model1 <- glm.nb(NW_total ~ 
                   Kids_a+
                   Age_a+
                   Mothers_kids_a+
                   MI_geo_proximity2*familyBariReligiousAfter+
                   MI_economic_capital2*familyBariReligiousAfter+
                   MI_human_capital2*familyBariReligiousAfter,
                 data=d)

summary(model1)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                    2.254341   0.067767  33.266  < 2e-16 ***
#   Kids_a                                         0.005187   0.008575   0.605   0.5452    
# Age_a                                         -0.003379   0.001388  -2.435   0.0149 *  
#   Mothers_kids_a                                 0.009469   0.004943   1.916   0.0554 .  
# MI_geo_proximity2                             -0.023202   0.020778  -1.117   0.2641    
# familyBariReligiousAfter                       0.095518   0.021366   4.471  7.8e-06 ***
#   MI_economic_capital2                           0.054977   0.017707   3.105   0.0019 ** 
#   MI_human_capital2                              0.029865   0.019825   1.506   0.1320    
# MI_geo_proximity2:familyBariReligiousAfter    -0.043655   0.031169  -1.401   0.1613    
# familyBariReligiousAfter:MI_economic_capital2 -0.024393   0.021808  -1.119   0.2633    
# familyBariReligiousAfter:MI_human_capital2    -0.053320   0.023653  -2.254   0.0242 * 

##More Religious WOMEN who are LESS educated OR LESS RELIGOUS women who are MORE educated have larger social NW's


### NEXT do Number of relatives for men and women

library(readr)

##MEN

d<-data3[c(7,37,8,9,51,52,54,101:103)]

d$rels_in_NW <- as.numeric(d$rels_in_NW)
### try model


library(MASS)
model1 <- glm.nb(rels_in_NW ~ 
                   Kids_a+
                   Age_a+
                   Mothers_kids_a+
                   MI_geo_proximity2*familyBariReligiousAfter+
                   MI_economic_capital2*familyBariReligiousAfter+
                   MI_human_capital2*familyBariReligiousAfter,
                 data=d)

summary(model1)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                    2.0175301  0.1163227  17.344  < 2e-16 ***
#   Kids_a                                         0.0325580  0.0143041   2.276  0.02284 *  
#   Age_a                                         -0.0021491  0.0023027  -0.933  0.35067    
# Mothers_kids_a                                 0.0005775  0.0056993   0.101  0.91929    
# MI_geo_proximity2                             -0.0356189  0.0293986  -1.212  0.22567    
# familyBariReligiousAfter                      -0.0032218  0.0326249  -0.099  0.92133    
# MI_economic_capital2                           0.0377925  0.0270418   1.398  0.16225    
# MI_human_capital2                              0.0617750  0.0298231   2.071  0.03832 *  
#   MI_geo_proximity2:familyBariReligiousAfter    -0.1030151  0.0398699  -2.584  0.00977 ** 
#   familyBariReligiousAfter:MI_economic_capital2 -0.0282635  0.0326945  -0.864  0.38733    
# familyBariReligiousAfter:MI_human_capital2    -0.0881489  0.0359136  -2.454  0.01411 *  

## Men who live closer to markets and are less religious or men who live forther from markets and 
# are more religious have more relatives in their networks

## Men who are more educated and are less religious or men who are less educated and 
# are more religious have more relatives in their networks



## now women
d<-data2[c(7,37,8,9,51,52,54,101:103)]

d$rels_in_NW <- as.numeric(d$rels_in_NW)
### try model


library(MASS)
model1 <- glm.nb(rels_in_NW ~ 
                   Kids_a+
                   Age_a+
                   Mothers_kids_a+
                   MI_geo_proximity2*familyBariReligiousAfter+
                   MI_economic_capital2*familyBariReligiousAfter+
                   MI_human_capital2*familyBariReligiousAfter,
                 data=d)

summary(model1)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                    2.107308   0.076514  27.541  < 2e-16 ***
#   Kids_a                                         0.009296   0.009606   0.968  0.33316    
# Age_a                                         -0.003482   0.001566  -2.224  0.02615 *  
#   Mothers_kids_a                                 0.009374   0.005566   1.684  0.09216 .  
# MI_geo_proximity2                             -0.045744   0.024567  -1.862  0.06261 .  
# familyBariReligiousAfter                       0.110133   0.024139   4.563 5.05e-06 ***
#   MI_economic_capital2                           0.061237   0.019991   3.063  0.00219 ** 
#   MI_human_capital2                              0.016151   0.022520   0.717  0.47325    
# MI_geo_proximity2:familyBariReligiousAfter    -0.086069   0.034505  -2.494  0.01262 *  
#   familyBariReligiousAfter:MI_economic_capital2 -0.010659   0.024487  -0.435  0.66336    
# familyBariReligiousAfter:MI_human_capital2    -0.048954   0.026823  -1.825  0.06799 . 


## WoMen who live closer to markets and are less religious or men who live forther from markets and 
# are more religious have more relatives in their networks

## WoMen who are more educated and are less religious or men who are less educated and 
# are more religious have more relatives in their networks
