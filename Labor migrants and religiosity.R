### correlations between religiosity (Data2) and husband is a migrant laborer

migrants <- read.csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HHQpeopleDeID.csv")

m <- migrants %>% filter (relationToRespondent=="Husband")



m <- m %>% select (1,3,14:17)
m[is.na(m)] <- 0



m$migrant <- m$laborMigrantNow+m$laborMigrantPast+m$spouseLaborMigrantNow+m$spouseLaborMigrantPast


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

## select identifier, sperate husbands and wives, connect to data1, data2, and data3

#  Link   "id_Questionaire" (from migrants)== "idwife_a"  or  "idhusband" (from data2 or data3)
d1<-data2[c(37,7,2,28,6,8,9,51,52,54,101:103)]

d2<-data3[c(37,7,2,28,6,8,9,51,52,54,101:103)]

women <-left_join (d1,m,by=c("idwife_a"="id_Questionaire"))
w <- women[sapply(women, is.numeric)]

men <- left_join (d2,m,by=c("idwife_a"="id_Questionaire"))
m <- men[sapply(men, is.numeric)]

# correlate religiosity of men and women all the variables
cor(w,use="pairwise.complete.obs")[,"familyBariReligiousAfter"]
summary(w)

cor(m,use="pairwise.complete.obs")[,"familyBariReligiousAfter"]
summary(m)


cor(w,use="pairwise.complete.obs")[,"religious_knowledge_scale"]
summary(w)

cor(m,use="pairwise.complete.obs")[,"religious_knowledge_scale"]
summary(m)