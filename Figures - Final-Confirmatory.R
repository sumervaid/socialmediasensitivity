library(ggplot2)
library(tidyverse)
library(lme4)
setwd("~/Desktop/socialmediasensitivity/Social Media Sensitivity Confirmatory Analyses")


main_effect_updated<-read_csv("modelling_data_exploratory_confirmatory.csv")

mainEffectPersonality<-main_effect_updated %>%
  dplyr::select(id, StartDateConvert,  
                affect_balance, 
                demog_age.z,
                MediaUse.scaled, MediaUse.mean,
                demog_sex_recoded,
                weekend, 
                nAct.cz,
                surveys_per_id.z,
                lagDur.cz,
                overall_study_day,
                lagged_affective_balance.cz) %>%
  na.omit() %>% dplyr::distinct()

baseline_affectbalance_use<-lmer(affect_balance ~ 1 +
                                demog_sex_recoded+ 
                                demog_age.z+
                                surveys_per_id.z+
                                overall_study_day+
                                weekend+
                                nAct.cz+
                                lagDur.cz*lagged_affective_balance.cz+
                                MediaUse.scaled+MediaUse.mean+
                                (1+MediaUse.scaled| id),  na.action = na.exclude, data=mainEffectPersonality,
                              REML = TRUE, control = lmerControl(optimizer ="Nelder_Mead"))


coefs_affectbalance_use<-tibble(coef(baseline_affectbalance_use)$id)
ranefs_affectbalance_use<-tibble(ranef(baseline_affectbalance_use)$id)
comb_affectbalance_use<-tibble(rownames(data.frame(coef(baseline_affectbalance_use)$id))) %>%
  dplyr::rename(id=`rownames(data.frame(coef(baseline_affectbalance_use)$id))`)

coefs_affectbalance_use<-cbind(coefs_affectbalance_use, comb_affectbalance_use) 


mainEffectPersonality<-main_effect_updated %>%
  dplyr::select(id, StartDateConvert,  
                affect_balance, 
                demog_age.z,
                socialmedia.cz, socialmedia.bp.z,
                demog_sex_recoded,
                weekend, 
                nAct.cz,
                surveys_per_id.z,
                lagDur.cz,
                overall_study_day,
                lagged_affective_balance.cz) %>%
  na.omit() %>% dplyr::distinct()

baseline_affectbalance_duration<-lmer(affect_balance~ 1 +
                                      demog_sex_recoded+ 
                                      demog_age.z+
                                      surveys_per_id.z+
                                      overall_study_day+
                                      weekend+
                                      nAct.cz+
                                      lagDur.cz*lagged_affective_balance.cz+
                                      socialmedia.cz+socialmedia.bp.z+
                                      (1+socialmedia.cz| id),  na.action = na.exclude, data=mainEffectPersonality,
                                    REML = TRUE, control = lmerControl(optimizer ="Nelder_Mead"))

coefs_affectbalance_duration<-tibble(coef(baseline_affectbalance_duration)$id)
ranefs_affectbalance_duration<-tibble(ranef(baseline_affectbalance_duration)$id)
comb_affectbalance_duration<-tibble(rownames(data.frame(coef(baseline_affectbalance_duration)$id))) %>%
  dplyr::rename(id=`rownames(data.frame(coef(baseline_affectbalance_duration)$id))`)

coefs_affectbalance_duration<-cbind(coefs_affectbalance_duration, comb_affectbalance_duration) 


mainEffect<-main_effect_updated %>%
  dplyr::select(id, StartDateConvert, lonely_r, demog_age.z, MediaUse.scaled, MediaUse.mean,   demog_sex_recoded, weekend, nAct.cz, surveys_per_id.z, lagDur.cz, laggedLonely.cz, overall_study_day) %>%
  na.omit() %>% dplyr::distinct()

baseline_lonely_use<-lmer(lonely_r ~ 1 + demog_sex_recoded+    
                             demog_age.z+
                             surveys_per_id.z+
                             overall_study_day+
                             weekend+
                             nAct.cz+
                             lagDur.cz*laggedLonely.cz+
                             MediaUse.scaled+MediaUse.mean+ 
                             (1+MediaUse.scaled| id),  na.action = na.exclude, data=mainEffect,
                           REML = TRUE, control = lmerControl(optimizer ="Nelder_Mead"))

coefs_lonely_use<-tibble(coef(baseline_lonely_use)$id)
ranefs_lonely_use<-tibble(ranef(baseline_lonely_use)$id)
comb_lonely_use<-tibble(rownames(data.frame(coef(baseline_lonely_use)$id))) %>%
  dplyr::rename(id=`rownames(data.frame(coef(baseline_lonely_use)$id))`)

coefs_lonely_use<-cbind(coefs_lonely_use, comb_lonely_use) 

mainEffect<-main_effect_updated %>%
  dplyr::select(id, StartDateConvert, lonely_r, demog_age.z, socialmedia.cz, socialmedia.bp.z, demog_sex_recoded, weekend, nAct.cz, surveys_per_id.z, lagDur.cz, laggedLonely.cz, overall_study_day) %>%
  na.omit() %>% dplyr::distinct()

baseline_lonely_duration<-lmer(lonely_r ~ 1 + demog_sex_recoded+    
                                   demog_age.z+
                                   surveys_per_id.z+
                                   overall_study_day+
                                   weekend+
                                   nAct.cz+
                                   lagDur.cz*laggedLonely.cz+
                                   socialmedia.cz+socialmedia.bp.z+
                                   (1+socialmedia.cz| id),  na.action = na.exclude, data=mainEffect,
                                 REML = TRUE, control = lmerControl(optimizer ="Nelder_Mead"))

coefs_lonely_duration<-tibble(coef(baseline_lonely_duration)$id)
ranefs_lonely_duration<-tibble(ranef(baseline_lonely_duration)$id)
comb_lonely_duration<-tibble(rownames(data.frame(coef(baseline_lonely_duration)$id))) %>%
  dplyr::rename(id=`rownames(data.frame(coef(baseline_lonely_duration)$id))`)

coefs_lonely_duration<-cbind(coefs_lonely_duration, comb_lonely_duration) 

mainEffectPersonality<-main_effect_updated %>%
  dplyr::select(id, StartDateConvert, 
                accepted, 
                demog_age.z,
                MediaUse.scaled, MediaUse.mean,
                demog_sex_recoded,
                weekend, 
                nAct.cz,
                surveys_per_id.z,
                lagDur.cz,
                overall_study_day,
                lagged_accepted.cz) %>%
  na.omit() %>% dplyr::distinct()

baseline_accepted_use<-lmer(accepted ~ 1 +
                                demog_sex_recoded+ 
                                demog_age.z+
                                surveys_per_id.z+
                                overall_study_day+
                                weekend+
                                nAct.cz+
                                lagDur.cz*lagged_accepted.cz+
                                MediaUse.scaled+MediaUse.mean+
                                (1+MediaUse.scaled| id),  na.action = na.exclude, data=mainEffectPersonality,
                              REML = TRUE, control = lmerControl(optimizer ="Nelder_Mead"))

coefs_accepted_use<-tibble(coef(baseline_accepted_use)$id)
ranefs_accepted_use<-tibble(ranef(baseline_accepted_use)$id)
comb_accepted_use<-tibble(rownames(data.frame(coef(baseline_accepted_use)$id))) %>%
  dplyr::rename(id=`rownames(data.frame(coef(baseline_accepted_use)$id))`)

coefs_accepted_use<-cbind(coefs_accepted_use, comb_accepted_use) 

mainEffectPersonality<-main_effect_updated %>%
  dplyr::select(id, StartDateConvert, 
                accepted, 
                demog_age.z,
                socialmedia.cz, socialmedia.bp.z,
                demog_sex_recoded,
                weekend, 
                nAct.cz,
                surveys_per_id.z,
                lagDur.cz,
                overall_study_day,
                lagged_accepted.cz) %>%
  na.omit() %>% dplyr::distinct()

baseline_accepted_duration<-lmer(accepted~ 1 +
                                      demog_sex_recoded+ 
                                      demog_age.z+
                                      surveys_per_id.z+
                                      overall_study_day+
                                      weekend+
                                      nAct.cz+
                                      lagDur.cz*lagged_accepted.cz+
                                      socialmedia.cz+socialmedia.bp.z+
                                      (1+socialmedia.cz| id),  na.action = na.exclude, data=mainEffectPersonality,
                                    REML = TRUE, control = lmerControl(optimizer ="Nelder_Mead"))

coefs_accepted_duration<-tibble(coef(baseline_accepted_duration)$id)
ranefs_accepted_duration<-tibble(ranef(baseline_accepted_duration)$id)
comb_accepted_duration<-tibble(rownames(data.frame(coef(baseline_accepted_duration)$id))) %>%
  dplyr::rename(id=`rownames(data.frame(coef(baseline_accepted_duration)$id))`)

coefs_accepted_duration<-cbind(coefs_accepted_duration, comb_accepted_duration) 

mainEffect<-main_effect_updated %>%
  dplyr::select(id, StartDateConvert, stressed_r, demog_age.z, MediaUse.scaled, MediaUse.mean,   demog_sex_recoded, weekend, nAct.cz, surveys_per_id.z, lagDur.cz, laggedStress.cz, overall_study_day) %>%
  na.omit() %>% dplyr::distinct()

baseline_stressed_use<-lmer(stressed_r ~ 1 + demog_sex_recoded+    
                               demog_age.z+
                               surveys_per_id.z+
                               overall_study_day+
                               weekend+
                               nAct.cz+
                               lagDur.cz*laggedStress.cz+
                               MediaUse.scaled+MediaUse.mean+ 
                               (1+MediaUse.scaled| id),  na.action = na.exclude, data=mainEffect,
                             REML = TRUE, control = lmerControl(optimizer ="Nelder_Mead"))

coefs_stressed_use<-tibble(coef(baseline_stressed_use)$id)
ranefs_stressed_use<-tibble(ranef(baseline_stressed_use)$id)
comb_stressed_use<-tibble(rownames(data.frame(coef(baseline_stressed_use)$id))) %>%
  dplyr::rename(id=`rownames(data.frame(coef(baseline_stressed_use)$id))`)

coefs_stressed_use<-cbind(coefs_stressed_use, comb_stressed_use) 

mainEffect<-main_effect_updated %>%
  dplyr::select(id, StartDateConvert, stressed_r, demog_age.z, socialmedia.cz, socialmedia.bp.z, demog_sex_recoded, weekend, nAct.cz, surveys_per_id.z, lagDur.cz, laggedStress.cz, overall_study_day) %>%
  na.omit() %>% dplyr::distinct()

baseline_stressed_duration<-lmer(stressed_r ~ 1 + demog_sex_recoded+    
                                     demog_age.z+
                                     surveys_per_id.z+
                                     overall_study_day+
                                     weekend+
                                     nAct.cz+
                                     lagDur.cz*laggedStress.cz+
                                     socialmedia.cz+socialmedia.bp.z+
                                     (1+socialmedia.cz| id),  na.action = na.exclude, data=mainEffect,
                                   REML = TRUE, control = lmerControl(optimizer ="Nelder_Mead"))


coefs_stressed_duration<-tibble(coef(baseline_stressed_duration)$id)
ranefs_stressed_duration<-tibble(ranef(baseline_stressed_duration)$id)
comb_stressed_duration<-tibble(rownames(data.frame(coef(baseline_stressed_duration)$id))) %>%
  dplyr::rename(id=`rownames(data.frame(coef(baseline_stressed_duration)$id))`)

coefs_stressed_duration<-cbind(coefs_stressed_duration, comb_stressed_duration)

coefs_stressed_use$wb<-"Stress"
coefs_stressed_use$sm<-"Use"

coefs_stressed_use<- coefs_stressed_use %>%
  dplyr::select(id, MediaUse.scaled, wb, sm) %>%
  dplyr::rename(
    socialmedia=MediaUse.scaled
  )

coefs_accepted_use$wb<-"Accepted"
coefs_accepted_use$sm<-"Use"

coefs_accepted_use<- coefs_accepted_use %>%
  dplyr::select(id, MediaUse.scaled, wb, sm) %>%
  dplyr::rename(
    socialmedia=MediaUse.scaled
  )

coefs_lonely_use$wb<-"Lonely"
coefs_lonely_use$sm<-"Use"

coefs_lonely_use<- coefs_lonely_use %>%
  dplyr::select(id, MediaUse.scaled, wb, sm) %>%
  dplyr::rename(
    socialmedia=MediaUse.scaled
  )

coefs_affectbalance_use$wb<-"Affect Balance"
coefs_affectbalance_use$sm<-"Use"

coefs_affectbalance_use<- coefs_affectbalance_use %>%
  dplyr::select(id, MediaUse.scaled, wb, sm) %>%
  dplyr::rename(
    socialmedia=MediaUse.scaled
  )


coefs_stressed_duration$wb<-"Stress"
coefs_stressed_duration$sm<-"Duration"

coefs_stressed_duration<- coefs_stressed_duration %>%
  dplyr::select(id, socialmedia.cz, wb, sm) %>%  dplyr::rename(
    socialmedia=socialmedia.cz
  )


coefs_accepted_duration$wb<-"Accepted"
coefs_accepted_duration$sm<-"Duration"

coefs_accepted_duration<- coefs_accepted_duration %>%
  dplyr::select(id, socialmedia.cz, wb, sm) %>%
  dplyr::rename(
    socialmedia=socialmedia.cz
  )


coefs_lonely_duration$wb<-"Lonely"
coefs_lonely_duration$sm<-"Duration"

coefs_lonely_duration<- coefs_lonely_duration %>%
  dplyr::select(id, socialmedia.cz, wb, sm) %>%
  dplyr::rename(
    socialmedia=socialmedia.cz
  )


coefs_affectbalance_duration$wb<-"Affect Balance"
coefs_affectbalance_duration$sm<-"Duration"

coefs_affectbalance_duration<- coefs_affectbalance_duration %>%
  dplyr::select(id, socialmedia.cz, wb, sm) %>%
  dplyr::rename(
    socialmedia=socialmedia.cz
  )


data<-rbind(coefs_stressed_use, coefs_accepted_use, coefs_affectbalance_use, coefs_lonely_use,
            coefs_stressed_duration, coefs_accepted_duration, coefs_affectbalance_duration, coefs_lonely_duration)  

data$sm<-factor(data$sm)

data$sm<-recode_factor(data$sm, "Use"="Use (Vs Non-Use)", "Duration"="Duration")

library(ggridges)

ggplot(data, aes(x = socialmedia, y = wb, group = wb, fill=wb)) + 
  geom_density_ridges2(scale = 1.5, alpha = 0.7, size = 0.1, rel_min_height = 0.01) + facet_wrap(~sm, scales="free_x") + theme(strip.text.x = element_text(size=40)) + theme_ridges() + 
  coord_cartesian(xlim = c(-0.19, 0.18)) + theme_apa() + 
  labs(x=expression(atop("β Coefficients"), atop("Person-Specific Coeffecients"))) + ylab("Psychological Wellbeing") + 
  theme (panel.border = element_blank(), axis.text=element_text(size=40), axis.title.x=element_text(size=40), strip.text.x = element_text(size=40),
         axis.title.y = element_text(size=40, margin = margin(t = 0, r = 20, b = 0, l = 0))) + 
  scale_discrete_manual("fill", values=c("#E6A0C4", "#F3DF6C","#F98400", "#972D15" )) + theme(legend.position = "none")

ggsave("Figure1-Confirmatory.png", width = 21, height=21)
