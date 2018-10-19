
library(redcapAPI)
library(dplyr)
library(plyr)
library(survival)
library(ggplot2)
library(survminer)
library(knitr)

rm(list=ls())


#########
#conecting to redcapAPI
#########

source("token.txt")

rcon <- redcapConnection(url=url, token=token)

rm(token)

##########
#Calling our variables 
##########



vector_id <- c("cadast_same", "cadast_idade_anos", "cadast_primeiro_sn", "cadast_data_programa", "contato_respondido_sn", "contato_data", "icc_termino_follow_up", "icc_term_follow_up_mot", "icc_term_follow_up_data", "icc_term_fup_obito_data")

trial_time <-  c("t0_arm_1", "72_horas_arm_1","30_dias_arm_1", "6_meses_arm_1", "1_ano_arm_1" ,"1_ano_e_meio_arm_1", "2_anos_arm_1","2_anos_e_meio_arm_1", "3_anos_arm_1","3_anos_e_meio_arm_1", "4_anos_arm_1", "4_anos_e_meio_arm_1","5_anos_arm_1" , "5_anos_e_meio_arm_1", "6_anos_arm_1","6_anos_e_meio_arm_1" ,"7_anos_arm_1" ,"7_anos_e_meio_arm_1", "8_anos_arm_1" , "8_anos_e_meio_arm_1", "9_anos_arm_1",  "9_anos_e_meio_arm_1", "10_anos_arm_1") 

covariates_baseline  <- c("cadast_genero" , "icc_pd_hos_eco_frac",  "drg_severidade_admissao", "cadast_primeiro_sn")

event_covar <-  c("t0_arm_1")



data_base_all <- exportRecords(rcon,  factors = FALSE,
                        fields = c("record_id", paste(vector_id)),
                        events = trial_time, dates = TRUE)



data_base_all <- data_base_all %>% select("record_id", vector_id)

data_base_num <- mutate_all(data_base_all, funs(as.numeric),1:length(data_base_all))

data_base_num$icc_term_follow_up_mot[is.na(data_base_num$icc_term_follow_up_mot)] <- 666

data_base_num[is.na(data_base_num)] <- 0


#RULES

#data_base_num$cadast_data_programa <- ifelse(data_base_num$cadast_primeiro_sn == 0, 0, #data_base_num$cadast_data_programa)
#data_base_num$icc_term_follow_up_data <- ifelse(data_base_num$icc_term_follow_up_mot!=20, #data_base_num$icc_term_follow_up_data, 0)
#data_base_num$contato_respondido_sn <- ifelse(data_base_num$icc_term_follow_up_mot==20, 0, #data_base_num$contato_respondido_sn)
data_base_num$contato_data <- ifelse(data_base_num$contato_respondido_sn == 0, 0, data_base_num$contato_data) 
 

group_by_record <- ddply(data_base_num, ~record_id , summarise,
               cadast_same = max(cadast_same),
			          cadast_idade_anos = max(cadast_idade_anos),
               cadast_data_programa = max(cadast_data_programa),
               contato_data = max(contato_data),
               icc_term_follow_up_mot = min(icc_term_follow_up_mot),
               icc_term_follow_up_data = max(icc_term_follow_up_data),
               icc_term_fup_obito_data=max(icc_term_fup_obito_data))


#group_by_record$contato_data <- ifelse(group_by_record$icc_term_follow_up_mot==20, 0, #group_by_record$contato_data)



#group_same <- ddply(group_by_record, ~cadast_same, summarise,  
				#cadast_idade_anos = max(cadast_idade_anos),
                #cadast_data_programa = max(cadast_data_programa),
                #contato_data = max(contato_data),
                #icc_term_follow_up_mot = min(icc_term_follow_up_mot),
                #icc_term_follow_up_data = max(icc_term_follow_up_data),
                #icc_term_fup_obito_data=max(icc_term_fup_obito_data))


group_by_record$recent_date <- ifelse(group_by_record$icc_term_fup_obito_data != 0, 							group_by_record$icc_term_fup_obito_data, 
                            pmax(group_by_record$contato_data, group_by_record$icc_term_follow_up_data, group_by_record$icc_term_fup_obito_data))

							

observar <- 0
group_by_record$censored <- ifelse(group_by_record$icc_term_follow_up_mot == observar, 1, 0)


group_same_clean <- group_by_record %>% select(record_id, cadast_idade_anos, cadast_data_programa, recent_date, icc_term_follow_up_mot, censored)


group_same_clean$recent_date <- ifelse(group_same_clean$recent_date == 0, NA, group_same_clean$recent_date)


group_same_clean <- group_same_clean[complete.cases(group_same_clean), ]


group_same_clean$recent_date <- as.Date(as.POSIXct(group_same_clean$recent_date, tz = "GMT", origin = "1970-01-01", format = "%Y-%m-%d"), "Y%m%d")


group_same_clean$cadast_data_programa  <- as.Date(as.POSIXct(group_same_clean$cadast_data_programa , tz = "GMT", origin = "1970-01-01", format = "%Y-%m-%d"), "Y%m%d")


group_same_clean$dias_vivo <- group_same_clean$recent_date - group_same_clean$cadast_data_programa


#############
#OUR STATICIAN RECOMENDATION 
#############

 #median_days          <- round(median(0:max(group_same_clean$dias_vivo, na.rm=TRUE)))

#group_same_clean$age <- cut(group_same_clean$cadast_idade_anos, c(20,60,80,100))


######
#CLEANING THE COVARIATES DATAFRAME
######

covariates_dataset <- exportRecords(rcon,  factors = FALSE,
                      fields = c("record_id", paste(covariates_baseline)),
                      events = event_covar, dates = TRUE)

covariates_dataset <- covariates_dataset %>% select(record_id, paste(covariates_baseline)) %>% group_by(record_id) %>% slice(1L)

data_compare <- merge(group_same_clean, covariates_dataset, by.x="record_id", by.y= "record_id")
#data_compare <- na.omit(data_compare)

cov.cox <- coxph(Surv(data_compare$dias_vivo, data_compare$censored)~ data_compare$cadast_idade_anos + data_compare$cadast_genero + data_compare$icc_pd_hos_eco_frac  +  data_compare$drg_severidade_admissao + data_compare$cadast_primeiro_sn , data =  data_compare)

summary(cov.cox)

# Plot the baseline survival function
ggsurvplot(survfit(cov.cox), color = "#2E9FDF",
           ggtheme = theme_minimal())



################
#KAPLAN-MEIER
################

#icc.fit<- survfit(Surv(group_same_clean$dias_vivo, group_same_clean$censored)~group_same_clean$age, data= #group_same_clean)
#summary(icc.fit)
#p_value <- surv_pvalue(icc.fit, data= data_compare, method = "n")

#ggsurvplot(icc.fit, fun = NULL, color = "strata", palette =  "hue", linetype = 1,
          # break.x.by = 30, surv.scale = c("percent"),
           #conf.int = FALSE, conf.int.fill = "strata", censor = TRUE, pval = TRUE,
           #pval.size = TRUE, pval.coord = c(NULL, NULL), main = "Congestive Heart Failure Survival" , xlab = #"Days",
           #ylab = "Survival probability", font.main = c(16, "plain", "black"),
           #font.x = c(14, "plain", "black"), font.y = c(14, "plain", "black"),
           #font.tickslab = c(12, "plain", "black"), xlim = c(0,248), ylim = c(0.5,1.0),
           #legend = c("right"), legend.title = " ",
           #legend.labs = NULL, font.legend = c(10, "plain", "black"), risk.table = TRUE,
           #risk.table.title = "Number at risk by time", risk.table.col = "strata",
           #risk.table.fontsize = 4.5, risk.table.y.text = TRUE, risk.table.y.text.col = TRUE,
           #risk.table.height = 0.25, surv.plot.height = 0.75, ggtheme = theme_minimal())

