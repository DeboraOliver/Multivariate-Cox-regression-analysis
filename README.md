 Multivariate-Cox-regression-analysis
 
## Overview

The purpose of the Cox model is to evaluate simultaneously the effect of several factors on survival. It is essentially a regression model commonly used in medical research for investigating the association between the survival time of patients and one or more predictor variables. In other words, it allows us to examine how specified factors influence the rate of a particular event happening (e.g., infection, death) at a particular point in time.

## 1. Requirements

To run this project you need the following libraries:


```{r libraries, message=FALSE, warning=FALSE}
library(redcapAPI)
library(dplyr)
library(plyr)
library(survival)
library(ggplot2)
library(survminer)
```

### i) redcapAPI

In this project, we use data collected using redcap to further use redcap API to establish a connection between the two softwares. Despite recapConnection(), this package also has other very useful tools, such as exportRecords() which allows to import to RStudio our patient's data.

```{r rcon, message=FALSE, warning=FALSE, paged.print=FALSE}
source("token.txt")

rcon <- redcapConnection(url=url, token=token)

rm(token)
```

Both token and url must be provided by your institution.

### ii) plyr and dplyr

These two libraries can be used mainly to clean and group the data from our source. From package plyr, ddply() is used o group patients by record_id and same. Although, it seems too repetitive, a patient can have only one same and many entries along the trial. The following code gives an example of how ddply() is implemented:

```{r grouping, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE}

group_by_record <- ddply(data_base_num, ~record_id , summarise,
                        cadast_same = max(cadast_same),
			                  cadast_idade_anos = max(cadast_idade_anos),
                        cadast_data_programa = max(cadast_data_programa),
                        contato_data = max(contato_data),
                        icc_term_follow_up_mot = min(icc_term_follow_up_mot),
                        icc_term_follow_up_data = max(icc_term_follow_up_data),
                        icc_term_fup_obito_data=max(icc_term_fup_obito_data))
```


### iii) survival

The survival package allows us to use both functions coxph(), Surv() and survfit(). Surv() function contains failure time and censoring information. It also provides the basic survival analysis data structure.

```{r cox, echo=FALSE, message=FALSE, warning=FALSE}

cov.cox <- coxph(Surv(data_compare$dias_vivo, data_compare$censored)~ data_compare$cadast_idade_anos + data_compare$cadast_genero + data_compare$icc_pd_adm_nyha_q01 + data_compare$icc_pd_hos_eco_frac  +  data_compare$drg_severidade_admissao + data_compare$cadast_primeiro_sn , data =  data_compare)

summary(cov.cox)

```

For the coxph() function, to indicate that the model is significant, the p-value for all three overall tests (likelihood, Wald, and score) are significant (p<0.05). 

## 2. Further Explanation

http://www.sthda.com/english/wiki/cox-proportional-hazards-model
