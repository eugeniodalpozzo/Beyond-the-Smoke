library(dplyr)
library(ggplot2)
install.packages("sfsmisc")
library(sfsmisc)
library(car)
library(tidyr)
install.packages("leaps")
library(leaps)
library(broom)
install.packages("corrplot")
library(corrplot)

options(scipen= 999 )



library(readxl)
Can_2017 <- read_excel("~/Desktop/Can_2017.xlsx", 
                       col_types = c("text", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))
View(Can_2017)
str(Can_2017)
summary(Can_2017)

#riclassify age-var in 9 categories
Can_2017$DVAGE1 <- cut(Can_2017$DVAGE,
                       breaks = c(0, 17, 19, 22, 24, 34, 44, 54, 64, Inf),
                       labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                       include.lowest = TRUE)



names(Can_2017)[names(Can_2017)== "DVAGE"] <- "DA_ELIMINARE" 

Can_2017 <- Can_2017[,-which(names(Can_2017)== "DA_ELIMINARE")]
names(Can_2017)[names(Can_2017)== "DVAGE1"] <- "DVAGE" 


#riclassify other var
Can_2017$MARRIAGE[Can_2017$MARRIAGE == 9] <- NA
#
Can_2017$EDU[Can_2017$EDU == 97] <- NA
Can_2017$EDU[Can_2017$EDU == 98] <- NA
Can_2017$EDU[Can_2017$EDU == 99] <- NA
#
Can_2017$HDSIZE[Can_2017$HDSIZE == 6] <- NA
#
Can_2017$MHS[Can_2017$MHS == 7] <- NA
Can_2017$MHS[Can_2017$MHS == 8] <- NA
#
Can_2017$ALC[Can_2017$ALC == 99] <- NA
#
Can_2017$DPR[Can_2017$DPR %in% c(6, 7, 8, 9)] <- NA
#
Can_2017$STI[Can_2017$STI %in% c(7, 8, 9)] <- NA
#
Can_2017$SED[Can_2017$SED %in% c(7, 8, 9)] <- NA
#
Can_2017$CAN[Can_2017$CAN %in% c(9)] <- NA
#
Can_2017$COC[Can_2017$COC %in% c(7, 8, 9)] <- NA
#
Can_2017$MET[Can_2017$MET %in% c(7, 8, 9)] <- NA
#
Can_2017$XTC[Can_2017$XTC %in% c(7, 8, 9)] <- NA
#
Can_2017$HAL[Can_2017$HAL %in% c(7, 8, 9)] <- NA
#
Can_2017$GLU[Can_2017$GLU %in% c(7, 8, 9)] <- NA
#
Can_2017$HER[Can_2017$HER %in% c(7, 8, 9)] <- NA
#
Can_2017$SAL[Can_2017$SAL %in% c(7, 8, 9)] <- NA
#
Can_2017$DAFRI1[Can_2017$DAFRI1 %in% c(6, 8, 9)] <- NA
#
Can_2017$DAPHY[Can_2017$DAPHY %in% c(6, 8, 9)] <- NA
#
Can_2017$DAMAR[Can_2017$DAMAR %in% c(6, 8, 9)] <- NA
#
Can_2017$DAWK[Can_2017$DAWK %in% c(6, 7, 8, 9)] <- NA
#
Can_2017$DAFIN[Can_2017$DAFIN %in% c(6, 7, 8, 9)] <- NA
#
Can_2017$DALEG[Can_2017$DALEG %in% c(6, 8, 9)] <- NA
#
Can_2017$DAHOU[Can_2017$DAHOU %in% c(6, 8, 9)] <- NA
#
Can_2017$DALEA[Can_2017$DALEA %in% c(6, 7, 8, 9)] <- NA
#
Can_2017$SMOKE[Can_2017$SMOKE %in% c(6, 7, 8, 9)] <- NA
#
Can_2017$JOB[Can_2017$JOB %in% c(7, 8, 9)] <- NA
#
Can_2017$ABS[Can_2017$ABS %in% c(6, 7, 8, 9)] <- NA
#
Can_2017$PHARABUS[Can_2017$PHARABUS %in% c(9)] <- NA
#
Can_2017$CAN12M[Can_2017$CAN12M %in% c(9)] <- NA
#
Can_2017$RISK[Can_2017$RISK %in% c(9)] <- NA
#
Can_2017$DVURBAN[Can_2017$DVURBAN %in% c(9)] <- NA


#

#Import dataset Can_2019
library(readxl)
Can_2019 <- read_excel("~/Desktop/Can_2019.xlsx", 
                       col_types = c("text", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))

View(Can_2019)






#modify "ALC"
Can_2019$ALC <- case_when(Can_2019$ALC %in% c(9, 8, 7) ~ 6, Can_2019$ALC == 6 ~ 5, TRUE ~ Can_2019$ALC)

table(Can_2019$ALC)
table(Can_2017$ALC)

#riclassify var
Can_2019$MARRIAGE[Can_2019$MARRIAGE == 9] <- NA
#
Can_2019$EDU[Can_2019$EDU == 99] <- NA
#
Can_2019$HDSIZE[Can_2019$HDSIZE == 9] <- NA
#
Can_2019$MHS[Can_2019$MHS == 9] <- NA
#
#Can_2019$ALC[Can_2019$ALC == 99] <- NA
#
Can_2019$DPR[Can_2019$DPR %in% c(9)] <- NA
#
Can_2019$STI[Can_2019$STI %in% c(9)] <- NA
#
Can_2019$SED[Can_2019$SED %in% c(9)] <- NA
#
Can_2019$CAN[Can_2019$CAN %in% c(9)] <- NA
#
Can_2019$COC[Can_2019$COC %in% c(9)] <- NA
#
Can_2019$MET[Can_2019$MET %in% c(9)] <- NA
#
Can_2019$XTC[Can_2019$XTC %in% c(9)] <- NA
#
Can_2019$HAL[Can_2019$HAL %in% c(9)] <- NA
#
Can_2019$GLU[Can_2019$GLU %in% c(9)] <- NA
#
Can_2019$HER[Can_2019$HER %in% c(9)] <- NA
#
Can_2019$SAL[Can_2019$SAL %in% c(9)] <- NA
#
Can_2019$DAFRI1[Can_2019$DAFRI1 %in% c(6, 9)] <- NA
#
Can_2019$DAPHY[Can_2019$DAPHY %in% c(6, 9)] <- NA
#
Can_2019$DAMAR[Can_2019$DAMAR %in% c(6, 9)] <- NA
#
Can_2019$DAWK[Can_2019$DAWK %in% c(6, 9)] <- NA
#
Can_2019$DAFIN[Can_2019$DAFIN %in% c(6, 9)] <- NA
#
Can_2019$DALEG[Can_2019$DALEG %in% c(6, 9)] <- NA
#
Can_2019$DAHOU[Can_2019$DAHOU %in% c(6, 9)] <- NA
#
Can_2019$DALEA[Can_2019$DALEA %in% c(6, 7, 8, 9)] <- NA
#
Can_2019$SMOKE[Can_2019$SMOKE %in% c(9)] <- NA
#
Can_2019$JOB[Can_2019$JOB %in% c(9)] <- NA
#
Can_2019$ABS[Can_2019$ABS %in% c(6, 9)] <- NA
#
Can_2019$PHARABUS[Can_2019$PHARABUS %in% c(9)] <- NA
#
Can_2019$CAN12M[Can_2019$CAN12M %in% c(9)] <- NA
#
Can_2019$RISK[Can_2019$RISK %in% c(9)] <- NA
#
Can_2019$DVURBAN[Can_2019$DVURBAN %in% c(9)] <- NA
###
#data merged
CADS_df <- rbind(Can_2017, Can_2019)
View(CADS_df)
###
#create dummy for treatment 2017==0
CADS_df$TREATMENT <- rep(NA,nrow(CADS_df))
CADS_df$TREATMENT <- ifelse(CADS_df$REFYEAR==2017, 0, 1)
#
View(CADS_df)
CADS_df$MHS[CADS_df$MHS == 1] <- "A"
CADS_df$MHS[CADS_df$MHS == 2] <- "B"
CADS_df$MHS[CADS_df$MHS == 3] <- "C"
CADS_df$MHS[CADS_df$MHS == 4] <- "D"
CADS_df$MHS[CADS_df$MHS == 5] <- "E"
CADS_df$MHS[CADS_df$MHS == 6] <- "F"
CADS_df$MHS[CADS_df$MHS == "A"] <- 5
CADS_df$MHS[CADS_df$MHS == "B"] <- 4
CADS_df$MHS[CADS_df$MHS == "C"] <- 3
CADS_df$MHS[CADS_df$MHS == "D"] <- 2
CADS_df$MHS[CADS_df$MHS == "E"] <- 1


CADS_df$CAN12M<-ifelse(CADS_df$CAN12M==1,1,0)
CADS_df$COC<-ifelse(CADS_df$COC==1,1,0)
CADS_df$MET<-ifelse(CADS_df$MET==1,1,0)
CADS_df$STI<-ifelse(CADS_df$STI==1,1,0)
CADS_df$SED<-ifelse(CADS_df$SED==1,1,0)
CADS_df$DPR<-ifelse(CADS_df$DPR==1,1,0)
CADS_df$XTC<-ifelse(CADS_df$XTC==1,1,0)
CADS_df$HAL<-ifelse(CADS_df$HAL==1,1,0)
CADS_df$GLU<-ifelse(CADS_df$GLU==1,1,0)
CADS_df$HER<-ifelse(CADS_df$HER==1,1,0)
CADS_df$SAL<-ifelse(CADS_df$SAL==1,1,0)
View(CADS_df)

#

#TOTAL REG

CADS_fct <- as.data.frame(lapply(CADS_df, as.factor))
str(CADS_fct)

modello_logit <- glm(CAN12M ~  SEX+MARRIAGE+EDU+HDSIZE+ALC+DPR+STI+SED+MHS+COC+MET+XTC+HAL+GLU+HER+SAL+SMOKE+DVAGE+DVURBAN+TREATMENT, data = CADS_fct, family = "binomial")
summary(modello_logit)


#modello_logit <- glm(CAN12M ~  SEX+MARRIAGE+EDU+HDSIZE+ALC+DPR+DAPHY+DAMAR+DAWK+DAFIN+DALEG+DAHOU+DALEA+SMOKE+JOB+PHARABUS+DVAGE+TREATMENT, data = CADS_fct, family = "binomial")
#summary(modello_logit)


modello_logit1 <- glm(MHS ~ SEX+MARRIAGE+EDU+HDSIZE+ALC+CAN12M+DPR+STI+SED+COC+MET+XTC+HAL+GLU+HER+SAL+SMOKE+DVAGE+DVURBAN+TREATMENT, data = CADS_fct, family = "binomial")
summary(modello_logit1)


###coef as odds-ratio



odds_ratios <- exp(coef(modello_logit1))
se_log_odds <- summary(modello_logit1)$coefficients[, "Std. Error"]
se_odds_ratio <- odds_ratios*se_log_odds
p_values <- summary(modello_logit1)$coefficients[, "Pr(>|z|)"]
results <- data.frame(OddsRatio= odds_ratios, StdError = se_odds_ratio, PValue = p_values)
print(results)

###PLOT

coefficients <- coef(modello_logit1)

coef_data <- data.frame(variable = names(coefficients), coefficient = coefficients)

ggplot(coef_data, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Diagramma a barre dei coefficienti del modello") +
  xlab("Variabili indipendenti") +
  ylab("Coefficiente") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#


rapporti_odds <- exp(coef(modello_logit1))
intervalli_confidenza <- exp(confint(modello_logit1))

# Crea un dataframe con i risultati
risultati <- data.frame(
  variabile = names(rapporti_odds),
  odds_ratio = rapporti_odds,
  conf.low = intervalli_confidenza[, 1],
  conf.high = intervalli_confidenza[, 2]
)

# Crea un coefficients plot utilizzando ggplot2

plot_coefficients <- ggplot(risultati, aes(x = variabile, y = odds_ratio, color = variabile)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(title = "Coefficients Plot with Odds Ratios",
       x = "Variable",
       y = "Odds Ratio") +
  theme_minimal()

# Visualizza il coefficients plot
print(plot_coefficients)




#





#Interaction analysis
CADS_num <- as.data.frame(lapply(CADS_fct, as.numeric))
attach(CADS_num)

#Interaction treatment-sex
table(SEX)
table(TREATMENT)
TRESEX <- TREATMENT*SEX
TREEDU <- TREATMENT*EDU
TREMHS <- TREATMENT*MHS
TREDVAGE <- TREATMENT*DVAGE
TREDVURBAN <- TREATMENT*DVURBAN
TREALC <- TREATMENT*ALC
TRESMOKE <- TREATMENT*SMOKE
detach(CADS_num)

CADS_fct1 <- as.data.frame(lapply(CADS_fct, as.factor))

CADS_fct1$TRESEX <- TRESEX
CADS_fct1$TREEDU <- TREEDU
CADS_fct1$TREMHS <- TREMHS
CADS_fct1$TREDVAGE <- TREDVAGE
CADS_fct1$TREDVURBAN <- TREDVURBAN
CADS_fct1$TREALC <- TREALC
CADS_fct1$TRESMOKE <- TRESMOKE

CADS_fct1 <- as.data.frame(lapply(CADS_fct1, as.factor))
attach(CADS_fct1)


#
mod_interation_COC <- glm(COC ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1, family="binomial")
summary(mod_interation_COC)

mod_interation_MET <- glm(MET ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1, family="binomial")
summary(mod_interation_MET)

mod_interation_XTC <- glm(XTC ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1, family="binomial")
summary(mod_interation_XTC)

mod_interation_HAL <- glm(HAL ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1, family="binomial")
summary(mod_interation_HAL)

mod_interation_GLU <- glm(GLU ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1, family="binomial")
summary(mod_interation_GLU)

mod_interation_HER <- glm(HER ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1, family="binomial")
summary(mod_interation_HER)

mod_interation_SAL <- glm(SAL ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1, family="binomial")
summary(mod_interation_SAL)

#######
mod_interation_ALC <- glm(ALC ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1, family="binomial")
summary(mod_interation_ALC)

mod_interation_SMO <- glm(SMOKE ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1, family="binomial")
summary(mod_interation_SMO)

odds_ratiosALC <- exp(coef(mod_interation_ALC))
length(odds_ratiosALC)
se_log_oddsALC <- summary(mod_interation_ALC)$coefficients[, "Std. Error"]
length(se_log_oddsALC)
se_odds_ratioALC <- odds_ratiosALC*se_log_oddsALC
p_valuesALC <- summary(mod_interation_ALC)$coefficients[, "Pr(>|z|)"]
resultsALC <- data.frame(OddsRatio= odds_ratiosALC, StdError = se_odds_ratioALC, PValue = p_valuesALC)
print(resultsALC)
length(odds_ratiosALC)
lenght(se_odds_ratioALC)

odds_ratiosALC <- exp(coef(mod_interation_ALC))
valid_coef_indices <- which(!is.na(odds_ratiosALC))
odds_ratios_valid <- odds_ratiosALC[valid_coef_indices]
se_log_odds_valid <- se_log_oddsALC[valid_coef_indices]
se_odds_ratio_valid <- odds_ratios_valid * se_log_odds_valid
p_values_valid <- p_valuesALC[valid_coef_indices]
resultsALC <- data.frame(OddsRatio = odds_ratios_valid, StdError = se_odds_ratio_valid, PValue = p_values_valid)
print(resultsALC)



CADS_num <- as.data.frame(lapply(CADS_fct1, as.numeric))

condizioneCAN <- CADS_num$TREATMENT > 0
condizioneALC <- CADS_num$TREATMENT > 0

CAN12M <- as.numeric(as.character(CAN12M))
ALC <- as.numeric(as.character(ALC))
cor.test(CAN12M[condizioneCAN], ALC[condizioneALC], method = "spearman")



#####
mod_interation_SMOKE <- glm(SMOKE ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1, family="binomial")
summary(mod_interation_SMOKE)







mod_interation_CAN <- glm(CAN12M ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1, family="binomial")
summary(mod_interation_CAN)

#####prova cpoefficient plot
coefficienti <- bind_rows(tidy(modello1, conf.int = TRUE), tidy(modello2, conf.int = TRUE), .id = "Modello")

# Filtra i risultati solo per la variabile "treatment"
coefficienti_treatment <- filter(coefficienti, term == "treatment")

# Crea un coefficients plot utilizzando ggplot2

coefficienti <- bind_rows(tidy(modello1, conf.int = TRUE), tidy(modello2, conf.int = TRUE), .id = "Modello")

# Filtra i risultati solo per la variabile "treatment"
coefficienti_treatment <- filter(coefficienti, term == "treatment")

# Crea un coefficients plot utilizzando ggplot2

coefficienti <- bind_rows(tidy(modello1, conf.int = TRUE), tidy(modello2, conf.int = TRUE), .id = "Modello")

# Filtra i risultati solo per la variabile "treatment"
coefficienti_treatment <- filter(coefficienti, term == "treatment")

# Crea un coefficients plot utilizzando ggplot2

# Installa il pacchetto se non è già installato
# install.packages("broom")

# Carica il pacchetto

#costruire dataset senza null value
CADS_fct1_na <- na.omit(CADS_fct1)

mod_interation_HAL <- glm(HAL ~ TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1_na, family="binomial")
summary(mod_interation_HAL)

mod_interation_MET <- glm(MET ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1_na, family="binomial")
summary(mod_interation_MET)


# Visualizza il coefficients plot
mod_interation_HAL <- glm(HAL ~ TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1_na, family="binomial")
summary(mod_interation_HAL)

mod_interation_MET <- glm(MET ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1_na, family="binomial")
summary(mod_interation_MET)

stime <- c(1.9895530191211,1.0445122186173, 1.7870228090967)  # Sostituisci con le tue stime



std_error <- c(1.282207, 0.0454885071208, 0.931276557573)  # Sostituisci con i tuoi errori standard
modelli <- c("ModelloCOC", "ModelloMET", "ModelloXTC")  # Nomi dei modelli

# Crea un dataframe con i dati
dati <- data.frame(Modello = modelli, Stima = stime, StdError = std_error)

# Crea un coefficient plot utilizzando ggplot2
library(ggplot2)

plot_coefficients <- ggplot(dati, aes(x = Modello, y = Stima, color = Modello)) +
  geom_point() +
  geom_errorbar(aes(ymin = Stima - 1.96 * StdError, ymax = Stima + 1.96 * StdError), width = 0.2) +
  labs(title = "Coefficient Plot for TREATMENT",
       x = "Model",
       y = "Estimate") +
  theme_minimal()
print(plot_coefficients)



mod_interation_COC <- glm(COC ~ SEX+TREATMENT+TRESEX+EDU+TREEDU+DVAGE+TREDVAGE+DVURBAN+TREDVURBAN+MHS+TREMHS, CADS_fct1_na, family="binomial")
summary(mod_interation_COC)

odds_ratios <- exp(coef(mod_interation_COC))
se_log_odds <- summary(mod_interation_COC)$coefficients[, "Std. Error"]
print(se_log_odds)

se_odds_ratio <- odds_ratios*se_log_odds




