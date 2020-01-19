# Analyse Skript 

#### Bibliotheken laden

library(tidyverse)
source("qualtricshelpers.R")

#### Datei laden

raw <- read_csv("Data_Cleaning_Omnibusbefragung/Datensatz_Omnibusbefragung_A.csv")

Omnibusbefragung_A <- "Data_Cleaning_Omnibusbefragung/Datensatz_Omnibusbefragung_A.csv"
raw <- load_qualtrics_csv(Omnibusbefragung_A)

raw <- filter(raw, Status == "IP Address")

#### Daten bereinigen ----
### Schritt 1: Unnötige Spalten löschen. 
raw.short <- raw[-26,c(-1:-17, -24:-27, -40:-83, -114:-118, -124:-129)]

### Schritt 2: Variablen umbenennen 
generate_codebook(raw.short, Omnibusbefragung_A, "Data_Cleaning_Omnibusbefragung/codebook.csv")
codebook <- read_codebook("Data_Cleaning_Omnibusbefragung/codebook_final.csv")

## neue Namen auf die Daten anwenden:
names(raw.short) <- codebook$variable

### Schritt 3: Variablen den richtigen Typen zuordnen

## Gender zu kategorialer Variable machen:
raw.short$gender <- as.factor(raw.short$gender)

## Anstellungsdauer bereinigen:
## Recoding raw.short$jobtime into raw.short$AD
raw.short$AD <- raw.short$jobtime
raw.short$AD[raw.short$jobtime == "8"] <- "8"
raw.short$AD[raw.short$jobtime == "7"] <- "7"
raw.short$AD[raw.short$jobtime == "4"] <- "4"
raw.short$AD[raw.short$jobtime == "1"] <- "1"
raw.short$AD[raw.short$jobtime == "6 Monate"] <- "0,5"
raw.short$AD[raw.short$jobtime == "35"] <- "35"
raw.short$AD[raw.short$jobtime == "23"] <- "23"
raw.short$AD[raw.short$jobtime == "15"] <- "15"
raw.short$AD[raw.short$jobtime == "30"] <- "30"
raw.short$AD[raw.short$jobtime == "1 Jahr"] <- "1"
raw.short$AD[raw.short$jobtime == "2"] <- "2"
raw.short$AD[raw.short$jobtime == "5"] <- "5"
raw.short$AD[raw.short$jobtime == "11"] <- "11"
raw.short$AD[raw.short$jobtime == "40"] <- "40"
raw.short$AD[raw.short$jobtime == "1,5"] <- "1,5"
raw.short$AD[raw.short$jobtime == "12"] <- "12"
raw.short$AD[raw.short$jobtime == "0,3"] <- "0,3"
raw.short$AD[raw.short$jobtime == "2 Jahre"] <- "2"
raw.short$AD[raw.short$jobtime == "1 Monat :D"] <- "0,08"
raw.short$AD[raw.short$jobtime == "Halbes Jahr"] <- "0,5"
raw.short$AD[raw.short$jobtime == "1 Monat"] <- "0,08"
raw.short$AD[raw.short$jobtime == "0.25"] <- "0,25"
raw.short$AD[raw.short$jobtime == "18"] <- "18"
raw.short$AD[raw.short$jobtime == "25"] <- "25"
raw.short$AD[raw.short$jobtime == "6"] <- "6"
raw.short$AD[raw.short$jobtime == "10"] <- "10"
raw.short$AD[raw.short$jobtime == "2 Monate"] <- "0,1"
raw.short$AD[raw.short$jobtime == "2Monate"] <- "0,1"
raw.short$AD[raw.short$jobtime == "0"] <- "0"
raw.short$AD[raw.short$jobtime == "3 jahre"] <- "3"
raw.short$AD[raw.short$jobtime == "0,4"] <- "0,4"
raw.short$AD[raw.short$jobtime == "3"] <- "3"
raw.short$AD[raw.short$jobtime == "ca. 1 Jahr"] <- "1"
raw.short$AD[raw.short$jobtime == "0,5"] <- "0,5"
raw.short$AD[raw.short$jobtime == "0.4"] <- "0,4"
raw.short$AD[raw.short$jobtime == "19"] <- "19"
raw.short$AD[raw.short$jobtime == "1 1/2"] <- "1,5"
raw.short$AD[raw.short$jobtime == "Kellner"] <- "0"
raw.short$AD[raw.short$jobtime == "4 Jahre"] <- "4"
raw.short$AD[raw.short$jobtime == "13"] <- "13"
raw.short$AD[raw.short$jobtime == "2,5"] <- "2,5"
raw.short$AD[raw.short$jobtime == "16"] <- "16"
raw.short$AD[raw.short$jobtime == "1,3 jahre"] <- "1,3"
raw.short$AD[raw.short$jobtime == "26"] <- "26"
raw.short$AD[raw.short$jobtime == "5 Monate"] <- "0,4"
raw.short$AD[raw.short$jobtime == "9"] <- "9"
raw.short$AD[raw.short$jobtime == "1 jahr"] <- "1"
raw.short$AD[raw.short$jobtime == "2,5 Jahre"] <- "2,5"
raw.short$AD[raw.short$jobtime == "45"] <- "45"
raw.short$AD[raw.short$jobtime == "6,5 Jahre"] <- "6,5"
raw.short$AD[raw.short$jobtime == "3 Jahre"] <- "3"
raw.short$AD[raw.short$jobtime == "34"] <- "34"
raw.short$AD[raw.short$jobtime == "5,5"] <- "5,5"
raw.short$AD[raw.short$jobtime == "Finanzberatung"] <- "0"
raw.short$AD[raw.short$jobtime == "0,5 Jahre"] <- "0,5"
raw.short$AD[raw.short$jobtime == "32"] <- "32"
raw.short$AD[raw.short$jobtime == "3,5"] <- "3,5"
raw.short$AD[raw.short$jobtime == "15 Jahre"] <- "15"
raw.short$AD[raw.short$jobtime == "4 jahre"] <- "4"
raw.short$AD[raw.short$jobtime == "0,25"] <- "0,25"
raw.short$AD[raw.short$jobtime == "30 Jahre"] <- "30"
raw.short$AD[raw.short$jobtime == "47"] <- "47"
raw.short$AD[raw.short$jobtime == "4,5"] <- "4,5"
raw.short$AD[raw.short$jobtime == "5 Jahre"] <- "5"
raw.short$AD[raw.short$jobtime == "8 Jahre"] <- "8"
raw.short$AD[raw.short$jobtime == "0,1"] <- "0,1"
raw.short$AD[raw.short$jobtime == "18 Jahre"] <- "18"
raw.short$AD[raw.short$jobtime == "1/4 Jahr (3 Monate)"] <- "0,25"
raw.short$AD[raw.short$jobtime == "14"] <- "14"
raw.short$AD[raw.short$jobtime == "20"] <- "20"
raw.short$AD[raw.short$jobtime == "0,7"] <- "0,7"
raw.short$AD[raw.short$jobtime == "0,2"] <- "0,2"
raw.short$AD[raw.short$jobtime == "3 Monate"] <- "0,25"
raw.short$AD[raw.short$jobtime == "0 Jahre"] <- "0"
raw.short$AD[raw.short$jobtime == "0.5"] <- "0,5"
raw.short$AD[raw.short$jobtime == "31"] <- "31"
raw.short$AD[raw.short$jobtime == "22"] <- "22"
raw.short$AD[raw.short$jobtime == "0.3"] <- "0,3"
raw.short$AD[raw.short$jobtime == "24"] <- "24"
raw.short$AD[raw.short$jobtime == "6 Jahre"] <- "6"
raw.short$AD[raw.short$jobtime == "41 Jahre"] <- "41"
raw.short$AD[raw.short$jobtime == "1,5 jahre"] <- "1,5"
raw.short$AD[raw.short$jobtime == "1,5 Jahre"] <- "1,5"
raw.short$AD[raw.short$jobtime == "0,6"] <- "0,6"
raw.short$AD[raw.short$jobtime == "1 Jahr 10 Monate"] <- "1,8"
raw.short$AD[raw.short$jobtime == "2.5 Jahren"] <- "2,5"
raw.short$AD[raw.short$jobtime == "17"] <- "17"
raw.short$AD[raw.short$jobtime == "3Jahre"] <- "3"
raw.short$AD[raw.short$jobtime == "1/2"] <- "0,5"
raw.short$AD[raw.short$jobtime == "Gerade angefangen"] <- "0"
raw.short$AD[raw.short$jobtime == "38"] <- "38"
raw.short$AD[raw.short$jobtime == "3 Monaten"] <- "0,25"
raw.short$AD[raw.short$jobtime == "Praktikant"] <- "0"
raw.short$AD[raw.short$jobtime == "0.1"] <- "0,1"
raw.short$AD[raw.short$jobtime == "ca. 2 Jahre"] <- "2"
raw.short$AD[raw.short$jobtime == "33"] <- "33"
raw.short$AD[raw.short$jobtime == "halbes Jahr"] <- "0,5"
raw.short$AD[raw.short$jobtime == "2,5 jahre"] <- "2,5"
raw.short$AD[raw.short$jobtime == "4 Monate"] <- "0,3"
raw.short$AD[raw.short$jobtime == "1 1/2 Jahre"] <- "1,5"
raw.short$AD[is.na(raw.short$jobtime)] <- NA
raw.short$AD <- as.numeric(raw.short$AD)


##  Skala für Likertskala einmal anlegen
scale.zustimmung1 <-c("Gar nicht", 
                      "Weniger als einen Tag", 
                      "Einen Tag", 
                      "Zwei Tage", 
                      "Drei Tage", 
                      "Vier Tage",
                      "Mehr als vier Tage")

raw.short$remote_hours <- ordered(raw.short$remote_hours, levels = scale.zustimmung1)

scale.zustimmung2 <-c("Stimme gar nicht zu", 
                      "Stimme nicht zu", 
                      "Stimme eher nicht zu", 
                      "Stimme eher zu", 
                      "Stimme zu", 
                      "Stimme völlig zu")

raw.short$flex_1 <- ordered(raw.short$flex_1, levels = scale.zustimmung2)
raw.short$flex_2 <- ordered(raw.short$flex_2, levels = scale.zustimmung2)
raw.short$flex_3 <- ordered(raw.short$flex_3, levels = scale.zustimmung2)
raw.short$flex_4 <- ordered(raw.short$flex_4, levels = scale.zustimmung2)
raw.short$flex_5 <- ordered(raw.short$flex_5, levels = scale.zustimmung2)
raw.short$flex_6 <- ordered(raw.short$flex_6, levels = scale.zustimmung2)

raw.short$feel_work_1 <- ordered(raw.short$feel_work_1, levels = scale.zustimmung2)
raw.short$feel_work_2 <- ordered(raw.short$feel_work_2, levels = scale.zustimmung2)
raw.short$feel_work_3 <- ordered(raw.short$feel_work_3, levels = scale.zustimmung2)
raw.short$feel_work_4 <- ordered(raw.short$feel_work_4, levels = scale.zustimmung2)
raw.short$feel_work_5 <- ordered(raw.short$feel_work_5, levels = scale.zustimmung2)
raw.short$feel_work_6 <- ordered(raw.short$feel_work_6, levels = scale.zustimmung2)
raw.short$feel_work_7 <- ordered(raw.short$feel_work_7, levels = scale.zustimmung2)
raw.short$feel_work_8 <- ordered(raw.short$feel_work_8, levels = scale.zustimmung2)
raw.short$feel_work_9 <- ordered(raw.short$feel_work_9, levels = scale.zustimmung2)
raw.short$feel_work_10 <- ordered(raw.short$feel_work_10, levels = scale.zustimmung2)
raw.short$feel_work_11 <- ordered(raw.short$feel_work_11, levels = scale.zustimmung2)
raw.short$feel_work_12 <- ordered(raw.short$feel_work_12, levels = scale.zustimmung2)

raw.short$free_time_1 <- ordered(raw.short$free_time_1, levels = scale.zustimmung2)
raw.short$free_time_2 <- ordered(raw.short$free_time_2, levels = scale.zustimmung2)
raw.short$free_time_3 <- ordered(raw.short$free_time_3, levels = scale.zustimmung2)
raw.short$free_time_4 <- ordered(raw.short$free_time_4, levels = scale.zustimmung2)
raw.short$free_time_5 <- ordered(raw.short$free_time_5, levels = scale.zustimmung2)
raw.short$free_time_6 <- ordered(raw.short$free_time_6, levels = scale.zustimmung2)
raw.short$free_time_7 <- ordered(raw.short$free_time_7, levels = scale.zustimmung2)
raw.short$free_time_8 <- ordered(raw.short$free_time_8, levels = scale.zustimmung2)
raw.short$free_time_9 <- ordered(raw.short$free_time_9, levels = scale.zustimmung2)

raw.short$flex_feel_1 <- ordered(raw.short$flex_feel_1, levels = scale.zustimmung2)
raw.short$flex_feel_2 <- ordered(raw.short$flex_feel_2, levels = scale.zustimmung2)
raw.short$flex_feel_3 <- ordered(raw.short$flex_feel_3, levels = scale.zustimmung2)
raw.short$flex_feel_4 <- ordered(raw.short$flex_feel_4, levels = scale.zustimmung2)

raw.short$attitude_company_1 <- ordered(raw.short$attitude_company_1, levels = scale.zustimmung2)
raw.short$attitude_company_2 <- ordered(raw.short$attitude_company_2, levels = scale.zustimmung2)
raw.short$attitude_company_3 <- ordered(raw.short$attitude_company_3, levels = scale.zustimmung2)
raw.short$attitude_company_4 <- ordered(raw.short$attitude_company_4, levels = scale.zustimmung2)
raw.short$attitude_company_5 <- ordered(raw.short$attitude_company_5, levels = scale.zustimmung2)

### Schritt 4: Skalen berechnen

library(psych)

schluesselliste <- list(SOFF = c("-feel_work_1", "-free_time_1", "-free_time_2", "free_time_3", "-free_time_4", "-free_time_5", "-free_time_6", "-free_time_7", "free_time_8", "free_time_9"),
                        AZZ = c("feel_work_8"),
                        AI = c("feel_work_7"),
                        FLEX = c("flex_1", "flex_2", "-flex_3", "flex_4", "flex_5", "flex_6", "feel_work_3", "feel_work_11", "flex_feel_1",  "flex_feel_2", "-flex_feel_3", "-flex_feel_4"),
                        ZUFR = c("feel_work_2", "-feel_work_9", "feel_work_10", "feel_time_1", "feel_time_2", "feel_time_3", "feel_time_4", "attitude_company_1", "attitude_company_2", "-attitude_company_3", "attitude_company_4", "attitude_company_5"))

scores <- scoreItems(schluesselliste, raw.short, missing = TRUE, min = 1, max = 6)

library(dplyr)

data <- bind_cols(raw.short, as_tibble(scores$scores))

## Entfernen alle EinzelItems
data <- data %>% 
  select(-starts_with("jobtime", ignore.case = F)) %>%
  select(-starts_with("flex", ignore.case = F)) %>% 
  select(-starts_with("feel_work", ignore.case = F)) %>%
  select(-starts_with("feel_time", ignore.case = F)) %>%
  select(-starts_with("free_time", ignore.case = F)) %>%
  select(-starts_with("flex_feel", ignore.case = F)) %>%
  select(-starts_with("attitude_company", ignore.case = F))

## Merke für uns
# SOFF = Switch-Off -> Variable: SOFF
# AZ = Arbeitszeit -> Variable: workingtime
# AZZ = Arbeitszeit-Zufriedenheit -> Variable: AZZ
# AI = Arbeitsintensität -> Variable: AI
# AD = Antellungsdauer -> Variable: jobtime
# JOB = Art der Tätigkeit -> Variable: edunow
# MA = Mobieles Arbeiten -> Variable: remote_hours
# FLEX = Flexibilität -> Variable: FLEX
# ZUFR = allg. Arbeitszufriedenheit -> Variable: ZUFR
# EN = Entferung zur Arbeitsstelle -> Variable: distance

## Einteilung von Skalen in Gruppen

#SOFF
## 1 - 2,5 = schlecht; 2,6 - 4,4 = mittelmäßig; 4,5 - 6 = gut
### Recoding data$SOFF into data$SOFF_GR
data$SOFF_GR <- as.character(data$SOFF)
data$SOFF_GR[data$SOFF == "3.9"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "3.4"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "3.6"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "4"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "5.2"] <- "gut"
data$SOFF_GR[data$SOFF == "3.8"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "4.1"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "2.6"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "5.3"] <- "gut"
data$SOFF_GR[data$SOFF == "4.3"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "2.7"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "3.1"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "3.2"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "3.5"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "3.7"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "3.3"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "2.9"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "4.6"] <- "gut"
data$SOFF_GR[data$SOFF == "3"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "4.5"] <- "gut"
data$SOFF_GR[data$SOFF == "4.4"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "2.4"] <- "schlecht"
data$SOFF_GR[data$SOFF == "4.2"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "4.9"] <- "gut"
data$SOFF_GR[data$SOFF == "2.3"] <- "schlecht"
data$SOFF_GR[data$SOFF == "5"] <- "gut"
data$SOFF_GR[data$SOFF == "5.5"] <- "gut"
data$SOFF_GR[data$SOFF == "2.8"] <- "mittelmäßig"
data$SOFF_GR[data$SOFF == "2"] <- "schlecht"
data$SOFF_GR[data$SOFF == "5.1"] <- "gut"
data$SOFF_GR[data$SOFF == "4.7"] <- "gut"
data$SOFF_GR[data$SOFF == "4.8"] <- "gut"


#AZ
## bis 29 Std./Woche = geringe Arbeitszeit; 30-50 Std./Woche = durchschnittliche Arbeitszeit; über 50 Std./Woche = überdurchschnittliche Arebitszeit
### Recoding data$workingtime into data$AZ_GR
data$AZ_GR <- data$workingtime
data$AZ_GR[data$workingtime == "40 Std. bis 50 Std."] <- "durchschnittliche Arbeitszeit (30-50 Std./Woche)"
data$AZ_GR[data$workingtime == "unter 20 Std."] <- "geringe Arbeitszeit (bis 29 Std./Woche)"
data$AZ_GR[data$workingtime == "20 Std. bis 29 Std."] <- "geringe Arbeitszeit (bis 29 Std./Woche)"
data$AZ_GR[data$workingtime == "30 Std. bis 39 Std."] <- "durchschnittliche Arbeitszeit (30-50 Std./Woche)"
data$AZ_GR[data$workingtime == "über 50 Std."] <- "überdurchschnittliche Arbeitszeit (über 50 Std./Woche)"
data$AZ_GR[is.na(data$workingtime)] <- NA


#AZZ
## 1 - 2,5 = unzufrieden; 2,6 - 4,4 = zufrieden; 4,5 - 6 = sehr zufrieden
### Recoding data$AZZ into data$AZZ_GR
data$AZZ_GR <- as.character(data$AZZ)
data$AZZ_GR[data$AZZ == "2"] <- "unzufrieden"
data$AZZ_GR[data$AZZ == "6"] <- "sehr zufrieden"
data$AZZ_GR[data$AZZ == "4"] <- "zufrieden"
data$AZZ_GR[data$AZZ == "3"] <- "zufrieden"
data$AZZ_GR[data$AZZ == "1"] <- "unzufrieden"
data$AZZ_GR[data$AZZ == "5"] <- "sehr zufrieden"


#AI
## 1 - 2,5 = gering; 2,6 - 4,4 = mittelmäßig; 4,5 - 6 = hoch
### Recoding data$AI into data$AI_GR
data$AI_GR <- as.character(data$AI)
data$AI_GR[data$AI == "2"] <- "gering"
data$AI_GR[data$AI == "3"] <- "mittelmäßig"
data$AI_GR[data$AI == "5"] <- "hoch"
data$AI_GR[data$AI == "1"] <- "gering"
data$AI_GR[data$AI == "4"] <- "mittelmäßig"
data$AI_GR[data$AI == "6"] <- "hoch"


#AD
##Mediansplit -> 6 Gruppen
### Cutting data$AD into data$AD_GR
data$AD_GR <- cut(data$AD, include.lowest=TRUE,  right=TRUE,
                  breaks=c(0, 1, 2, 3, 5, 15, 47))


##JOB
## NA; Selbstständig, Angestellt; Führungsposition
### Recoding data$edunow into data$JOB_GR
data$JOB_GR <- data$edunow
data$JOB_GR[data$edunow == "Geschäftsführung (Vollzeit)"] <- "Führungsposition"
data$JOB_GR[data$edunow == "Selbstständig"] <- "Selbstständig"
data$JOB_GR[data$edunow == "Student"] <- NA
data$JOB_GR[data$edunow == "Angestellt (Vollzeit)"] <- "Angestellt"
data$JOB_GR[data$edunow == "Praktikant"] <- "Angestellt"
data$JOB_GR[data$edunow == "Angestellt in Führungsposition (Vollzeit)"] <- "Führungsposition"
data$JOB_GR[data$edunow == "Angestellt (Teilzeit)"] <- "Angestellt"
data$JOB_GR[data$edunow == "Angestellt in Führungsposition (Teilzeit)"] <- "Führungsposition"
data$JOB_GR[data$edunow == "Auszubildender"] <- "Angestellt"
data$JOB_GR[data$edunow == "Werksstudent"] <- "Angestellt"
data$JOB_GR[data$edunow == "Freiberuflich (Freelancer)"] <- "Selbstständig"
data$JOB_GR[data$edunow == "Arbeitssuchend"] <- NA
data$JOB_GR[data$edunow == "Geschäftsführung (Teilzeit)"] <- "Führungsposition"
data$JOB_GR[data$edunow == "Rentner"] <- NA
data$JOB_GR[data$edunow == "Schüler"] <- NA
data$JOB_GR[is.na(data$edunow)] <- NA


#MA
## NA; Gar nicht; ≤ 2 Tage/Woche = geringer Anteil der Woche; ≥ 3 Tage/Woche = Großteil der Woche
### Recoding data$remote_hours into data$MA_GR
data$MA_GR <- as.character(data$remote_hours)
data$MA_GR[data$remote_hours == "Gar nicht"] <- "Gar nicht"
data$MA_GR[data$remote_hours == "Weniger als einen Tag"] <- "geringer Anteil der Woche (≤ 2 Tage/Woche)"
data$MA_GR[data$remote_hours == "Einen Tag"] <- "geringer Anteil der Woche (≤ 2 Tage/Woche)"
data$MA_GR[data$remote_hours == "Zwei Tage"] <- "geringer Anteil der Woche (≤ 2 Tage/Woche)"
data$MA_GR[data$remote_hours == "Drei Tage"] <- "Großteil der Woche (≥ 3 Tage/Woche)"
data$MA_GR[data$remote_hours == "Vier Tage"] <- "Großteil der Woche (≥ 3 Tage/Woche)"
data$MA_GR[data$remote_hours == "Mehr als vier Tage"] <- "Großteil der Woche (≥ 3 Tage/Woche)"
data$MA_GR[is.na(data$remote_hours)] <- NA


#FLEX
## 1 - 2,5 = unfelxibel; 2,6 - 4,4 = flexibel; 4,5 - 6 = sehr flexibel
### Recoding data$FLEX into data$FLEX_GR
data$FLEX_GR <- as.character(data$FLEX)
data$FLEX_GR[data$FLEX == "3.41666666666667"] <- "flexibel"
data$FLEX_GR[data$FLEX == "2.83333333333333"] <- "flexibel"
data$FLEX_GR[data$FLEX == "3.91666666666667"] <- "flexibel"
data$FLEX_GR[data$FLEX == "2"] <- "unflexibel"
data$FLEX_GR[data$FLEX == "3.25"] <- "flexibel"
data$FLEX_GR[data$FLEX == "3.08333333333333"] <- "flexibel"
data$FLEX_GR[data$FLEX == "2.25"] <- "unflexibel"
data$FLEX_GR[data$FLEX == "2.75"] <- "flexibel"
data$FLEX_GR[data$FLEX == "4.33333333333333"] <- "flexibel"
data$FLEX_GR[data$FLEX == "4.16666666666667"] <- "flexibel"
data$FLEX_GR[data$FLEX == "5.25"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "2.91666666666667"] <- "flexibel"
data$FLEX_GR[data$FLEX == "4.25"] <- "flexibel"
data$FLEX_GR[data$FLEX == "2.66666666666667"] <- "flexibel"
data$FLEX_GR[data$FLEX == "3.33333333333333"] <- "flexibel"
data$FLEX_GR[data$FLEX == "3.83333333333333"] <- "flexibel"
data$FLEX_GR[data$FLEX == "4.91666666666667"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "3.16666666666667"] <- "flexibel"
data$FLEX_GR[data$FLEX == "2.58333333333333"] <- "unflexibel"
data$FLEX_GR[data$FLEX == "4.5"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "3.58333333333333"] <- "flexibel"
data$FLEX_GR[data$FLEX == "5.66666666666667"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "2.16666666666667"] <- "unflexibel"
data$FLEX_GR[data$FLEX == "3.75"] <- "flexibel"
data$FLEX_GR[data$FLEX == "2.08333333333333"] <- "unflexibel"
data$FLEX_GR[data$FLEX == "4.58333333333333"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "3"] <- "flexibel"
data$FLEX_GR[data$FLEX == "3.66666666666667"] <- "flexibel"
data$FLEX_GR[data$FLEX == "2.41666666666667"] <- "unflexibel"
data$FLEX_GR[data$FLEX == "4.08333333333333"] <- "flexibel"
data$FLEX_GR[data$FLEX == "4"] <- "flexibel"
data$FLEX_GR[data$FLEX == "2.33333333333333"] <- "unflexibel"
data$FLEX_GR[data$FLEX == "5"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "2.5"] <- "unflexibel"
data$FLEX_GR[data$FLEX == "4.41666666666667"] <- "flexibel"
data$FLEX_GR[data$FLEX == "5.5"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "4.75"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "3.5"] <- "flexibel"
data$FLEX_GR[data$FLEX == "4.66666666666667"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "1.83333333333333"] <- "unflexibel"
data$FLEX_GR[data$FLEX == "5.75"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "1.91666666666667"] <- "unflexibel"
data$FLEX_GR[data$FLEX == "5.41666666666667"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "5.16666666666667"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "1.75"] <- "unflexibel"
data$FLEX_GR[data$FLEX == "5.58333333333333"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "4.83333333333333"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "5.08333333333333"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "1.16666666666667"] <- "unflexibel"
data$FLEX_GR[data$FLEX == "5.91666666666667"] <- "sehr flexibel"
data$FLEX_GR[data$FLEX == "1.33333333333333"] <- "unflexibel"


#ZUFR
## 1 - 2,5 = unzufrieden; 2,6 - 4,4 = zufrieden; 4,5 - 6 = sehr zufrieden
### Recoding data$ZUFR into data$ZUFR_GR
data$ZUFR_GR <- as.character(data$ZUFR)
data$ZUFR_GR[data$ZUFR == "3.75"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "5.41666666666667"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "4.5"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "3.91666666666667"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "4.25"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "4.75"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "4.16666666666667"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "5.16666666666667"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "4.91666666666667"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "5.08333333333333"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "4.41666666666667"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "2.16666666666667"] <- "unzufrieden"
data$ZUFR_GR[data$ZUFR == "3.58333333333333"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "3.33333333333333"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "4.66666666666667"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "3.66666666666667"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "5.25"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "3.83333333333333"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "4.08333333333333"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "2.75"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "5.75"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "4.33333333333333"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "5"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "3.41666666666667"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "1.16666666666667"] <- "unzufrieden"
data$ZUFR_GR[data$ZUFR == "5.5"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "3.5"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "4"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "3.25"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "4.58333333333333"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "2.83333333333333"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "2.5"] <- "unzufrieden"
data$ZUFR_GR[data$ZUFR == "3.08333333333333"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "3"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "6"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "2"] <- "unzufrieden"
data$ZUFR_GR[data$ZUFR == "2.25"] <- "unzufrieden"
data$ZUFR_GR[data$ZUFR == "2.08333333333333"] <- "unzufrieden"
data$ZUFR_GR[data$ZUFR == "4.83333333333333"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "3.16666666666667"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "1.5"] <- "unzufrieden"
data$ZUFR_GR[data$ZUFR == "5.33333333333333"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "5.91666666666667"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "2.41666666666667"] <- "unzufrieden"
data$ZUFR_GR[data$ZUFR == "2.66666666666667"] <- "zufrieden"
data$ZUFR_GR[data$ZUFR == "2.58333333333333"] <- "unzufrieden"
data$ZUFR_GR[data$ZUFR == "1.66666666666667"] <- "unzufrieden"
data$ZUFR_GR[data$ZUFR == "5.58333333333333"] <- "sehr zufrieden"
data$ZUFR_GR[data$ZUFR == "2.91666666666667"] <- "zufrieden"


#EN
## 0-50; 51-100; 101-150; 151-200; >200
### Recoding data$distance into data$EN_GR
data$EN_GR <- as.character(data$distance)
data$EN_GR <- fct_recode(data$EN_GR,
               "51-100" = "65",
               "> 200" = "999",
               "0-50" = "1",
               "0-50" = "8",
               "0-50" = "10",
               "0-50" = "22",
               "0-50" = "6",
               "0-50" = "12",
               "51-100" = "77",
               "0-50" = "20",
               "0-50" = "50",
               "0-50" = "2",
               "0-50" = "5",
               "0-50" = "3",
               "0-50" = "9",
               "> 200" = "600",
               "0-50" = "30",
               "0-50" = "14",
               "51-100" = "100",
               "51-100" = "80",
               "0-50" = "25",
               "0-50" = "4",
               "0-50" = "40",
               "0-50" = "7",
               "0-50" = "0",
               "0-50" = "19",
               "0-50" = "15",
               "151-200" = "180",
               "0-50" = "18",
               "0-50" = "11",
               "0-50" = "23",
               "151-200" = "175",
               "0-50" = "31",
               "0-50" = "1.5",
               "51-100" = "82",
               "0-50" = "35",
               "101-150" = "110",
               "0-50" = "37",
               "51-100" = "60",
               "0-50" = "45",
               "51-100" = "70",
               "0-50" = "21",
               "0-50" = "29",
               "0-50" = "0.05",
               "0-50" = "27",
               "0-50" = "0.5",
               "0-50" = "13",
               "0-50" = "0.7",
               "0-50" = "5.5",
               "101-150" = "150",
               "0-50" = "28",
               "0-50" = "7.2",
               "0-50" = "42",
               "151-200" = "185",
               "101-150" = "105",
               "51-100" = "92",
               "0-50" = "36",
               "0-50" = "32",
               "0-50" = "3.5",
               "51-100" = "86",
               "0-50" = "16",
               "0-50" = "34",
               "51-100" = "75",
               "0-50" = "41",
               "> 200" = "430",
               "0-50" = "17",
               "0-50" = "0.6",
               "0-50" = "4.7",
               "0-50" = "0.2",
               "0-50" = "4.5",
               "51-100" = "90",
               "51-100" = "68",
               "51-100" = "67")


saveRDS(data, "Data_Cleaning_Omnibusbefragung/data.rds")

## Merke für uns
# SOFF_GR = Switch-Off -> schlecht/mittelmäßig/gut
# AZ_GR = Arbeitszeit -> NA/geringe Arbeitszeit (bis 29 Std./Woche)/durchschnittliche Arbeitszeit (30-50 Std./Woche)/überdurchschnittliche Arbeitszeit (über 50 Std./Woche)
# AZZ_GR = Arbeitszeit-Zufriedenheit -> unzufrieden/zufrieden/sehr zufrieden
# AI_GR = Arbeitsintensität -> gering/mittelmäßig/hoch 
# AD_GR = Antellungsdauer -> Mediansplit 6 Gruppen 
# JOB_GR = Art der Tätigkeit -> NA/Selbstständig/Angestellt/Führungsposition
# MA_GR = Mobieles Arbeiten -> NA/Gar nicht/geringer Anteil der Woche (≤ 2 Tage/Woche)/Großteil der Woche (≥ 3 Tage/Woche)
# FLEX_GR = Flexibilität -> unfelxibel/flexibel/sehr flexibel
# ZUFR_GR = allg. Arbeitszufriedenheit -> unzufrieden/zufrieden/sehr zufrieden
# EN_GR = Entferung zur Arbeitsstelle -> NA/0-50/51-100/101-150/151-200/>200

