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
## Reordering data$SOFF into data$SOFF_O
data$SOFF_O <- ordered(data$SOFF, levels=c("2", "2.3", "2.4", "2.6", "2.7", "2.8", "2.9", "3", "3.1", "3.2", "3.3", "3.4", "3.5", "3.6", "3.7", "3.8", "3.9", "4", "4.1", "4.2", "4.3", "4.4", "4.5", "4.6", "4.7", "4.8", "4.9", "5", "5.1", "5.2", "5.3", "5.5"))
## Recoding data$SOFF_O into data$SOFF_GR
data$SOFF_GR <- recode(data$SOFF_O,
               "2" = "schlecht",
               "2.3" = "schlecht",
               "2.4" = "schlecht",
               "2.6" = "mittelmäßig",
               "2.7" = "mittelmäßig",
               "2.8" = "mittelmäßig",
               "2.9" = "mittelmäßig",
               "3" = "mittelmäßig",
               "3.1" = "mittelmäßig",
               "3.2" = "mittelmäßig",
               "3.3" = "mittelmäßig",
               "3.4" = "mittelmäßig",
               "3.5" = "mittelmäßig",
               "3.6" = "mittelmäßig",
               "3.7" = "mittelmäßig",
               "3.8" = "mittelmäßig",
               "3.9" = "mittelmäßig",
               "4" = "mittelmäßig",
               "4.1" = "mittelmäßig",
               "4.2" = "mittelmäßig",
               "4.3" = "mittelmäßig",
               "4.4" = "mittelmäßig",
               "4.5" = "gut",
               "4.6" = "gut",
               "4.7" = "gut",
               "4.8" = "gut",
               "4.9" = "gut",
               "5" = "gut",
               "5.1" = "gut",
               "5.2" = "gut",
               "5.3" = "gut",
               "5.5" = "gut")


#AZ
## bis 29 Std./Woche = geringe Arbeitszeit; 30-50 Std./Woche = durchschnittliche Arbeitszeit; über 50 Std./Woche = überdurchschnittliche Arebitszeit
## Reordering data$workingtime into data$AZ_O
data$AZ_O <- ordered(data$workingtime, levels=c("unter 20 Std.", "20 Std. bis 29 Std.", "30 Std. bis 39 Std.", "40 Std. bis 50 Std.", "über 50 Std."))
## Recoding data$AZ_O into data$AZ_GR
data$AZ_GR <- fct_recode(data$AZ_O,
               "geringe Arbeitszeit (bis 29 Std./Woche)" = "unter 20 Std.",
               "geringe Arbeitszeit (bis 29 Std./Woche)" = "20 Std. bis 29 Std.",
               "durchschnittliche Arbeitszeit (30-50 Std./Woche)" = "30 Std. bis 39 Std.",
               "durchschnittliche Arbeitszeit (30-50 Std./Woche)" = "40 Std. bis 50 Std.",
               "überdurchschnittliche Arbeitszeit (über 50 Std./Woche)" = "über 50 Std.")


#AZZ
## 1 - 2,5 = unzufrieden; 2,6 - 4,4 = zufrieden; 4,5 - 6 = sehr zufrieden
## Reordering data$AZZ into data$AZZ_O
data$AZZ_O <- ordered(data$AZZ, levels=c("1", "2", "3", "4", "5", "6"))
## Recoding data$AZZ_O into data$AZZ_GR
data$AZZ_GR <- fct_recode(data$AZZ_O,
               "unzufrieden" = "1",
               "unzufrieden" = "2",
               "zufrieden" = "3",
               "zufrieden" = "4",
               "sehr zufrieden" = "5",
               "sehr zufrieden" = "6")


#AI
## 1 - 2,5 = gering; 2,6 - 4,4 = mittelmäßig; 4,5 - 6 = hoch
## Reordering data$AI into data$AI_O
data$AI_O <- ordered(data$AI, levels=c("1", "2", "3", "4", "5", "6"))
## Recoding data$AI_O into data$AI_GR
data$AI_GR <- fct_recode(data$AI_O,
               "gering" = "1",
               "gering" = "2",
               "mittelmäßig" = "3",
               "mittelmäßig" = "4",
               "hoch" = "5",
               "hoch" = "6")


#AD
##Mediansplit -> 6 Gruppen
### Cutting data$AD into data$AD_GR
data$AD_GR <- cut(data$AD, include.lowest=TRUE,  right=TRUE,
                  breaks=c(0, 1, 2, 3, 5, 15, 47))


##JOB
## NA; Selbstständig, Angestellt; Führungsposition
data$JOB_O <- as.factor(data$edunow)
## Recoding data$JOB_O into data$JOB_GR
data$JOB_GR <- fct_recode(data$JOB_O,
               "Angestellt" = "Angestellt (Teilzeit)",
               "Angestellt" = "Angestellt (Vollzeit)",
               "Führungsposition" = "Angestellt in Führungsposition (Teilzeit)",
               "Führungsposition" = "Angestellt in Führungsposition (Vollzeit)",
               NULL = "Arbeitssuchend",
               "Angestellt" = "Auszubildender",
               "Selbstständig" = "Freiberuflich (Freelancer)",
               "Führungsposition" = "Geschäftsführung (Teilzeit)",
               "Führungsposition" = "Geschäftsführung (Vollzeit)",
               "Angestellt" = "Praktikant",
               NULL = "Rentner",
               NULL = "Schüler",
               NULL = "Student",
               "Angestellt" = "Werksstudent")


#MA
## NA; Gar nicht; ≤ 2 Tage/Woche = geringer Anteil der Woche; ≥ 3 Tage/Woche = Großteil der Woche
## Reordering data$remote_hours into data$MA_O
data$MA_O <- ordered(data$remote_hours, levels=c("Gar nicht", "Weniger als einen Tag", "Einen Tag", "Zwei Tage", "Drei Tage", "Vier Tage", "Mehr als vier Tage"))
## Recoding data$MA_O into data$MA_GR
data$MA_GR <- fct_recode(data$MA_O,
               "geringer Anteil der Woche (≤ 2 Tage/Woche)" = "Weniger als einen Tag",
               "geringer Anteil der Woche (≤ 2 Tage/Woche)" = "Einen Tag",
               "geringer Anteil der Woche (≤ 2 Tage/Woche)" = "Zwei Tage",
               "Großteil der Woche (≥ 3 Tage/Woche)" = "Drei Tage",
               "Großteil der Woche (≥ 3 Tage/Woche)" = "Vier Tage",
               "Großteil der Woche (≥ 3 Tage/Woche)" = "Mehr als vier Tage")


#FLEX
## 1 - 2,5 = unfelxibel; 2,6 - 4,4 = flexibel; 4,5 - 6 = sehr flexibel
## Reordering data$FLEX into data$FLEX_O
data$FLEX_O <- ordered(data$FLEX, levels=c("1.16666666666667", "1.33333333333333", "1.75", "1.83333333333333", "1.91666666666667", "2", "2.08333333333333", "2.16666666666667", "2.25", "2.33333333333333", "2.41666666666667", "2.5", "2.58333333333333", "2.66666666666667", "2.75", "2.83333333333333", "2.91666666666667", "3", "3.08333333333333", "3.16666666666667", "3.25", "3.33333333333333", "3.41666666666667", "3.5", "3.58333333333333", "3.66666666666667", "3.75", "3.83333333333333", "3.91666666666667", "4", "4.08333333333333", "4.16666666666667", "4.25", "4.33333333333333", "4.41666666666667", "4.5", "4.58333333333333", "4.66666666666667", "4.75", "4.83333333333333", "4.91666666666667", "5", "5.08333333333333", "5.16666666666667", "5.25", "5.41666666666667", "5.5", "5.58333333333333", "5.66666666666667", "5.75", "5.91666666666667"))
## Recoding data$FLEX_O into data$FLEX_GR
data$FLEX_GR <- fct_recode(data$FLEX_O,
               "unflexibel" = "1.16666666666667",
               "unflexibel" = "1.33333333333333",
               "unflexibel" = "1.75",
               "unflexibel" = "1.83333333333333",
               "unflexibel" = "1.91666666666667",
               "unflexibel" = "2",
               "unflexibel" = "2.08333333333333",
               "unflexibel" = "2.16666666666667",
               "unflexibel" = "2.25",
               "unflexibel" = "2.33333333333333",
               "unflexibel" = "2.41666666666667",
               "unflexibel" = "2.5",
               "flexibel" = "2.58333333333333",
               "flexibel" = "2.66666666666667",
               "flexibel" = "2.75",
               "flexibel" = "2.83333333333333",
               "flexibel" = "2.91666666666667",
               "flexibel" = "3",
               "flexibel" = "3.08333333333333",
               "flexibel" = "3.16666666666667",
               "flexibel" = "3.25",
               "flexibel" = "3.33333333333333",
               "flexibel" = "3.41666666666667",
               "flexibel" = "3.5",
               "flexibel" = "3.58333333333333",
               "flexibel" = "3.66666666666667",
               "flexibel" = "3.75",
               "flexibel" = "3.83333333333333",
               "flexibel" = "3.91666666666667",
               "flexibel" = "4",
               "flexibel" = "4.08333333333333",
               "flexibel" = "4.16666666666667",
               "flexibel" = "4.25",
               "flexibel" = "4.33333333333333",
               "flexibel" = "4.41666666666667",
               "sehr flexibel" = "4.5",
               "sehr flexibel" = "4.58333333333333",
               "sehr flexibel" = "4.66666666666667",
               "sehr flexibel" = "4.75",
               "sehr flexibel" = "4.83333333333333",
               "sehr flexibel" = "4.91666666666667",
               "sehr flexibel" = "5",
               "sehr flexibel" = "5.08333333333333",
               "sehr flexibel" = "5.16666666666667",
               "sehr flexibel" = "5.25",
               "sehr flexibel" = "5.41666666666667",
               "sehr flexibel" = "5.5",
               "sehr flexibel" = "5.58333333333333",
               "sehr flexibel" = "5.66666666666667",
               "sehr flexibel" = "5.75",
               "sehr flexibel" = "5.91666666666667")


#ZUFR
## 1 - 2,5 = unzufrieden; 2,6 - 4,4 = zufrieden; 4,5 - 6 = sehr zufrieden
## Reordering data$ZUFR into data$ZUFR_O
data$ZUFR_O <- ordered(data$ZUFR, levels=c("1.16666666666667", "1.5", "1.66666666666667", "2", "2.08333333333333", "2.16666666666667", "2.25", "2.41666666666667", "2.5", "2.58333333333333", "2.66666666666667", "2.75", "2.83333333333333", "2.91666666666667", "3", "3.08333333333333", "3.16666666666667", "3.25", "3.33333333333333", "3.41666666666667", "3.5", "3.58333333333333", "3.66666666666667", "3.75", "3.83333333333333", "3.91666666666667", "4", "4.08333333333333", "4.16666666666667", "4.25", "4.33333333333333", "4.41666666666667", "4.5", "4.58333333333333", "4.66666666666667", "4.75", "4.83333333333333", "4.91666666666667", "5", "5.08333333333333", "5.16666666666667", "5.25", "5.33333333333333", "5.41666666666667", "5.5", "5.58333333333333", "5.75", "5.91666666666667", "6"))
## Recoding data$ZUFR_O into data$ZUFR_GR
data$ZUFR_GR <- fct_recode(data$ZUFR_O,
               "unzufrieden" = "1.16666666666667",
               "unzufrieden" = "1.5",
               "unzufrieden" = "1.66666666666667",
               "unzufrieden" = "2",
               "unzufrieden" = "2.08333333333333",
               "unzufrieden" = "2.16666666666667",
               "unzufrieden" = "2.25",
               "unzufrieden" = "2.41666666666667",
               "unzufrieden" = "2.5",
               "zufrieden" = "2.58333333333333",
               "zufrieden" = "2.66666666666667",
               "zufrieden" = "2.75",
               "zufrieden" = "2.83333333333333",
               "zufrieden" = "2.91666666666667",
               "zufrieden" = "3",
               "zufrieden" = "3.08333333333333",
               "zufrieden" = "3.16666666666667",
               "zufrieden" = "3.25",
               "zufrieden" = "3.33333333333333",
               "zufrieden" = "3.41666666666667",
               "zufrieden" = "3.5",
               "zufrieden" = "3.58333333333333",
               "zufrieden" = "3.66666666666667",
               "zufrieden" = "3.75",
               "zufrieden" = "3.83333333333333",
               "zufrieden" = "3.91666666666667",
               "zufrieden" = "4",
               "zufrieden" = "4.08333333333333",
               "zufrieden" = "4.16666666666667",
               "zufrieden" = "4.25",
               "zufrieden" = "4.33333333333333",
               "zufrieden" = "4.41666666666667",
               "sehr zufrieden" = "4.5",
               "sehr zufrieden" = "4.58333333333333",
               "sehr zufrieden" = "4.66666666666667",
               "sehr zufrieden" = "4.75",
               "sehr zufrieden" = "4.83333333333333",
               "sehr zufrieden" = "4.91666666666667",
               "sehr zufrieden" = "5",
               "sehr zufrieden" = "5.08333333333333",
               "sehr zufrieden" = "5.16666666666667",
               "sehr zufrieden" = "5.25",
               "sehr zufrieden" = "5.33333333333333",
               "sehr zufrieden" = "5.41666666666667",
               "sehr zufrieden" = "5.5",
               "sehr zufrieden" = "5.58333333333333",
               "sehr zufrieden" = "5.75",
               "sehr zufrieden" = "5.91666666666667",
               "sehr zufrieden" = "6")


#EN
## 0-50; 51-100; 101-150; 151-200; >200
## Reordering data$distance into data$EN_O
data$EN_O <- ordered(data$distance, levels=c("0", "0.05", "0.2", "0.5", "0.6", "0.7", "1", "1.5", "2", "3", "3.5", "4", "4.5", "4.7", "5", "5.5", "6", "7", "7.2", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "25", "27", "28", "29", "30", "31", "32", "34", "35", "36", "37", "40", "41", "42", "45", "50", "60", "65", "67", "68", "70", "75", "77", "80", "82", "86", "90", "92", "100", "105", "110", "150", "175", "180", "185", "430", "600", "999"))
## Recoding data$EN_O into data$EN_GR
data$EN_GR <- fct_recode(data$EN_O,
               "0-50" = "0",
               "0-50" = "0.05",
               "0-50" = "0.2",
               "0-50" = "0.5",
               "0-50" = "0.6",
               "0-50" = "0.7",
               "0-50" = "1",
               "0-50" = "1.5",
               "0-50" = "2",
               "0-50" = "3",
               "0-50" = "3.5",
               "0-50" = "4",
               "0-50" = "4.5",
               "0-50" = "4.7",
               "0-50" = "5",
               "0-50" = "5.5",
               "0-50" = "6",
               "0-50" = "7",
               "0-50" = "7.2",
               "0-50" = "8",
               "0-50" = "9",
               "0-50" = "10",
               "0-50" = "11",
               "0-50" = "12",
               "0-50" = "13",
               "0-50" = "14",
               "0-50" = "15",
               "0-50" = "16",
               "0-50" = "17",
               "0-50" = "18",
               "0-50" = "19",
               "0-50" = "20",
               "0-50" = "21",
               "0-50" = "22",
               "0-50" = "23",
               "0-50" = "25",
               "0-50" = "27",
               "0-50" = "28",
               "0-50" = "29",
               "0-50" = "30",
               "0-50" = "31",
               "0-50" = "32",
               "0-50" = "34",
               "0-50" = "35",
               "0-50" = "36",
               "0-50" = "37",
               "0-50" = "40",
               "0-50" = "41",
               "0-50" = "42",
               "0-50" = "45",
               "0-50" = "50",
               "51-100" = "60",
               "51-100" = "65",
               "51-100" = "67",
               "51-100" = "68",
               "51-100" = "70",
               "51-100" = "75",
               "51-100" = "77",
               "51-100" = "80",
               "51-100" = "82",
               "51-100" = "86",
               "51-100" = "90",
               "51-100" = "92",
               "51-100" = "100",
               "101-150" = "105",
               "101-150" = "110",
               "101-150" = "150",
               "151-200" = "175",
               "151-200" = "180",
               "151-200" = "185",
               ">200" = "430",
               ">200" = "600",
               ">200" = "999")


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

