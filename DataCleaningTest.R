# Analyse Skript 

#### Bibliotheken laden

library(tidyverse)
source("qualtricshelpers.R")

#### Datei laden

raw <- read_csv("Omnibusbefragung.csv")
Omnibusbefragung <- "Omnibusbefragung.csv"
raw <- load_qualtrics_csv("Omnibusbefragung.csv")

#### Daten bereinigen ----
### Schritt 1: Unnötige Spalten löschen. 
raw.short <- raw[-201:-505,c(-1:-17, -36:-79, -110:-114, -120:-124)]

### Schritt 2: Variablen umbenennen 
generate_codebook(raw.short, Omnibusbefragung, "codebook.csv")
codebook <- read_codebook("codebook_final.csv")

## neue Namen auf die Daten anwenden:
names(raw.short) <- codebook$variable

### Schritt 3: Variablen den richtigen Typen zuordnen

## Gender zu kategorialer Variable machen:
raw.short$gender <- as.factor(raw.short$gender)

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
## Recoding data$SOFF into data$SOFF_ORDERD
data$SOFF_ORDERD <- as.character(data$SOFF)
data$SOFF_ORDERD[data$SOFF == "2.7"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "3.6"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "4"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "4.6"] <- "gut"
data$SOFF_ORDERD[data$SOFF == "3.3"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "3.4"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "3.5"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "4.3"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "4.1"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "3.7"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "2.8"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "3.9"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "2.2"] <- "schlecht"
data$SOFF_ORDERD[data$SOFF == "4.5"] <- "gut"
data$SOFF_ORDERD[data$SOFF == "3.1"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "3.2"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "3.8"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "2.9"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "3"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "4.2"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "2.4"] <- "schlecht"
data$SOFF_ORDERD[data$SOFF == "4.4"] <- "mittelmäßig"
data$SOFF_ORDERD[data$SOFF == "2.5"] <- "schlecht"
data$SOFF_ORDERD[data$SOFF == "4.7"] <- "gut"
data$SOFF_ORDERD[data$SOFF == "2.3"] <- "schlecht"
data$SOFF_ORDERD[data$SOFF == "2.1"] <- "schlecht"
data$SOFF_ORDERD[data$SOFF == "2.6"] <- "mittelmäßig"

##AZ
## Recoding data$workingtime into data$AZ_ORDERD
data$AZ_ORDERD <- data$workingtime
data$AZ_ORDERD[data$workingtime == "unter 20 Std."] <- "geringe Arbeitszeit (bis 29 Std./Woche)"
data$AZ_ORDERD[data$workingtime == "40 Std. bis 50 Std."] <- "durchschnittliche Arbeitszeit (30-50 Std./Woche)"
data$AZ_ORDERD[data$workingtime == "20 Std. bis 29 Std."] <- "geringe Arbeitszeit (bis 39 Std./Woche)"
data$AZ_ORDERD[data$workingtime == "über 50 Std."] <- "überdurchschnittliche Arbeitszeit (über 50 Std./Woche)"
data$AZ_ORDERD[data$workingtime == "30 Std. bis 39 Std."] <- "durchschnittliche Arbeitszeit (30-50 Std./Woche)"
data$AZ_ORDERD[is.na(data$workingtime)] <- NA

##AZZ
## Recoding data$AZZ into data$AZZ_ORDERD
data$AZZ_ORDERD <- as.character(data$AZZ)
data$AZZ_ORDERD[data$AZZ == "3"] <- "zufrieden"
data$AZZ_ORDERD[data$AZZ == "2"] <- "sehr zufrieden"
data$AZZ_ORDERD[data$AZZ == "1"] <- "sehr zufrieden"
data$AZZ_ORDERD[data$AZZ == "6"] <- "unzufrieden"
data$AZZ_ORDERD[data$AZZ == "4"] <- "zufrieden"
data$AZZ_ORDERD[data$AZZ == "5"] <- "unzufrieden"

##AI
## Recoding data$AI into data$AI_ORDERD
data$AI_ORDERD <- as.character(data$AI)
data$AI_ORDERD[data$AI == "3"] <- "mittelmäßig"
data$AI_ORDERD[data$AI == "4"] <- "mittelmäßig"
data$AI_ORDERD[data$AI == "6"] <- "hoch"
data$AI_ORDERD[data$AI == "5"] <- "hoch"
data$AI_ORDERD[data$AI == "1"] <- "gering"
data$AI_ORDERD[data$AI == "2"] <- "gering"

##AD
#geht erst mit neuem Datensatz

##JOB
## Recoding data$edunow into data$JOB_ORDERD
data$JOB_ORDERD <- data$edunow
data$JOB_ORDERD[data$edunow == "Student"] <- NA
data$JOB_ORDERD[data$edunow == "Angestellt (Vollzeit)"] <- "Angestellt"
data$JOB_ORDERD[data$edunow == "Rentner"] <- NA
data$JOB_ORDERD[data$edunow == "Praktikant"] <- "Angestellt"
data$JOB_ORDERD[data$edunow == "Selbstständig"] <- "Selbstständig"
data$JOB_ORDERD[data$edunow == "Freiberuflich (Freelancer)"] <- "Selbstständig"
data$JOB_ORDERD[data$edunow == "Arbeitssuchend"] <- NA
data$JOB_ORDERD[data$edunow == "Werksstudent"] <- "Angestellt"
data$JOB_ORDERD[data$edunow == "Geschäftsführung (Vollzeit)"] <- "Führungsposition"
data$JOB_ORDERD[data$edunow == "Auszubildender"] <- "Angestellt"
data$JOB_ORDERD[data$edunow == "Angestellt (Teilzeit)"] <- "Angestellt"
data$JOB_ORDERD[data$edunow == "Geschäftsführung (Teilzeit)"] <- "Führungsposition"
data$JOB_ORDERD[data$edunow == "Angestellt in Führungsposition (Teilzeit)"] <- "Führungsposition"
data$JOB_ORDERD[data$edunow == "Angestellt in Führungsposition (Vollzeit)"] <- "Führungsposition"
data$JOB_ORDERD[data$edunow == "Schüler"] <- NA
data$JOB_ORDERD[is.na(data$edunow)] <- NA

##MA
## Recoding data$remote_hours into data$MA_ORDERD
data$MA_ORDERD <- as.character(data$remote_hours)
data$MA_ORDERD[data$remote_hours == "Gar nicht"] <- "Gar nicht"
data$MA_ORDERD[data$remote_hours == "Weniger als einen Tag"] <- "geringer Anteil der Woche (≤ 2 Tage/Woche)"
data$MA_ORDERD[data$remote_hours == "Einen Tag"] <- "geringer Anteil der Woche (≤ 2 Tage/Woche)"
data$MA_ORDERD[data$remote_hours == "Zwei Tage"] <- "geringer Anteil der Woche (≤ 2 Tage/Woche)"
data$MA_ORDERD[data$remote_hours == "Drei Tage"] <- "Großteil der Woche (≥ 3 Tage/Woche)"
data$MA_ORDERD[data$remote_hours == "Vier Tage"] <- "Großteil der Woche (≥ 3 Tage/Woche)"
data$MA_ORDERD[data$remote_hours == "Mehr als vier Tage"] <- "Großteil der Woche (≥ 3Tage/Woche)"
data$MA_ORDERD[is.na(data$remote_hours)] <- NA

##FLEX
## Recoding data$FLEX into data$FLEX_ORDERD
data$FLEX_ORDERD <- as.character(data$FLEX)
data$FLEX_ORDERD[data$FLEX == "3.83333333333333"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "3.58333333333333"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "3.66666666666667"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "3"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "4.08333333333333"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "2.83333333333333"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "3.75"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "3.41666666666667"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "3.33333333333333"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "4.83333333333333"] <- "sehr flexibel"
data$FLEX_ORDERD[data$FLEX == "3.5"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "4.41666666666667"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "4"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "2.75"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "4.91666666666667"] <- "sehr flexibel"
data$FLEX_ORDERD[data$FLEX == "3.16666666666667"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "2.91666666666667"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "4.5"] <- "sehr flexibel"
data$FLEX_ORDERD[data$FLEX == "3.25"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "4.16666666666667"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "3.08333333333333"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "4.25"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "2.58333333333333"] <- "unfelxibel"
data$FLEX_ORDERD[data$FLEX == "4.58333333333333"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "4.66666666666667"] <- "sehr flexibel"
data$FLEX_ORDERD[data$FLEX == "2.66666666666667"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "3.91666666666667"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "4.33333333333333"] <- "flexibel"
data$FLEX_ORDERD[data$FLEX == "2.25"] <- "unflexibel"
data$FLEX_ORDERD[data$FLEX == "2.41666666666667"] <- "unflexibel"
data$FLEX_ORDERD[data$FLEX == "2.5"] <- "unfelxibel"

##ZUFR
## Recoding data$ZUFR into data$ZUFR_ORDERD
data$ZUFR_ORDERD <- as.character(data$ZUFR)
data$ZUFR_ORDERD[data$ZUFR == "3.5"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "3"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "2.66666666666667"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "3.25"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "3.41666666666667"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "2.58333333333333"] <- "unzufrieden"
data$ZUFR_ORDERD[data$ZUFR == "2.75"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "3.33333333333333"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "4.25"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "4.5"] <- "sehr zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "3.08333333333333"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "3.16666666666667"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "4.08333333333333"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "3.58333333333333"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "4.16666666666667"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "3.83333333333333"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "4"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "3.91666666666667"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "2.91666666666667"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "3.66666666666667"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "4.33333333333333"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "4.58333333333333"] <- "sehr zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "4.66666666666667"] <- "sehr zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "2.33333333333333"] <- "unzufrieden"
data$ZUFR_ORDERD[data$ZUFR == "3.75"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "4.41666666666667"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "2.83333333333333"] <- "zufrieden"
data$ZUFR_ORDERD[data$ZUFR == "2.5"] <- "unzufrieden"
data$ZUFR_ORDERD[data$ZUFR == "2.41666666666667"] <- "unzufrieden"
data$ZUFR_ORDERD[data$ZUFR == "4.83333333333333"] <- "sehr zufrieden"

##EN
# geht erst mit neuem Datensatz

saveRDS(data, "data.rds")

## Merke für uns
# SOFF_ORDERD = Switch-Off -> schlecht/mittelmäßig/gut
# AZ_ORDERD = Arbeitszeit -> NA/geringe Arbeitszeit (bis 29 Std./Woche)/durchschnittliche Arbeitszeit (30-50 Std./Woche)/überdurchschnittliche Arbeitszeit (über 50 Std./Woche)
# AZZ_ORDERD = Arbeitszeit-Zufriedenheit -> unzufrieden/zufrieden/sehr zufrieden
# AI_ORDERD = Arbeitsintensität -> gering/mittelmäßig/hoch 
# AD_ORDERD = Antellungsdauer -> Mediansplit
# JOB_ORDERD = Art der Tätigkeit -> NA/Selbstständig/Angestellt/Führungsposition
# MA_ORDERD = Mobieles Arbeiten -> NA/Gar nicht/geringer Anteil der Woche (≤ 2 Tage/Woche)/Großteil der Woche (≥ 3 Tage/Woche)
# FLEX_ORDERD = Flexibilität -> unfelxibel/flexibel/sehr flexibel
# ZUFR_ORDERD = allg. Arbeitszufriedenheit -> unzufrieden/zufrieden/sehr zufrieden
# EN_ORDERD = Entferung zur Arbeitsstelle



### questionr -> Addins -> für Median -> icut -> Quantile; Breaks = Anazhl der Gruppen 
### ansonsten -> mutate(Datensatz = case_when(Bedingung))

