library(psych)
library(tidyverse)

source("qualtricshelpers.R")

raw <- read_csv("Omnibusbefragung.csv")

Omnibusbefragung <- "Omnibusbefragung.csv"

raw <- load_qualtrics_csv(Omnibusbefragung)


raw.short <- raw[-201:-505,c(-1:-17)]

generate_codebook(raw.short, Omnibusbefragung, "codebook.csv")

codebook <- read_codebook("codebook_final.csv")

## neue Namen auf die Daten anwenden:
names(raw.short) <- codebook$variable


### Schritt 3: Variablen den richtigen Typen zuordnen

## Gender zu kategorialer Variable machen:
raw.short$gender <- as.factor(raw.short$gender)


##  Skala für Likertskala einmal anlegen
scale.zustimmung1 <-c("Gar nicht zu", 
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

raw.short$ikt_easy_1 <- ordered(raw.short$ikt_easy_1, levels = scale.zustimmung2)
raw.short$ikt_easy_2 <- ordered(raw.short$ikt_easy_2, levels = scale.zustimmung2)
raw.short$ikt_easy_3 <- ordered(raw.short$ikt_easy_3, levels = scale.zustimmung2)
raw.short$ikt_easy_4 <- ordered(raw.short$ikt_easy_4, levels = scale.zustimmung2)
raw.short$ikt_easy_5 <- ordered(raw.short$ikt_easy_5, levels = scale.zustimmung2)


## Namen eindeutig benennen
raw.short$q74_1 <- ordered(raw.short$q74_1, levels = scale.zustimmung2)
raw.short$q74_2 <- ordered(raw.short$q74_2, levels = scale.zustimmung2)
raw.short$q74_3 <- ordered(raw.short$q74_3, levels = scale.zustimmung2)
raw.short$q74_4 <- ordered(raw.short$q74_4, levels = scale.zustimmung2)
raw.short$q74_5 <- ordered(raw.short$q74_5, levels = scale.zustimmung2)
raw.short$q74_6 <- ordered(raw.short$q74_6, levels = scale.zustimmung2)
raw.short$q74_7 <- ordered(raw.short$q74_7, levels = scale.zustimmung2)
raw.short$q74_8 <- ordered(raw.short$q74_8, levels = scale.zustimmung2)
raw.short$q74_9 <- ordered(raw.short$q74_9, levels = scale.zustimmung2)
raw.short$q74_10 <- ordered(raw.short$q74_10, levels = scale.zustimmung2)
raw.short$q74_11 <- ordered(raw.short$q74_11, levels = scale.zustimmung2)
raw.short$q74_12 <- ordered(raw.short$q74_12, levels = scale.zustimmung2)

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

raw.short$ikt_rate_1 <- ordered(raw.short$ikt_rate_1, levels = scale.zustimmung2)
raw.short$ikt_rate_2 <- ordered(raw.short$ikt_rate_2, levels = scale.zustimmung2)
raw.short$ikt_rate_3 <- ordered(raw.short$ikt_rate_3, levels = scale.zustimmung2)
raw.short$ikt_rate_4 <- ordered(raw.short$ikt_rate_4, levels = scale.zustimmung2)
raw.short$ikt_rate_5 <- ordered(raw.short$ikt_rate_5, levels = scale.zustimmung2)

raw.short$attitude_company_1 <- ordered(raw.short$attitude_company_1, levels = scale.zustimmung2)
raw.short$attitude_company_2 <- ordered(raw.short$attitude_company_2, levels = scale.zustimmung2)
raw.short$attitude_company_3 <- ordered(raw.short$attitude_company_3, levels = scale.zustimmung2)
raw.short$attitude_company_4 <- ordered(raw.short$attitude_company_4, levels = scale.zustimmung2)
raw.short$attitude_company_5 <- ordered(raw.short$attitude_company_5, levels = scale.zustimmung2)

scale.zustimmung3 <-c("Ich kenne solche Programme nicht", 
                      "Damit arbeite ich nie", 
                      "Damit arbeite ich selten", 
                      "Damit arbeite ich hin und wieder", 
                      "Damit arbeite ich häufig") 

raw.short$programms_1 <- ordered(raw.short$programms_1, levels = scale.zustimmung3)
raw.short$programms_2 <- ordered(raw.short$programms_2, levels = scale.zustimmung3)
raw.short$programms_3 <- ordered(raw.short$programms_3, levels = scale.zustimmung3)
raw.short$programms_4 <- ordered(raw.short$programms_4, levels = scale.zustimmung3)


scale.zustimmung4 <-c("Nie", 
                      "Weniger als einmal im Monat", 
                      "Weniger als einmal pro Woche", 
                      "Ein- bis zweimal pro Woche", 
                      "Täglich",
                      "mehrmals täglich") 

raw.short$ikt_use <- ordered(raw.short$ikt_use, levels = scale.zustimmung4)
raw.short$ikt_use_2 <- ordered(raw.short$ikt_use_2, levels = scale.zustimmung4)


scale.zustimmung5 <-c("Sehr selten", 
                      "selten", 
                      "eher selten", 
                      "eher häufig", 
                      "häufig",
                      "sehr häufig") 

raw.short$ikt_help_1 <- ordered(raw.short$ikt_help_1, levels = scale.zustimmung5)
raw.short$ikt_help_2 <- ordered(raw.short$ikt_help_2, levels = scale.zustimmung5)
raw.short$ikt_help_3 <- ordered(raw.short$ikt_help_3, levels = scale.zustimmung5)
raw.short$ikt_help_4 <- ordered(raw.short$ikt_help_4, levels = scale.zustimmung5)
raw.short$ikt_help_5 <- ordered(raw.short$ikt_help_5, levels = scale.zustimmung5)
raw.short$ikt_help_6 <- ordered(raw.short$ikt_help_6, levels = scale.zustimmung5)
raw.short$ikt_help_7 <- ordered(raw.short$ikt_help_7, levels = scale.zustimmung5)


scale.zustimmung6 <-c("Vollstes Misstrauen", 
                      "Misstraue ich sehr", 
                      "Misstraue ich etwas", 
                      "Vertraue ich etwas", 
                      "Vertraue ich sehr",
                      "Vollstes Vertrauen") 

raw.short$ikt_help_trust_1 <- ordered(raw.short$ikt_help_trust_1, levels = scale.zustimmung6)
raw.short$ikt_help_trust_2 <- ordered(raw.short$ikt_help_trust_2, levels = scale.zustimmung6)
raw.short$ikt_help_trust_3 <- ordered(raw.short$ikt_help_trust_3, levels = scale.zustimmung6)
raw.short$ikt_help_trust_4 <- ordered(raw.short$ikt_help_trust_4, levels = scale.zustimmung6)
raw.short$ikt_help_trust_5 <- ordered(raw.short$ikt_help_trust_5, levels = scale.zustimmung6)
raw.short$ikt_help_trust_6 <- ordered(raw.short$ikt_help_trust_6, levels = scale.zustimmung6)
raw.short$ikt_help_trust_7 <- ordered(raw.short$ikt_help_trust_7, levels = scale.zustimmung6)


## Schritt 4: Skalen berechnen

library(psych)

schluesselliste <- list(FLEX = c("flex_1", "flex_2", "flex_3", "flex_4", "flex_5", "flex_6"),
                                 ICT_EASY = c("ikt_easy_1", "ikt_easy_2", "ikt_easy_3", "ikt_easy_4", "ikt_easy5"),
                                              SOFF = c("-free_time_1", "free_time_2", "free_time_3", "free_time_4", "free_time_5", "free_time_6", "free_time_7", "free_time_8", "free_time_9",
                                              ))


## Merke für uns
# FLEX
# SOFF
# ZUFR
# AZ
# AI
# JOB

## Recoding raw.short$edunow into raw.short$edunow_rec
# Einteilung von Gruppen bei Job
raw.short$edunow_rec <- fct_recode(raw.short$edunow,
               "Angestellt" = "Angestellt (Vollzeit)",
               "Angestellt" = "Praktikant",
               "Selbstständig" = "Freiberuflich (Freelancer)",
               "Angestellt" = "Werksstudent",
               "Führungsposition" = "Geschäftsführung (Vollzeit)",
               "Angestellt" = "Auszubildender",
               "Angestellt" = "Angestellt (Teilzeit)",
               "Führungsposition" = "Geschäftsführung (Teilzeit)",
               "Führungsposition" = "Angestellt in Führungsposition (Teilzeit)",
               "Führungsposition" = "Angestellt in Führungsposition (Vollzeit)")








