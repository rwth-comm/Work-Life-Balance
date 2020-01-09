#Analyse Skript


#### Bibliotheken laden

library(tidyverse)

source("qualtricshelpers.R")

#### Datei laden

raw <- read_csv("Omnibusbefragung.csv")
Omnibusbefragung <- "Omnibusbefragung.csv"
raw <- load_qualtrics_csv(Omnibusbefragung)


#### Daten bereinigen

### Schritt 1: Unnötige Spalten löschen

raw.short <- raw[-201:-505,c(-1:-17, -36:-79, -110:-114, -120:-124)]

### Schritt 2: Variablen unbenennen

codebook.csv <- generate_codebook(raw.short, Omnibusbefragung, "codebook.csv")

codebook <- read_codebook("codebook_final.csv")

## neue Namen auf die Daten anwenden:
names(raw.short) <- codebook$variable

### Schritt 3: Variablen den richtigen Typen zuordnen

## Gender zu kategorialer Variable machen:
raw.short$gender <- as.factor(raw.short$gender)

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

## Job zu kategorialer Variable machen:
raw.short$job <- as.factor(raw.short$job)

##  Skala für Likertskala
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


## Schritt 4: Skalen berechnen

library(psych)

schluesselliste <- list(SOFF = c("-feel_work_1", "-free_time_1", "-free_time_2", "free_time_3", "-free_time_4", "-free_time_5", "-free_time_6", "-free_time_7", "free_time_8", "free_time_9"),
                        AI = c("feel_work_7"),
                        FLEX = c("flex_1", "flex_2", "-flex_3", "flex_4", "flex_5", "flex_6", "feel_work_3", "feel_work_11", "flex_feel_1", "flex_feel_2", "-flex_feel_3", "-flex_feel_4"), 
                        ZUFR = c("feel_work_2", "feel_work_8", "-feel_work_9", "feel_work_10", "feel_time_1", "feel_time_2", "feel_time_3", "feel_time_4", "attitude_company_1", "attitude_company_2", "-attitude_company_3", "attitude_company_4", "attitude_company_5"))


## Merke für uns
# SOFF = Switch-Off (1-6)
# AZ = Arbeitszeit (keine Skalenberechnung)
# AI = Arbeitsintensität (1-6)
# AD = Antellungsdauer (keine Skalenberechnung)
# JOB = Art der Tätigkeit (keine Skalenberechnung -> Einteilung?)
# MA = Mobieles Arbeiten (keine Skalenberechnung)
# FLEX = Flexibilität (1-6; work_time_free ausgenommen)
# ZUFR = allg. Arbeitszufriedenheit (1-6)

## Hier werden die Skalen berechnet: 
scores <- scoreItems(schluesselliste, raw.short, missing = TRUE, min = 1, max = 6)

## Die errechneten Scores ATI, VBA usw. werden hinten als Spalten an raw.short angefügt:
data <- bind_cols(raw.short, as_tibble(scores$scores))




