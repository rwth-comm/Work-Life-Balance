library(tidyverse)
source("qualtricshelpers.R")


raw <- read_csv("Datensatz.csv")
Datensatz <- "Datensatz.csv"
raw <- load_qualtrics_csv(Datensatz)


# Daten bereinigen
# unnötige Spalten löschen

raw <- filter(raw, Status == "IP Address")





# Variablen umbenennen
