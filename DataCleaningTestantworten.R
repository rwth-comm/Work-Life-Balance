library(tidyverse)
source("qualtricshelpers.R")

filename <- "qualtrics_export.csv"
raw <- load_qualtrics_csv(filename)

raw.short <- raw[-201:-505,c(-1:-17)]

generate_codebook(raw.short, filename, "codebook.csv")

codebook <- read_codebook("codebook_final.csv")

names(raw.short) <- codebook$variable

raw.short$q8 <- as.factor(raw.short$q8)
