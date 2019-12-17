library(tidyverse)
source("qualtricshelpers.R")

filename <- "qualtrics_export.csv"
raw <- load_qualtrics_csv(filename)

raw.short <- raw[-201:-505,c(-1:-17)]

generate_codebook(raw.short, filename, "codebook.csv")