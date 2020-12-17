library(tidyverse)

spe.2019.1 <- read.csv("Spécialités-2019-Première.csv", na.strings = c("VALEUR MANQUANTE")) %>%
  transmute(
    Bac = 2021,
    Niveau = "Première",
    Spécialités = toupper(iconv(Première.2019, from="UTF-8", to="ASCII//TRANSLIT")),
    Effectifs = Effectifs
  )

spe.2020.T <- read.csv("Spécialités-2020-Terminale.csv") %>%
  transmute(
    Bac = 2021,
    Niveau = "Terminale",
    Spécialités = Terminale.2020,
    Effectifs = Effectifs
  )

spe.2020.1 <- read.csv("Spécialités-2020-Première.csv") %>%
  transmute(
    Bac = 2022,
    Niveau = "Première",
    Spécialités = Première.2020,
    Effectifs = Effectifs
  )

spe <- bind_rows(spe.2019.1,spe.2020.T,spe.2020.1)

write.csv(spe, "SpécialitésLG.csv",row.names = FALSE)

spe.noms <- unique(c(unlist(str_split(spe$Spécialités," "))))
