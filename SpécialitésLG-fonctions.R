library(ggcpesrthemes)
library(cowplot)
library(ggrepel)

source("SpécialitésLG-data.R")

theme_cpesr_setup(authors=c("J. Gossa"),
                  source="DEPP")


plot_global <- function(bac.cible, discipline.cible) {
  df <- spe %>%
    filter(Bac == bac.cible) %>%
    mutate(Discipline = str_detect(Spécialités,discipline.cible)) %>%
    group_by(Niveau,Discipline) %>%
    summarise(
      Effectifs = sum(Effectifs)) %>%
    mutate(
      Niveau=recode(Niveau,"Première" = "1ère"),
      effectifs.labels = ifelse(Effectifs>10000,
                                paste0(round(Effectifs/1000),"k"),
                                Effectifs),
      effectifs.y = ifelse(Effectifs>32000,0,Effectifs))

    ggplot(df, aes(x=Niveau,y=Effectifs, alpha=Discipline, fill=Niveau)) +
      geom_col(color="black") +
      geom_text(aes(y=effectifs.y, label=effectifs.labels), position = "identity", hjust=-0.1) +
      coord_polar(theta = "y", clip="off") +
      scale_alpha_manual(values=c(0,1)) +
      scale_fill_brewer(
        palette = "Paired",
        labels=c(paste("Première"),paste("Terminale"))) +
      # geom_text(aes(label=Niveau),y=0,nudge_x =-0.1,color="white",stat = "unique",hjust=-0.1) +
      # annotate("text", x=1,y=0,label="Terminale") +
      # annotate("text", x=1,y=0,label="Terminale") +
      guides(alpha=FALSE) +
      theme_cpesr_cap(base_family = "sans") + theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y  = element_blank()
      )
}


plot_cospés <- function(bac.cible, discipline.cible, nbspé = 10) {
  df <- spe %>%
    filter(Bac == bac.cible) %>%
    mutate(Discipline = str_detect(Spécialités,discipline.cible)) %>%
    filter(Discipline) %>%
    separate_rows(Spécialités, sep=" ")

  effectifs.disc.1 <- sum(filter(df, Spécialités == discipline.cible, Niveau=="Première")$Effectifs)
  effectifs.disc.T <- sum(filter(df, Spécialités == discipline.cible, Niveau=="Terminale")$Effectifs)

  df.rank <- df %>%
    #filter(Spécialités != discipline.cible, Niveau == "Terminale") %>%
    filter(Niveau == "Terminale") %>%
    group_by(Spécialités) %>%
    summarise(Effectifs = sum(Effectifs)) %>%
    transmute(
      Spécialités = Spécialités,
      Rang = rank(desc(Effectifs)))

  df %>%
    #filter(Spécialités != discipline.cible) %>%
    group_by(Niveau, Spécialités) %>%
    summarise(Effectifs = sum(Effectifs)) %>%
    merge(df.rank) %>%
    mutate(Spécialités = case_when(
      Rang <= nbspé ~ Spécialités,
      TRUE ~ "Autres"
    )) %>%
    group_by(Niveau, Spécialités) %>%
    summarise(
      Effectifs = sum(Effectifs),
      Rang = min(Rang)) %>%
    mutate(
      Ratio = Effectifs/effectifs.disc.1,
      Ratio.label.1 = ifelse(Effectifs/effectifs.disc.1>0.03,
                             scales::percent(Effectifs/effectifs.disc.1, accuracy=1),
                             NA),
      Ratio.label.T = ifelse(Effectifs/effectifs.disc.T>0.09,
                             scales::percent(Effectifs/effectifs.disc.T, accuracy=1),
                             NA)) %>%

    ggplot(aes(x=reorder(Spécialités, -Rang),y=Ratio,fill=Niveau)) +
    geom_col(position="identity",color="black") +
    geom_text(aes(label=Ratio.label.1), position = "identity", direction ="x", hjust=-0.1,vjust=0.5) +
    geom_text(aes(label=Ratio.label.T), position = "identity", direction ="x", hjust=1.1,vjust=1.7,size=3,color="white") +
    coord_flip(clip="off") +
    scale_fill_brewer(
      palette = "Paired",
      labels=c("Première","Première et Terminale")) +
    scale_y_continuous(labels = scales::percent, name="Ratio des élèves ayant suivi la spécialité en Première") +
    guides(fill=FALSE) +
    theme_cpesr(base_family = "sans") + theme(
      plot.margin = margin(0,20,0,0),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color="grey",size=0.2))
}

plots <- function(bac.cible,discipline.cible) {
  plot_grid(nrow=1, rel_widths = c(1, 2),
    plot_global(bac.cible,discipline.cible),
    plot_cospés(bac.cible,discipline.cible)
    )
}
