library(ggcpesrthemes)
library(cowplot)

source("SpécialitésLG-data.R")

theme_cpesr_setup(authors=c("J. Gossa"),
                  source="DEPP")

plot_global <- function(bac.cible, discipline.cible) {
  spe %>%
    filter(Bac == bac.cible) %>%
    mutate(Discipline = str_detect(Spécialités,discipline.cible)) %>%
    group_by(Niveau,Discipline) %>%
    summarise(
      Effectifs = sum(Effectifs)) %>%
    mutate(Niveau=recode(Niveau,"Première" = "1ère")) %>%
    ggplot(aes(x=Niveau,y=Effectifs, alpha=Discipline, fill=Niveau)) +
    geom_col(color="black") +
    geom_text(aes(label=label_effectifs(Effectifs)), position = position_stack(vjust = .5)) +
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

  effectifs.disc <- sum(filter(df, Spécialités == discipline.cible, Niveau=="Première")$Effectifs)

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
      Ratio = Effectifs/effectifs.disc,
      Ratio.label = ifelse(Ratio>0.03,scales::percent(Ratio, accuracy=1),NA)) %>%

    ggplot(aes(x=reorder(Spécialités, -Rang),y=Ratio,fill=Niveau)) +
    geom_col(position="identity",color="black") +
    geom_text(aes(label=Ratio.label), position = "identity", direction ="x", hjust=-0.1) +
    coord_flip(clip="off") +
    scale_fill_brewer(
      palette = "Paired",
      labels=c("Première","Première et Terminale")) +
    scale_y_continuous(labels = scales::percent, name="Ratio des élèves ayant la spécialité en Première") +
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
