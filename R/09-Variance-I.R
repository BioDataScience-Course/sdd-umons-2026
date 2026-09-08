#' ---
#' title: "SDD I module 9 : Variance I"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 9 du cours SDD I de 2025-2026](https://wp.sciviews.org/sdd-umons-2025/variance.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

#'
#' ### ANOVA à un facteur {#anova1fact}
#'

# Configuration de R pour le dialecte SciViews::R avec les modules infer et model
SciViews::R("infer", "model", lang = "fr")

# Lecture du jeu de données depuis le package MASS
crabs <- read("crabs", package = "MASS", lang = "fr")
# Création d'une variable de groupe combinant espèce et sexe avec label correct
crabs2 <- smutate(crabs, group  = labelise(
    factor(paste(species, sex, sep = "-")),
    "Groupe espèce - sexe", units = NA))

# Graphique de comparaison de la largeur à l'arrière en fonction du groupe
chart(data = crabs2, rear ~ group) +
  geom_violin() +
  geom_jitter(width = 0.05, alpha = 0.5) +
  stat_summary(geom = "point", fun = "mean", color = "red", size = 3)

# Calcul d'une variable aspect (ratio largeur arrière / largeur max) avec label
crabs2 %>.%
  smutate(., aspect = labelise(
    as.numeric(rear / width),
    "Ratio largeur arrière / max", units = NA)) %>.%
  sselect(., species, sex, group, aspect) ->
  crabs2
# Exploration des données
skimr::skim(crabs2)

# Graphique du ratio largeur arrière / largeur max en fonction du groupe
chart(data = crabs2, aspect ~ group) +
  geom_violin() +
  geom_jitter(width = 0.05, alpha = 0.5) +
  stat_summary(geom = "point", fun = "mean", color = "red", size = 3)

# Test de Bartlett pour vérifier l'homoscédasticité
bartlett.test(data = crabs2, aspect ~ group)

# Nouveau test d'homoscédasticité sur données log-transformées
crabs2 <- smutate(crabs2, log_aspect = log(aspect))
bartlett.test(data = crabs2, log_aspect ~ group)

# Essai d'une transformation puissance 5 pour stabiliser la variance
crabs2 <- smutate(crabs2, aspect5 = aspect^5)
bartlett.test(data = crabs2, aspect5 ~ group)

# Graphique des données avec transformation puissance 5 de aspect
chart(data = crabs2, aspect5 ~ group) +
  geom_violin() +
  geom_jitter(width = 0.05, alpha = 0.5) +
  stat_summary(geom = "point", fun = "mean", color = "red", size = 3) +
  ylab("(ratio largeur arrière/max)^5")

# Tableau descriptif des données en vue d'une ANOVA
crabs2 %>.%
  sgroup_by(., group) %>.%
  ssummarise(.,
    mean  = fmean(aspect5),
    sd    = fsd(aspect5),
    count = fsum(!is.na(aspect5)))

# Analyse de variance à un facteur (présentation avec tabularise)
anova(crabs2_anova <- lm(data = crabs2, aspect5 ~ group)) |>
  tabularise()

# Graphique quantile-quantile de distribution des résidus
chart$qqplot(crabs2_anova, lang = "fr")

#'
#' ### Tests post-hoc {#posthoc}
#'

# Comparaisons multiples avec la méthode HSD de Tukey
# Version textuelle avec summary()
summary(crabs2_posthoc <- confint(multcomp::glht(crabs2_anova,
  linfct = multcomp::mcp(group = "Tukey"))))
# Version graphique avec plot()
oma <- par(oma = c(0, 5.1, 0, 0))
plot(crabs2_posthoc)
par(oma)
rm(oma)

#'
#' ### Test de Kruskal-Wallis {#kruskalwallis}
#'

# Calcul des rangs manuellement (pour illustration)
# Un échantillon exemple au hasard
x <- c(4.5, 2.1, 0.5, 2.4, 2.1, 3.5)
# Tri par ordre croissant
sort(x)
# Remplacement par les rangs
rank(sort(x))

kruskal.test(data = crabs2, aspect ~ group)

# Comparaisons multiples non paramétriques après un test de Kruskal-Wallis
summary(crabs2_kw_comp <- nparcomp::nparcomp(data = crabs2, aspect ~ group))
plot(crabs2_kw_comp)
