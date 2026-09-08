#' ---
#' title: "SDD I module 10 : Variance II"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 10 du cours SDD I de 2025-2026](https://wp.sciviews.org/sdd-umons-2025/variance2.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

#'
#' ### ANOVA à deux facteurs sans interactions {#anova2nointer}
#'

# Chargement du dialecte SciViews::R avec les modules infer et model
SciViews::R("infer", "model", lang = "fr")
# Lecture et transformation des données (calcul de aspect^5)
read("crabs", package = "MASS", lang = "fr") %>.%
  smutate(., aspect = labelise(
    as.numeric(rear / width),
    "Ratio largeur arrière / max", units = NA)) %>.%
  smutate(., aspect5 = labelise(
    aspect^5,
    "(Ratio largeur arrière /max)^5", units = NA)) %>.%
  sselect(., species, sex, aspect, aspect5) ->
  crabs2

# Graphique de base pour visualiser les interactions
#chart$base(interaction.plot(crabs2$species, crabs2$sex, crabs2$aspect5))

# Version avec chart
crabs2 %>.%
  sgroup_by(., species, sex) %>.%
  ssummarise(., aspect5_groups = mean(aspect5)) %>.%
  print(.) %>.% # Tableau des moyennes par groupes
  chart(data = ., aspect5_groups ~ species %col=% sex %group=% sex) +
    geom_line() +
    geom_point()

# Résumé des données en vue de réaliser une ANOVA à deux facteurs
crabs2 %>.%
  sgroup_by(., species, sex) %>.%
  ssummarise(.,
    "Moyenne aspect^5"  = fmean(aspect5),
    "Variance aspect^5" = fvar(aspect5),
    "N"                 = fnobs(aspect5))

# Visualisation des données en vue d'une ANOVA à deux facteurs
chart(data = crabs2, aspect5 ~ species | sex) +
  geom_boxplot()

# Version améliorée avec les observations et les moyennes
chart(data = crabs2, aspect5 ~ species | sex) +
  geom_boxplot() +
  geom_jitter(width = 0.05, alpha = 0.3) +
  stat_summary(geom = "point", fun = "mean", color = "red", size = 3)

# Test d'homoscédasticité de Batlett pour l'ANOVA à deux facteurs
bartlett.test(data = crabs2, aspect5 ~ interaction(species, sex))

# ANOVA à deux facteurs sans interaction
anova(crabs2_anova2 <- lm(data = crabs2, aspect5 ~ species + sex)) %>.%
  tabularise(.)

# Tests de comparaisons multiples selon la méthode HSD de Tukey
summary(crabs2_posthoc2 <- confint(multcomp::glht(crabs2_anova2,
  linfct = multcomp::mcp(species = "Tukey", sex = "Tukey"))))
oma <- par(oma = c(0, 5.1, 0, 0))
plot(crabs2_posthoc2)
par(oma)
rm(oma)

# Graphique quantile-quantile de notre ANOVA à deux facteurs sans interactions
chart$qqplot(crabs2_anova2)

#'
#' ### ANOVA à deux facteurs croisés complet {#anova2complete}
#'

# Dénombrement des observations par niveaux (ici le plan est balancé)
scount(crabs2, species, sex)

# ANOVA à deux facteurs, modèle complet avec interactions
crabs2_anova2comp <- lm(data = crabs2, aspect5 ~ species * sex)
anova(crabs2_anova2comp) %>.%
  tabularise(.)

#'
#' ### ANOVA à deux facteurs hiérarchisés {#anova2hierar}
#'

# Lecture des données eggs depuis le package faraway
eggs <- read("eggs", package = "faraway")
# Exploration des données
skimr::skim(eggs)

# Correction de l'encodage (techniciens imbriqués dans les laboratoires)
eggs <- smutate(eggs, Technician = interaction(Lab, Technician))
# Vérification des données
skimr::skim(eggs)

# Graphique de description des données adéquat pour une ANOVA à facteurs hiérarchisés
chart(data = eggs, Fat ~ Lab %col=% Technician) +
  geom_jitter(width = 0.05, alpha = 0.5) +
  stat_summary(geom = "point", fun = "mean", color = "red", size = 3)

# Test d'homoscédasticité de Bartlett
bartlett.test(data = eggs, Fat ~ Technician)

# ANOVA avec facteur technicien imbriqué dans le facteur laboratoire
eggs_anova <- lm(data = eggs, Fat ~ Lab + Technician %in% Lab)
anova(eggs_anova) %>.%
  tabularise(.)

# Graphique quantile-quantile des résidus de notre ANOVA à facteurs hiérarchisés
chart$qqplot(eggs_anova)

# Comparaisons multiples par HSD de Tukey en partant de aov()
eggs_aov <- aov(data = eggs, Fat ~ Lab + Technician %in% Lab)
(eggs_posthoc <- TukeyHSD(eggs_aov, "Lab"))
plot(eggs_posthoc)

#'
#' ### Simplification d'une ANOVA à deux facteurs {#anova2simple}
#'

# Simplification des données
eggs %>.%
  sgroup_by(., Technician) %>.%
  ssummarise(.,
    Fat_mean = fmean(Fat),
    Lab      = funique(Lab)) ->
  eggs_means
skimr::skim(eggs_means)

# Résumé des données simplifiées
eggs_means %>.%
  sgroup_by(., Lab) %>.%
  ssummarise(.,
    moyenne  = fmean(Fat_mean),
    variance = fsd(Fat_mean),
    n        = fnobs(Fat_mean))

# Représentation graphique adéquate de nos données résumées
chart(eggs_means, Fat_mean ~ Lab) +
  geom_point() +
  stat_summary(geom = "point", fun = "mean", color = "red", size = 3)

# Test de Bartlett pour l'homoscédasticité
bartlett.test(data = eggs_means, Fat_mean ~ Lab)

# ANOVA à un facteur sur les données moyennées
eggs_means_anova <- lm(data = eggs_means, Fat_mean ~ Lab)
anova(eggs_means_anova) %>.%
  tabularise(.)

# Graphique quantile-quantile des résidus de notre modèle simplifié
chart$qqplot(eggs_means_anova)

#'
#' ### ANOVA avec effet aléatoire (split-plot) {#anova2splitplot}
#'

# Lecture des données Penicillin depuis le package lme4
pen <- read("Penicillin", package = "lme4")
# Premières et dernières lignes du jeu de données
tabularise$headtail(pen, n = 16)

# Détermination du nombre de replicats
replications(data = pen, diameter ~ sample + plate)

# Visualisation des données
chart(data = pen, diameter ~ plate | sample) +
  geom_point()

# Tests de Batlett en fonction de sample et de plate
bartlett.test(data = pen, diameter ~ sample)
bartlett.test(data = pen, diameter ~ plate)

# ANOVA classique à deux facteurs sans interactions
pen_anova <- lm(data = pen, diameter ~ sample + plate)
anova(pen_anova) %>.%
  tabularise(.)

# Graphique quantile-quantile des résidus du modèle
chart$qqplot(pen_anova)

# Graphique des résidus en fonction des valeurs prédites par le modèle
chart$resfitted(pen_anova)

# Ajustement du modèle en parcelles divisées avec lmeTest::lmer()
pen_split_plot <- lmerTest::lmer(data = pen, diameter ~ sample + (1 | plate))
pen_split_plot

# Tableau ANOVA de notre modèle split-plot
anova(pen_split_plot) %>.%
  tabularise(.)

# Intervalles de confiance à 95% sur les paramètres du modèle split-plot
confint(pen_split_plot)

# Comparaisons multiples HSD de Tukey sur sample de notre modèle split-plot
summary(pen_posthoc <- confint(multcomp::glht(pen_split_plot,
  linfct = multcomp::mcp(sample = "Tukey"))))
oma <- par(oma = c(0, 5.1, 0, 0))
plot(pen_posthoc)
par(oma)
rm(oma)

# Graphique quantile-quantile du modèle split-plot
library(broom.mixed) # Required for mixed models
#chart$qqplot(pen_split_plot) # Not implemented yet
pen_split_plot %>.%
  augment(.) %>.%
  car::qqPlot(.$.resid, distribution = "norm",
    envelope = 0.95, col = "Black", xlab = "Quantiles théoriques",
    ylab = "Résidus standardisés")

# Graphique des résidus en fonction des valeurs prédites par le modèle split-plot
#chart$resfitted(pen_split_plot) # Not implemented yet
pen_split_plot %>.%
  augment(.) %>.%
  chart(., .resid ~ .fitted) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  labs(x = "Valeurs prédites", y = "Résidus") +
  ggtitle("Distribution des résidus - pen_split_plot")

# Modèle avec deux facteurs aléatoires
pen_split_plot2 <- lmerTest::lmer(data = pen, diameter ~ (1 | sample) + (1 | plate))
pen_split_plot2

# Intervalles de confiance pour notre modèle avec deux fateurs aléatoires
confint(pen_split_plot2)

#'
#' ### ANOVA avec effet aléatoire (mesures répétées) {#anova2rep}
#'

# Lecture des données sleep du package lme4
sleep <- read("sleepstudy", package = "lme4")
sleep

# Exploration des données
skimr::skim(sleep)

# Graphique adéquat pour visualiser des données pour modèles à mesures répétées
chart(data = sleep, reaction ~ days %col=% subject) +
  geom_line()

# Autre graphique adéquat avec des facettes
chart(data = sleep, reaction ~ days | subject) +
  geom_line()

# Graphique à facette avec représentation d'une tendance linéaire
chart(data = sleep, reaction ~ days | subject) +
  geom_point() +
  stat_smooth(method = "lm") # Ajuste une droite sur les données

# Modèle à mesurées répétées avec lmerTest::lmer()
sleep_rep <- lmerTest::lmer(data = sleep, reaction ~ days + (days | subject))
sleep_rep

# Tableau de l'ANOVA de notre modèle à mesures répétées
anova(sleep_rep) %>.%
  tabularise(.)

# Résumé du modèle
summary(sleep_rep)

# Intervalles de confiance à 95% sur les paramètres du modèle à mesures répétées
confint(sleep_rep)

# Graphique quantile-quantile des résidus du modèle à mesures répétées
library(broom.mixed) # Required for mixed models
#chart$qqplot(sleep_rep) # Not implemented yet
sleep_rep %>.%
  augment(.) %>.%
  car::qqPlot(.$.resid, distribution = "norm",
    envelope = 0.95, col = "Black", xlab = "Quantiles théoriques",
    ylab = "Résidus standardisés")

# Distribution des résidus du modèle à mesures répétées par les valeurs prédites
#chart$resfitted(sleep_rep) # Not implemented yet
sleep_rep %>.%
  augment(.) %>.%
  chart(., .resid ~ .fitted) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  labs(x = "Valeurs prédites", y = "Résidus") +
  ggtitle("Distribution des résidus - pen_split_plot")

#'
#' ### Syntaxe de R {#syntaxr}
#'

# Un vecteur contenant 4 valeurs numériques nommées a, b, c et d
v <- c(a = 2.6, b = 7.1, c = 4.9, d = 5.0)
# Le second élément du vecteur
v[2]
# Le premier et le troisième élément
v[c(1, 3)]
# Les 3 premiers éléments avec la séquence 1, 2, 3 issue de 1:3
v[1:3]

# Tout le vecteur v, sauf le 2ème élément
v[-2]
# Élimination du 2ème et du 3ème élément
v[-(2:3)]
# Élimination du dernier élément
v[-length(v)]

# Élément de v s'appelant 'a'
v['a']
# Les éléments 'b' et 'd'
v[c('b', 'd')]

# Garder le premier et le quatrième élément
v[c(TRUE, FALSE, FALSE, TRUE)]
# Garder le premier et le troisième (recyclage des indices une seconde fois)
v[c(TRUE, FALSE)]

# Déterminer quel élément est plus grand que 3 dans v
v > 3
# Utilisation de cette instruction comme indiçage pour filtrer les éléments de v
v[v > 3]

# Filtrage d'un vecteur en R de base
v[v > 3 & v <= 5]

# Création d'un data frame
df <- dtx_rows(
  ~x, ~y, ~z,
   1,  2,  3,
   4,  5,  6
)
df

# Élément à la première ligne, colonnes 2 et 3
df[1, 2:3]

# Toute la seconde ligne
df[2, ]
# Toute la seconde colonne
df[ , 2]
# Tout le tableau (pas très utile !)
df[ , ]

# Lignes pour lesquelles x est plus grand que 3 et colonnes nommée 'y' et 'z'
df[df$x > 3, c('y', 'z')]

# Récupération d'une colonne d'un data frame
df[[2]]
df[['y']]
df$y

# Remplacer la troisième colonne par des nouvelles valeurs
df[ , 3] <- c(-10, -15)
df
# Ceci donne le même résultat
df$z <- c(-10, -15)
df

# Dialecte SciViews::R pour lire un jeu de données cas par variables
SciViews::R
# Lecture des données zooplankton
zoo <- read("zooplankton", package = "data.io", lang = "FR")
# Exploration des données
skimr::skim(zoo)

# Tableau de contingence en R de base
table(zoo$class)

# Tbleau de contingence avec interface formule
mosaic::tally(data = zoo, ~ class)

# Tableau de contingence en syntaxe Tidyverse
zoo %>%
  group_by(class) %>%
  summarise(n())

# Fonction Tidyverse spécialisée pour le contingentement
count(zoo, class)

# Filtrage de lignes et sélection de colonnes avec [,] en R de base
zoo2 <- zoo[zoo$class == "Oeuf_allongé" | zoo$class == "Oeuf_rond",
  c("aspect", "area", "class")]
zoo2

# Filtrage de lignes et sélection de colonnes avec le Tidyverse
zoo %>.%
  dplyr::filter(., class == "Oeuf_allongé" | class == "Oeuf_rond") %>.%
  dplyr::select(., aspect, area, class) ->
  zoo2
zoo2

# Calcul d'une nouvelle variable en R de base
zoo2$log_area <- log10(zoo2$area)
head(zoo2)

# Calcul d'une nouvelle variable avec mutate() du Tidyverse
zoo2 <- mutate(zoo2, log_area = log10(area))
head(zoo2)

zoo2 <- mutate_(zoo2, log_area = ~log10(area))
head(zoo2)

# Tous les niveaux sont toujours là
levels(zoo2$class)
# Ne retenir que les niveaux relatifs aux œufs
zoo2$class <- droplevels(zoo2$class)
# C'est mieux !
levels(zoo2$class)

# Graphique en R de base
plot(zoo2$log_area, zoo2$aspect, col = zoo2$class)
legend("bottomright", legend = c("Oeuf allongé", "Oeuf rond"), col = 1:2, pch = 1)

# Idem, mais avec interface formule
plot(data = zoo2, aspect ~ log_area, col = class)
legend("bottomright", legend = c("Oeuf allongé", "Oeuf rond"), col = 1:2, pch = 1)

# Graphique lattice (xyplot) à partir de chart()
chart$xyplot(data = zoo2, aspect ~ log_area, groups = zoo2$class, auto.key = TRUE)

# Graphique ggplot2 (Tidyverse)
ggplot(data = zoo2, aes(x = log_area, y = aspect, col = class)) +
  geom_point()

# chart() avec aes() 
chart(data = zoo2, aes(x = log_area, y = aspect, col = class)) +
  geom_point()

# chart() avec une formule élargie
chart(data = zoo2, aspect ~ log_area %col=% class) +
  geom_point()
