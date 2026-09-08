#' ---
#' title: "SDD I module 7 : Distribution & Test Chi carré"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 7 du cours SDD I de 2025-2026](https://wp.sciviews.org/sdd-umons-2025/districhi2.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

# Initie le dialecte SciViews::R avec le module d'inférence
SciViews::R("infer", lang = "fr")

#'
#' ### Distribution binomiale {#distribinom}
#'

# Table de la distribution binomiale avec size = 6 et p = 0.75
(binom_table <- dtx(succès = 0:6,
  probabilité = dbinom(0:6, size = 6, prob = 0.75)))

# Graphique de la distribution binomiale avec size = 6 et p = 0.75
bi <- dist_binomial(size = 6, prob = 0.75)
chart(bi) +
  ylab("Densité de probabilité")

# Graphique de la distribution binomiale avec size = 6 et p = 0.5
chart(dist_binomial(size = 6, prob = 0.5)) +
  ylab("Densité de probabilité")

#'
#' ### Distribution de Poisson {#distripoiss}
#'

# Distribution de Poisson avec lambda = 1
P <- dist_poisson(lambda = 1)
chart(P, xlim = c(0, 8))

# Distribution de Poisson avec lambda = 3
P <- dist_poisson(lambda = 3)
P
quantile(P, p = 1/4)
qpois(p = 1/4, lambda = 3)

#'
#' ### Distribution log-normale {#distrilognorm}
#'

# Distribution log-normale avec mu = 0 et sigma = 0.5
chart(dist_lognormal(mu = 0, sigma = 0.5), xlim = c(0, 4)) +
  ylab("Densité de probabilité")

# Calculs sur la distribution log-normale avec mu = 1 et sigma = 0.5
lN1 <- dist_lognormal(mu = 1, sigma = 0.5) # Arguments mu =, sigma =
lN1 # Attention: lN(mu, variance) 0.5^2 = 0.25
quantile(lN1, p = 0.95)
qlnorm(p = 0.95, meanlog = 1, sdlog = 0.5) # Nom des arguments meanlog= / sdlog=

#'
#' ### Graphique quantile-quantile {#quantilequantile}
#'

## Un **quantile** divise des données quantitatives en deux sous-groupes de telle manière que le groupe contenant les observations plus petites que ce quantile représente un effectif équivalent à la fraction considérée. Donc, un quantile 10% correspondra à la valeur qui sépare le jeu de données en 10% des observations les plus petites et 90% des observations les plus grandes.

# Lecture et remaniement des données zooplancton -> eggs
read("zooplankton", package = "data.io") %>.%
  sfilter(., class == "Egg_elongated") %>.%
  smutate(., log_area = log10(area)) %>.%
  sselect(., area, log_area) ->
  eggs
summary(eggs)

# Histogramme de la surface des œufs
chart(data = eggs, ~ area) +
  geom_histogram(bins = 12)

# Histogramme du log de la surface des œufs
chart(data = eggs, ~ log_area) +
  geom_histogram(bins = 12)

# Graphique quantile-quantile du log de la surface des œufs
car::qqPlot(eggs[["log_area"]], distribution = "norm",
  envelope = 0.95, col = "Black", ylab = "log(area [mm^2])")

#'
#' ### Test Chi carré {#testchi2}
#'

# Données concernant les bec-croisés
crossbill <- dtx(cb = c(rep("left", 1895), rep("right", 1752)))
tabularise$headtail(crossbill)

# Tableau de contingence de crossbill
(crossbill_tab <- table(Cross = crossbill$cb))

# Version formatée avec tabularise du tableau de contingence de crossbill
tabularise(crossbill_tab)

# Test Chi^2 univarié sur le tableau de contingence de crossbill
chisq.test(crossbill_tab, p = c(left = 1/2, right = 1/2), rescale.p = FALSE)

# Test Chi^2 univarié de crossbill formaté avec tabularise
chisq.test(crossbill_tab, p = c(left = 1/2, right = 1/2), rescale.p = FALSE) |>
  tabularise()

# Distribution Chi^2 avec ddl = 3
chi2 <- dist_chisq(3)
chart(chi2) +
  ylab("Densité de probabilité")

# quantile à partir duquel on rejette H0 lorsque ddl = 1
qchisq(0.05, df = 1, lower.tail = FALSE)

# P-value du test (aire à droite du quantile observé ave ddl = 1)
pchisq(5.61, df = 1, lower.tail = FALSE)

# Test Chi^2 sur crossbill : proportions équivalentes, échantillon 10x plus petit
(crossbill_tab2 <- as.table(c(left = 190, right = 175)))
chisq.test(crossbill_tab2, p = c(left = 1/2, right = 1/2), rescale.p = FALSE)

# Jeu de données timolol
timolol <- dtx(
  traitement = c(
    rep("timolol", 160), rep("placebo", 147)),
  patient = c(
    rep("sain", 44), rep("malade", 116), 
    rep("sain", 19), rep("malade", 128))
  )
  tabularise$headtail(timolol)

# Table de contingence à deux entrées pout timolol
(timolol_table <- table(Traitement = timolol$traitement, Résultat = timolol$patient))

# Formatage du tableau de contingence à deux entrées de timolol avec tabularise
(tabularise(timolol_table))

# Test Chi^2 d'indépendance sur timolol
(chi2_timolol <- chisq.test(timolol_table))
cat("Expected frequencies:\n"); chi2_timolol[["expected"]]

# Mise en forme du test Chi^2 d'indépendance sur timolol
tabularise(chi2_timolol)

#'
#' ### Métriques {#metriques}
#'

# Préparation de R et lecture des données crabs du package MASS
SciViews::R("infer", lang = "fr")
crabs <- read("crabs", package = "MASS")

# Table de contingence espèces - sexes pour crabs
table(Espèce = crabs$species, Sexe = crabs$sex)

# Table de contingence formaté avec tabularise
table(Espèce = crabs$species, Sexe = crabs$sex) |>
  tabularise()

# Graphe en violon de front
chart(data = crabs, front ~ species %fill=% sex) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), trim = FALSE)

# Graphe en violon de rear
chart(data = crabs, rear ~ species %fill=% sex) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# Graphe en violon de length
chart(data = crabs, length ~ species %fill=% sex) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# Graphe en violon de width
chart(data = crabs, width ~ species %fill=% sex) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# Graphe en violon de depth
chart(data = crabs, depth ~ species %fill=% sex) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# Graphique en nuage de points de rear en fonction de length
chart(data = crabs, rear ~ length %shape=% species %col=% sex) +
  geom_point()

# Graphique en nuage de points de front en fonction de width
chart(data = crabs, front ~ width %shape=% species %col=% sex) +
  geom_point()

# Calcul et graphique de la métrique (ratio) rear / length
crabs %>.%
  smutate(., rear_length = rear / length) %>.%
  chart(data = ., rear_length ~ species %fill=% sex) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    ylab("Ratio largeur arrière/longueur")

# Calcul et graphique de rear_length2
crabs %>.%
  mutate(., rear_length2 = rear / (0.3 * length + 2.4)) %>.%
  chart(data = ., rear_length2 ~ species %fill=% sex) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    ylab("Ratio largeur arrière/(0.3*longueur + 2.4)")

# Calcul et graphique de front_width
crabs %>.%
  mutate(., front_width = front / (0.43 * width)) %>.%
  chart(data = ., front_width ~ species %fill=% sex) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    ylab("Ratio lobe frontal/(0.43*largeur)")
