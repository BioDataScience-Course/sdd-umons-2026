#' ---
#' title: "SDD I module 6 : Probabilités, distributions I & corrélation"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 6 du cours SDD I de 2025-2026](https://wp.sciviews.org/sdd-umons-2025/probacorr.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

# Initie le dialecte SciViews::R avec le module d'inférence
SciViews::R("infer", lang = "fr")

## En statistique, nous appelons cela un **tirage au sort sans remise**. Le résultat est très différent si le premier individu tiré au hasard était remis dans la population et pouvait être éventuellement pris à nouveau au second ou troisième tirage (**tirage au sort avec remise**). Notez aussi que, pour une population de taille infinie ou très grande, les deux types de tirage au sort sont équivalents à celui **avec** remise, car enlever un individu d'une population infinie ne change pas fondamentalement son effectif, donc les probabilités ultérieures.

#'
#' ### Distribution uniforme {#distriuni}
#'

dtx(Portée = 1:4, Probabilité = 1/4) %>.%
  chart(., aes()) +
    geom_segment(aes(x = Portée, xend = Portée, y = 0, yend = Probabilité)) +
  ylab("Probabilité")

# Probabilité pour le quantile 2.5 de la distribution uniforme [0, 4]
punif(2.5, min = 0, max = 4, lower.tail = TRUE)

# Probabilité comprise entre 2 et 2.5 pour la distribution uniforme [0, 4]
punif(2.5, min = 0, max = 4, lower.tail = TRUE) -
punif(2.0, min = 0, max = 4, lower.tail = TRUE)

# Quantile pour une probabilité d'1/3 de la distribution uniforme [0, 4]
qunif(1/3, min = 0, max = 4, lower.tail = TRUE)

# Distribution uniforme [0, 4]
U <- dist_uniform(min = 0, max = 4)
U

# Fonctions donnant un résultat identique
cdf(U, q = 3); punif(q = 3, min = 0, max = 4)
quantile(U, p = 1/3); qunif(p = 1/3, min = 0, max = 4, lower.tail = TRUE)
density(U, 1:3)[[1]]; dunif(1:3, min = 0, max = 4)
set.seed(575); generate(U, 5)[[1]]
set.seed(575); runif(5, min = 0, max = 4)

# Initialisation du générateur de nombre pseudo-aléatoires (pour la reproductibilité)
set.seed(946)
# Génération de 10 nombres selon la distribution uniforme [0, 1]
runif(10, min = 0, max = 1) # Série de 10 nombres les mêmes à chaque exécution

# Information à propos de la distribution U
format(U)
parameters(U)
mean(U)
variance(U)
support(U)

# Graphique de la distribution U
chart(U) +
  geom_funfill(fun = dfun(U), from = 1, to = 3) +
  annotate("text", x = 2, y = 0.10, label = "P[1, 3]", col = "red")

# Graphique de densité de probabilité cumulée de la distribution U
chart$cumulative(U) +
  geom_funfill(fun = cdfun(U), from = 1, to = 3)

#'
#' ### Distribution normale {#distrinorm}
#'

# Distribution normale N(12, 1.5^2)
N1 <- dist_normal(mu = 12, sigma = 1.5) # Arguments mu =, sigma =
N1 # Attention: N(mu, variance) 1.5^2 = 2.2
quantile(N1, p = 0.95)
qnorm(p = 0.95, mean = 12, sd = 1.5) # Notez le nom des arguments mean = et sd =

#'
#' ### Corrélation {#correl}
#'

# Jeu de données artificiel
set.seed(653643)
df <- dtx(
  x  =  rnorm(100),
  y1 = x + rnorm(100, sd = 0.2),
  y2 = rnorm(100),
  y3 = -x + rnorm(100, sd = 0.2))

# Graphiques
pl <- list(
  chart(data = df, y1 ~ x) + geom_point(),
  chart(data = df, y2 ~ x) + geom_point(),
  chart(data = df, y3 ~ x) + geom_point()
)
combine_charts(pl, ncol = 3L)

# Covariance de df
cov(df$x, df$y1)
cov(df$x, df$y2)
cov(df$x, df$y3)

# Covariance de df2 == df * 10
df2 <- df * 10
cov(df2$x, df2$y1)
cov(df2$x, df2$y2)
cov(df2$x, df2$y3)

# Corrélation de df
cor(df$x, df$y1)
cor(df2$x, df2$y1)
cor(df$x, df$y2)
cor(df2$x, df2$y2)
cor(df$x, df$y3)
cor(df2$x, df2$y3)

# Jeu de données trees
trees <- read("trees", package = "datasets")
tabularise$headtail(trees)
trees_cor <- correlation(trees)
trees_cor |> tabularise()

# Résumé de la corrélation de trees
summary(trees_cor)

# Graphique de la corrélation de trees
plot(trees_cor)

# Autre exemple de corrélation sur le zooplancton
zoo <- read("zooplankton", package = "data.io")
zoo %>.%
  sselect(., size:density) %>.%
  correlation(.) ->
  zoo_cor
plot(zoo_cor)

# Graphique de la matrice de corrélation de zoo
plot(zoo_cor, type = "lower")

# Jeu de données Anscombe
anscombe <- read("anscombe", package = "datasets")
head(anscombe)

# Deux variables d'Anscombe
ans_x <- anscombe[, 1:4]
ans_y <- anscombe[, 5:8]

# Description statistique de X
fmean(ans_x)
fvar(ans_x)
fsd(ans_x)

# Description statistique de Y
fmean(ans_y)
fvar(ans_y)
fsd(ans_y)

# Coefficients de corrélation entre x1 et y1, x2 et y2, ...
diag(correlation(ans_x, ans_y))

# Graphique d'Anscombe
pl <- list(
  chart(data = anscombe, y1 ~ x1) + geom_point(),
  chart(data = anscombe, y2 ~ x2) + geom_point(),
  chart(data = anscombe, y3 ~ x3) + geom_point(),
  chart(data = anscombe, y4 ~ x4) + geom_point()
)
combine_charts(pl)

# Matrice de nuage de points de trees
GGally::ggscatmat(trees, 1:3)

# Matrice variance-covariance de trees
cov(trees) |> tabularise()

# Matrice de corrélation de Spearman pour trees
correlation(trees, method = "spearman")

# Matrice de corrélation de Kendall pour trees
correlation(trees, method = "kendall")

# Matrice de corrélation de Pearson pour trees
correlation(trees) # Équivalent à method = "pearson"

# Test de corrélation pour trees, variables diameter et volume
cor.test(data = trees, ~ diameter + volume, alternative = "greater")

# Idem, mais mise en forme du tableau avec tabularise()
cor.test(data = trees, ~ diameter + volume, alternative = "greater") |> tabularise()

# Test de corrélation entre diameter et height pour trees
cor.test(data = trees, ~ diameter + height, alternative = "greater") |> tabularise()

# Idem, mais corrélation de Spearman
trees_cor_test <- cor.test(data = trees, ~ diameter + height,
  alternative = "greater", method = "spearman")

# Mise en forme du tableau avec tabularise()
tabularise(trees_cor_test)

# Idem, mais corrélation de Kendall
trees_cor_test <- cor.test(data = trees, ~ diameter + height,
  alternative = "greater", method = "kendall")

# Mise en forme du tableau avec tabularise()
tabularise(trees_cor_test)
