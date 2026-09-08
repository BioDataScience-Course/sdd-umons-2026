#' ---
#' title: "SDD I module 8 : Moyenne"
#' author: "Ph. Grosjean et G. Engels"
#' date: "2025-2026"
#' output:
#'   html_document:
#'     highlight: kate
#' ---
#' 
#' Document complémentaire au [module 8 du cours SDD I de 2025-2026](https://wp.sciviews.org/sdd-umons-2025/moyenne.html).
#' Distribué sous licence [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.fr).
#' 
#' **Veuillez vous référer au cours en ligne pour les explications et les interprétations de cette analyse.**
#' 
#' [Installer un environnement R](https://github.com/SciViews/svbox/tree/main/svbox2025-native)
#' adéquat pour reproduire cette analyse. 

# Initie le dialecte SciViews::R avec le module d'inférence
SciViews::R("infer", lang = "fr")

#'
#' ### Calculs avec la distribution *t* de Student {#distristudent}
#'

# Probabilité de l'aire à droite du quantile 8.5, t(n = 9; mu = 8, sd = 2/3)
mu <- 8
s <- 2/3
pt((8.5 - mu)/s, df = 8, lower.tail = FALSE)

# Quantile définissant une aire à gauche de 5%, t(n = 9; mu = 8, sd = 2/3)
mu <- 8
s <- 2/3
mu + s * qt(0.05, df = 8, lower.tail = TRUE)

# Résolution des deux questions précédentes en utilisant dist_student()
student_t <- dist_student_t(df = 8, mu = 8, sigma = 2/3)
# Aire à droite du quantile 8.5 (1 - x car cdf() renvoie toujours l'aire à gauche)
1 - cdf(student_t, 8.5)
# Quantile délimitant une aire à gauche de 5%
quantile(student_t, 0.05)

# Aire à gauche de 8 - 0.5 + à droite de 8 + 0.5
student_t <- dist_student_t(df = 8, mu = 8, sigma = 2/3)
(left_area <- cdf(student_t, 7.5))
(right_area <- 1 - cdf(student_t, 8.5))
left_area + right_area

# Idem, mais de manière plus concise
cdf(student_t, 7.5) * 2

# Quantile à gauche
quantile(student_t, 0.025)
# Quantile à droite
quantile(student_t, 1 - 0.025)

#'
#' ### Test *t*t de Student {#testt}
#'

crabs <- read("crabs", package = "MASS", lang = "fr")
skimr::skim(crabs)

chart(data = crabs, rear ~ sex) +
  geom_boxplot()

# Test t de Student : moyenne de la largeur arrière de catace en fonction du sexe
t.test(data = crabs, rear ~ sex,
  alternative = "two.sided", conf.level = 0.95, var.equal = TRUE)

# Variante de Welch du test t de Student avec variances inégales
t.test(data = crabs, rear ~ sex,
  alternative = "two.sided", conf.level = 0.95, var.equal = FALSE)

# Test t de Student unilatéral à droite avec variances inégales
t.test(data = crabs, rear ~ sex,
  alternative = "greater", conf.level = 0.95, var.equal = FALSE)

# Calcul de delta f-r et de t_obs pour le test t apparié
crabs %>.%
  smutate(., delta_f_r = front - rear) %>.%
  ssummarise(.,
    mean_f_r = fmean(delta_f_r),
    se_f_r   = fsd(delta_f_r) / sqrt(fnobs(delta_f_r))) %>.%
  smutate(., t_obs = mean_f_r / se_f_r)

# Valeur p de ce test
pt(25.324, df = 199, lower.tail = FALSE) * 2

# Graphique largeur arrière en fonction de la largeur à l'avance
chart(data = crabs, rear ~ front) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

# Test t de Student apparié
t.test(crabs$front, crabs$rear,
  alternative = "two.sided", conf.level = 0.95, paired = TRUE)

# Exemple d'un jeu de données encodé incorrectement
sleep <- read("sleep", package = "datasets")
tabularise(sleep)

# Restructuration du tableau pour un test t apparié
sleep2 <- spivot_wider(sleep, names_from = group, values_from = extra)
names(sleep2) <- c("id", "med1", "med2")
tabularise(sleep2)

# Graphique de ces données
chart(data = sleep2, med2 ~ med1) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

# Test t de Student apparié
t.test(sleep2$med1, sleep2$med2,
  alternative = "two.sided", conf.level = 0.95, paired = TRUE)

# Graphique de med1
chart(data = sleep2, med1 ~ "") +
  geom_boxplot() +
  geom_hline(yintercept = 0, col = "red") +
  xlab("") +
  ylab("Sommeil supplémentaire avec med1 [h]")

# Test t de Student univarié sur med1
t.test(sleep2$med1,
  alternative = "two.sided", mu = 0, conf.level = 0.95)

#'
#' ### Test de Wilcoxon {#testwilcoxon}
#'

# Test de Wilcoxon-Mann-Whitney univarié sur med1
wilcox.test(sleep2$med1,
  alternative = "two.sided", mu = 0, conf.level = 0.95)

# Test de Wilcoxon-Mann-Whitney indépendant unilatéral à droite
wilcox.test(data = crabs, rear ~ sex,
  alternative = "greater", conf.level = 0.95)

#'
#' ### Puissance d'un test et représentation graphique {#puissanceplot}
#'

# Calcul de puissance d'un test t de Student
pwr::pwr.t.test(n = 10, d = 1.3, sig.level = 0.05,
  type = "one.sample", alternative = "two.sided")

# Représentation graphique des données pour un test t de Student ou de Wilcoxon
a <- chart(data = crabs, rear ~ sex) +
  stat_summary(geom = "col", fun = "mean") +
  stat_summary(geom = "errorbar", width = 0.1,
    fun.data = "mean_cl_normal", fun.args = list(conf.int = 0.95))

b <- chart(data = crabs, rear ~ sex) +
  geom_jitter(alpha = 0.3, width = 0.2) +
  stat_summary(geom = "point", fun = "mean", size = 2) +
  stat_summary(geom = "errorbar", width = 0.1,
    fun.data = "mean_cl_normal", fun.args = list(conf.int = 0.95), linewidth = 1)

combine_charts(list(a,b))

# Meilleure représentation graphique pour un test de Wilcoxon-Mann-Withney
chart(data = crabs, rear ~ sex) +
  geom_boxplot()

# Meilleure représentation graphique pour un test t de Student
chart(data = crabs, rear ~ sex) +
  geom_jitter(alpha = 0.3, width = 0.2) +
  stat_summary(geom = "point", fun = "mean") +
  stat_summary(geom = "errorbar", width = 0.1,
    fun.data = "mean_cl_normal", fun.args = list(conf.int = 0.95))

# 4 graphiques avec différentes barres d'erreurs (IC95%, écart type, 2*écart type et erreur standard)
p <- chart(data = crabs, rear ~ sex) +
  geom_jitter(alpha = 0.1, width = 0.2) +
  stat_summary(geom = "point", fun = "mean") +
  scale_y_continuous(limits = c(5,22))

a <- p +
  stat_summary(geom = "errorbar", width = 0.1,
    fun.data = "mean_cl_normal", fun.args = list(conf.int = 0.95)) 

b <- p +
  stat_summary(geom = "errorbar", width = 0.1,
    fun.data = "mean_sdl", fun.args = list(mult = 1))

c <- p +
  stat_summary(geom = "errorbar", width = 0.1,
    fun.data = "mean_sdl", fun.args = list(mult = 2)) 

d <- p + 
  stat_summary(geom = "errorbar", width = 0.1,
    fun.data = "mean_se", fun.args = list(mult = 1))

combine_charts(list(a,b,c,d))
