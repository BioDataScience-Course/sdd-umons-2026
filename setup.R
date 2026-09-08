# Learnitdown configuration and functions
# We use glue() often for variables replacement from learnitdown, so, we
# define the `!` operator for character objects to glue the string
# Cons: it slightly slows down the usual `!` operator (10x slower)
`!` <- function(x) {
  if (is.character(x)) {
    as.character(glue::glue_data(learnitdown, x))
  } else {# Usual ! operator
    .Primitive('!')(x)
  }
}

# General configuration data
learnitdown <- list(
  baseurl = "https://wp.sciviews.org", # The base URL for the site
  connecturl = "https://sdd.umons.ac.be", # The URL for the Posit Connect server
  imgbaseurl =
    "https://filedn.com/lzGVgfOGxb6mHFQcRn9ueUb/sdd-umons", # The base URL for external (big) images
  shiny_imgdir = "images/shinyapps",   # The Shiny image directory (screenshots)
  svbox = 2025,                        # The SciViews Box version used
  rstudio = "start_rstudio2025.html",  # Run Rstudio from the box
  package = "BioDataScience1",         # Associated package for the exercises
  institutions = "UMONS",              # Known institutions
  sets = "25M",                        # The course set (in case there are several ones)
  courses = c(
    "S-BIOG-006",                      # SDD1 Q1 Mons
    "S-BIOG-027" #,                      # SDD1 Q2 Mons
    #"S-BIOG-921"                       # SDD1 Charleroi
  ),
  courses_names = c(
    "Science des Données Biologiques I : visualisation à l'UMONS",
    "Science des Données Biologiques I : inférence à l'UMONS" #,
    #"Bio-Informatique et Science des Données à Charleroi"
  ),
  terms = c("Q1", "Q2"),                 # The term of each course
  acad_year = "2025-2026",               # The academic year
  YYYY = 2025,                           # The academic year long id
  YY = 25,                               # The academic year short id
  W = as.Date("2025-09-07") + (0:37)*7,  # Sundays before each academic week
  Q1 = as.Date("2025-09-07") + (0:15)*7, # There are 15 weeks at Q1
  Q2 = as.Date("2026-02-01") + c(0:11, 14:16)*7 # Q2 starts 02/02 w22 but w33-34 are holidays
)

# Course start and end dates
learnitdown$course_start <- !"{W[2]+1}"
learnitdown$course_end   <- !"{W[36]+2}"

# Modules dates
learnitdown$mod <- as.data.frame(tibble::tribble(
  ~id,       ~term,       ~start,       ~class1,         ~end,       ~class2,         ~N3,            ~N4,   ~challenge,        ~test,
  # Q1
  "install",  "Q1",  !"{W[2]+1}", "13:30-15:30",          "-",          "-",          "-",            "-",          "-",          "-",
  "A01",      "Q1",  !"{W[3]+1}", "13:30-15:30",  !"{W[4]+1}", "13:30-17:30", !"{W[4]+3}",            "-",          "-",  !"{W[4]+1}",
  "A02",      "Q1",  !"{W[5]+1}", "13:30-15:30",  !"{W[5]+5}", "08:15-12:30", !"{W[6]+1}",    !"{W[5]+5}",          "-",  !"{W[5]+5}",
  "remed1",   "Q1",  !"{W[6]+1}", "13:30-16:30",          "-",           "-",         "-",            "-",          "-",          "-",
  "remed2",   "Q1",  !"{W[7]+1}", "13:30-16:30",          "-",           "-",         "-",            "-",          "-",          "-",
  "A03",      "Q1", !"{W[10]+1}", "13:30-15:30", !"{W[10]+5}", "08:15-12:30", !"{W[11]+1}", "continue...", !"{W[10]+5}",          "-",
  "A04",      "Q1", !"{W[12]+1}", "13:30-15:30", !"{W[12]+5}", "08:15-12:30", !"{W[13]+1}", "continue...",          "-", !"{W[12]+5}",
  "A05",      "Q1", !"{W[14]+1}", "13:30-15:30", !"{W[14]+5}", "08:15-12:30", !"{W[15]+1}",  !"{W[15]+2}",          "-", !"{W[14]+5}",
  # Q2
  "A06",      "Q2", !"{W[23]+1}", "13:30-15:30", !"{W[23]+5}", "08:15-12:30", !"{W[24]+1}",           "-",          "-", !"{W[23]+5}",
  "A07",      "Q2", !"{W[25]+1}", "13:30-15:30", !"{W[25]+5}", "08:15-12:30", !"{W[26]+1}",  !"{W[25]+5}",          "-", !"{W[25]+5}",
  "A08",      "Q2", !"{W[27]+1}", "13:30-15:30", !"{W[27]+5}", "08:15-12:30", !"{W[28]+1}", "continue...",          "-", !"{W[27]+5}",
  "A09",      "Q2", !"{W[29]+1}", "13:30-15:30", !"{W[29]+5}", "08:15-12:30", !"{W[30]+1}", "continue...",          "-", !"{W[29]+5}",
  "remed3",   "Q2", !"{W[32]+1}", "13:30-17:30",          "-",          "-",           "-",           "-",          "-",          "-",
  "A10",      "Q2", !"{W[35]+1}", "13:30-15:30", !"{W[35]+5}", "08:15-12:30", !"{W[36]+1}",  !"{W[36]+2}", !"{W[35]+5}",          "-"
))
rownames(learnitdown$mod) <- learnitdown$mod$id

# Assignment URLS
learnitdown$assign_url <- list(
  # Q1
  A00Qa_issues         = "https://classroom.github.com/a/NRpZGtDz",
  A01Ia_scatterplot    = "https://classroom.github.com/a/_XG5igiU",
  A02Ia_distributions  = "https://classroom.github.com/a/k2L6oNfO",
  A02Ga_analysis       = "https://classroom.github.com/a/Bop98YHF",
  A03Ia_graphe_avance  = "https://classroom.github.com/a/pNd8091z",
  A03Ca_charts         = "https://classroom.github.com/a/...",
  A04Ia_transformation = "https://classroom.github.com/a/3g3BVLpd",
  A04Ga_biometry       = "https://classroom.github.com/a/blC-3zPZ",
  A05Ia_belgium_demo   = "https://classroom.github.com/a/CQCQZ2OS",
  # Q2
  A06Ia_correlation    = "https://classroom.github.com/a/zBiX2yci",
  A07Ia_pea            = "https://classroom.github.com/a/do5k6xnr",
  A07Ga_human          = "https://classroom.github.com/a/1oznwQf6",
  A08Ia_ttest          = "https://classroom.github.com/a/4UQuKF6f",
  A09Ia_acidification  = "https://classroom.github.com/a/QvelyvsN",
  A10Ia_anova2         = "https://classroom.github.com/a/lrYFp5CX",
  A10Ca_infer          = "https://classroom.github.com/a/..."
)

# Date and time for start and end of classes for each module
class1_start <- function(x, module)
  paste0(x[module, 'start'], " ", substring(x[module, 'class1'], 1, 5), ":00")
class1_end <- function(x, module)
  paste0(x[module, 'start'], " ", substring(x[module, 'class1'], 7, 11), ":00")
class2_start <- function(x, module)
  paste0(x[module, 'end'], " ", substring(x[module, 'class2'], 1, 5), ":00")
class2_end <- function(x, module)
  paste0(x[module, 'end'], " ", substring(x[module, 'class2'], 7, 11), ":00")
n3_end <- function(x, module, hour = "23:59:59")
  paste0(x[module, 'N3'], " ", hour)
n4_end <- function(x, module, hour = "23:59:59")
  paste0(x[module, 'N4'], " ", hour)

# Examples:
#!"svbox{svbox} is for academic year {acad_year}"
#  -> svbox2025 is for academic year 2025-2026

# Link inside the courses: the link are a little bit more complex because the
# bookdown is embedded in a Wordpress site. A direct link like:
# https://wp.sciviews.org/sdd-umons2-2025/outils-de-diagnostic-suite.html#résumé-avec-summarysuite
# becomes:
# https://wp.sciviews.org/sdd-umons2/?iframe=wp.sciviews.org/sdd-umons2-2025/outils-de-diagnostic-suite.html%23résumé-avec-summarysuite
course_link <- function(label, course = 1, page, anchor = "", year = !"{YYYY}",
    baseurl = !"{baseurl}", course_page = "sdd-umons") {
  if (course == 1) {
    course <- ""
  } else{
    course <- as.character(course)
  }
  if (anchor != "")
    anchor <- paste0("%23", anchor) # %23 is the URLencoded value of '#'
  baseurl2 <- sub("https://", "", baseurl, fixed = TRUE) # Remove https://
  url <- glue::glue("{baseurl}/{course_page}{course}/?iframe={baseurl2}/{course_page}{course}-{year}/{page}.html{anchor}")
  paste0("[", label, "](", url, ")")
}
# ex.: course_link("diagnostic", 2, "outils-de-diagnostic-suite", "résumé-avec-summarysuite")

## Big images (animated gifs, ...) are stored externally, refer them this way:
#
# `r img("sub_dir", "image_name.gif")`
# or to add a caption (use ''' instead of ` if you need it in your caption):
# # `r img("sub_dir", "image_name.gif", caption = "A nice picture.")`

## h5p(), launch_shiny(), learnr() & assignation() for exercises blocks, use:
#
# `r h5p(x, toc = "Short description")`
#
# `r launch_shiny(url, toc = "Short description")`
#
# `r learnr(id, title = "...", toc = "Short description")`
#
#```{r assign_A01Ia_markdown, echo=FALSE, results='asis'}
#if (exists("assignment"))
#  assignment("A01Ia_markdown", part = NULL,
#    url = "https://github.com/BioDataScience-Course/A01Ia_markdown",
#    course.ids = c(
#      'S-BIOG-015'         = !"A01Ia_{YY}M_markdown",
#      'S-BIOG-937-958-959' = !"A01Ia_{YY}C_markdown",
#      'late_mons'          = !"A01Ia_{YY}M_markdown"),
#    course.urls = c(
#      'S-BIOG-015'         = !"{assign_url$A01Ia_markdownM}",
#      'S-BIOG-937-958-959' = !"{assign_url$A01Ia_markdownC}",
#      'late_mons'          = !"{assign_url$A01Ia_markdownM}"),
#    course.starts = c(
#      'S-BIOG-015'         = !"{class1_start(mod, 'A01')}",
#      'S-BIOG-937-958-959' = NA, # Nondefined date, or just ignore it
#      'sdd1late'           = !"{class2_start(mod, 'A01')}"),
#    course.ends = c(
#      'S-BIOG-015'         = !"{n3_end(mod, 'A01')}",
#      'sdd1late'           = !"{n3_end(mod, 'A02')}"),
#    term = "Q1", level = 3,
#    toc = "Réalisation d'un premier document en Markdown")
#```
# Use assignment2() for group assignment, and challenge() or challenge2()
# for assignments that are linked to challenges
#
# Then, at the end of the module, create the exercises toc with:
#
# `r show_ex_toc()`
#
# Use `r learnitdown::clean_ex_toc()` at the beginning of index.Rmd to
# make sure the ex dir is clean when the book compiles

img <- function(..., caption = "") {
  path <- paste(learnitdown$imgbaseurl, ..., sep = "/")
  # Cannot use ` inside R code => use ''' instead
  caption <- gsub("'''", "`", caption)
  paste0("![", caption, "](", path, ")")
}

h5p <- function(id, name, toc = "",
  icourse = if (as.integer(substring(id, 2, 3)) < 6L) !"{courses[1]}" else
    !"{courses[2]}",
  institution = !"{institutions[1]}", acad_year = !"{acad_year}",
  term = if (as.integer(substring(id, 2, 3)) < 6L) !"{terms[1]}" else
    !"{terms[2]}",
  set = !"{sets[1]}", ...)
  learnitdown::h5p(id, name = name, toc = toc, baseurl = learnitdown$baseurl,
    toc.def = "Exercice H5P {id}",
    h5p.img = "images/list-h5p.png",
    h5p.link = paste(learnitdown$baseurl, "h5p", sep = "/"),
    icourse = icourse, institution = institution, acad_year = acad_year,
    term = term, set = set, ...)

launch_shiny <- function(url, toc = "", fun = paste(learnitdown$package,
  "run_app", sep = "::"), app = sub("\\?.+$", "", basename(url)),
  #ENalt1 = "*Click to start the Shiny application*",
  alt1 = "*Cliquez pour lancer l'application Shiny.*",
  #ENalt2 = "*Click to start or [run `{run.cmd}`]({run.url}{run.arg}) in RStudio.*",
  alt2 = "*Cliquez pour lancer ou [exécutez dans RStudio]({run.url}{run.arg}){{target=\"_blank\"}} `{run.cmd}`.*",
  baseurl = !"{baseurl}",
  icourse = if (as.integer(substring(app, 2, 3)) < 6L)
    !"{courses[1]}" else !"{courses[2]}",
  institution = !"{institutions[1]}", acad_year = !"{acad_year}",
  term = if (as.integer(substring(app, 2, 3)) < 6L)
    !"{terms[1]}" else !"{terms[2]}",
  set = !"{sets[1]}", ...)
  learnitdown::launch_shiny(url = url, toc = toc, imgdir = learnitdown$shiny_imgdir,
    fun = fun, alt1 = alt1, alt2 = alt2, toc.def = "Application Shiny {app}",
    baseurl = baseurl,
    run.url = paste(learnitdown$baseurl, "/", learnitdown$rstudio,  "?runrcode=", sep = ""),
    app.img = "images/list-app.png",
    app.link = paste(learnitdown$baseurl, "shiny_app", sep = "/"),
    icourse = icourse, institution = institution, acad_year = acad_year,
    term = term, set = set,...)

launch_report <- function(module, course = "S-BIOG-006", toc = NULL, fun = NULL,
  #ENalt1 = "*Click to see the progress report.*",
  alt1 = "*Cliquez pour visualiser le rapport de progression.*",
  #ENalt2 = "*Click to calculate your progress report for this module.*",
  alt2 = "*Cliquez pour calculer votre rapport de progression pour ce module.*",
  height = 800, ...)
  learnitdown::launch_shiny(url =
      paste0("https://sdd.umons.ac.be/sdd-progress-report?course=", course,
        "&module=", module),
    toc = toc, imgdir = learnitdown$shiny_imgdir,
    fun = fun, alt1 = alt1, alt2 = alt2, toc.def = "Progress report {app}",
    run.url = paste(learnitdown$baseurl, "/", learnitdown$rstudio,  "?runrcode=", sep = ""),
    app.img = "images/list-app.png",
    app.link = paste(learnitdown$baseurl, "shiny_app", sep = "/"), height = height, ...)

# Note: not used yet!
launch_learnr <- function(url, toc = "", fun = paste(learnitdown$package, "run", sep = "::"), ...)
  launch_shiny(url = url, toc = toc, fun = fun, ...)

learnr <- function(id, title = NULL, toc = "", package = learnitdown$package,
text = "Effectuez maintenant les exercices du tutoriel",
icourse = if (as.integer(substring(id, 2, 3)) < 6L) !"{courses[1]}" else
  !"{courses[2]}",
institution = !"{institutions[1]}", acad_year = !"{acad_year}",
term = if (as.integer(substring(id, 2, 3)) < 6L) !"{terms[1]}" else
  !"{terms[2]}", set = !"{sets[1]}")
  learnitdown::learnr(id = id, title = title, package = package, toc = toc,
    text = text, toc.def = "Tutoriel {id}",
    #rstudio.url = paste(learnitdown$baseurl, learnitdown$rstudio, sep = "/"),
    connect.url = learnitdown$connecturl, tuto.img = "images/list-tuto.png",
    tuto.link = paste(learnitdown$baseurl, "tutorial", sep = "/"),
    icourse = icourse, institution = institution, acad_year = acad_year,
    term = term, set = set)

# Note: use course.urls = c(`S-BIOG-006` = "classroom url1", `S-BIOG-921` = "classroom url2"), and url = link to Github template repository for the assignment
assignment <- function(name, url, course.ids = NULL, course.urls = NULL,
  course.starts = NULL, course.ends = NULL, part = NULL, toc = "", clone = TRUE,
  level = 3, n = 1, type = "ind. github", institution = !"{institutions[1]}",
  acad_year = !"{acad_year}", term = "Q1", set = !"{sets[1]}",
  texts = learnitdown::assignment_fr())
  learnitdown::assignment(name = name, url = url, course.ids = course.ids,
    course.urls = course.urls, course.starts = course.starts,
    course.ends = course.ends, part = part,
    course.names = stats::setNames(learnitdown$courses_names,
      learnitdown$courses), toc = toc, clone = clone, level = level, n = n,
    type = type, institution = institution, acad_year = acad_year, term = term,
    set = set, texts = texts, assign.img = "images/list-assign.png",
    assign.link = paste(learnitdown$baseurl, "github_assignment", sep = "/"),
    template = "assignment_fr.html", baseurl = learnitdown$baseurl)

assignment2 <- function(name, url, course.ids = NULL, course.urls = NULL,
  course.starts = NULL, course.ends = NULL, part = NULL, toc = "", clone = TRUE,
  level = 4, n = 2, type = "group github", institution = !"{institutions[1]}",
  acad_year = !"{acad_year}", term = "Q1",  set = !"{sets[1]}",
  texts = learnitdown::assignment2_fr())
  learnitdown::assignment2(name = name, url = url, course.ids = course.ids,
    course.urls = course.urls, course.starts = course.starts,
    course.ends = course.ends, part = part,
    course.names = stats::setNames(learnitdown$courses_names,
      learnitdown$courses), toc = toc, clone = clone, level = level, n = n,
    type = type, institution = institution, acad_year = acad_year, term = term,
    set = set, texts = texts, assign.img = "images/list-assign2.png",
    assign.link = paste(learnitdown$baseurl, "github_assignment", sep = "/"),
    template = "assignment_fr.html", baseurl = learnitdown$baseurl)

challenge <- function(name, url, course.ids = NULL, course.urls = NULL,
  course.starts = NULL, course.ends = NULL, part = NULL, toc = "", clone = TRUE,
  level = 3, n = 1, type = "ind. challenge", institution = !"{institutions[1]}",
  acad_year = !"{acad_year}", term = "Q1",  set = !"{sets[1]}",
  texts = learnitdown::challenge_fr())
  learnitdown::challenge(name = name, url = url, course.ids = course.ids,
    course.urls = course.urls, course.starts = course.starts,
    course.ends = course.ends, part = part,
    course.names = stats::setNames(learnitdown$courses_names,
      learnitdown$courses), toc = toc, clone = clone, level = level, n = n,
    type = type, institution = institution, acad_year = acad_year, term = term,
    set = set, texts = texts, assign.img = "images/list-challenge.png",
    assign.link = paste(learnitdown$baseurl, "github_challenge", sep = "/"),
    template = "assignment_fr.html", baseurl = learnitdown$baseurl)

challenge2 <- function(name, url, course.ids = NULL, course.urls = NULL,
  course.starts = NULL, course.ends = NULL, part = NULL, toc = "", clone = TRUE,
  level = 4, n = 2, type = "group challenge", institution = !"{institutions[1]}",
  acad_year = !"{acad_year}", term = "Q1",  set = !"{sets[1]}",
  texts = learnitdown::challenge2_fr())
  learnitdown::challenge2(name = name, url = url, course.ids = course.ids,
    course.urls = course.urls, course.starts = course.starts,
    course.ends = course.ends, part = part,
    course.names = stats::setNames(learnitdown$courses_names,
      learnitdown$courses), toc = toc, clone = clone, level = level, n = n,
    type = type, institution = institution, acad_year = acad_year, term = term,
    set = set, texts = texts, assign.img = "images/list-challenge2.png",
    assign.link = paste(learnitdown$baseurl, "github_challenge", sep = "/"),
    template = "assignment_fr.html", baseurl = learnitdown$baseurl)

# Note: use `r learnitdown::clean_ex_toc()` at the beginning of index.Rmd to
# make sure the ex dir is clean when the book compiles.
show_ex_toc <- function(id, header = "", clear.it = TRUE, finalize = FALSE)
  learnitdown::show_ex_toc(id = id, header = header, clear.it = clear.it,
    finalize = finalize)

# Include javascript and css code for {learnitdown} additional features
# in style.css and header.html, respectively
learnitdown::learnitdown_init(
  baseurl = learnitdown$baseurl,
  #EN hide.code.msg = "See the code",
  hide.code.msg = "Voir le code",
  institutions = learnitdown$institutions,
  courses = learnitdown$courses)

# Knitr default options
knitr::opts_chunk$set(comment = "#", fig.align = "center")
