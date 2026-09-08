# Purl-spin SDD I

# - Extract important R code from the R Markdown files using purl in an R script
# - Knit it to HTML using purl
# - Place these files in /more subdirectory of the website

files_noext <- c("06-Probabilites-Correlation", "07-Distributions-Chi2",
  "08-Moyenne", "09-Variance-I", "10-Variance-II")
output_dir <- "../wp.sciviews.org/htdocs/sdd-umons-2025/more"
fs::dir_create(output_dir)
fs::dir_create("R")

for (f in files_noext) {
  origrmdfile <- paste0(f, ".Rmd")
  fout <- fs::path(output_dir, f)
  rfile <- fs::path("R", paste0(f, ".R")) # In R subdir
  rmdfile <- fs::path("R", paste0(f, ".Rmd"))
  htmlfile <-  fs::path("R", paste0(f, ".html"))
  htmlfilefinal <- paste0(fout, ".html")

  # Make sure old files are removed
  unlink(rfile)
  unlink(rmdfile)
  unlink(htmlfile)
  unlink(htmlfilefinal)

  # Extract R code using purl
  knitr::purl(origrmdfile, output = rfile, documentation = 0L)

  # Eliminate multiple empty lines in this R script
  rlines <- readLines(rfile)
  rlines_clean <- rlines[!grepl("^\\s*$", rlines) |
      c(TRUE, diff(grepl("^\\s*$", rlines)) != 0)]
  writeLines(rlines_clean, rfile)

  # Knit with spin to HTML
  knitr::spin(rfile, knit = FALSE, format = "Rmd")
  rmarkdown::render(rmdfile, output_format = "html_document")
  fs::file_move(htmlfile, htmlfilefinal)
  unlink(rmdfile)
}
