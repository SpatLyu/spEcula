# Pre-compiled vignettes that need parallel computation.
# reference: https://ropensci.org/blog/2019/12/08/precompute-vignettes/
# Must manually move image files from `spEcula/` to `spEcula/vignettes/` after knit.

devtools::load_all()

knitr::knit("vignettes/GOS.Rmd.orig",
            "vignettes/GOS.Rmd")

knitr::knit("vignettes/sandwich.Rmd.orig",
            "vignettes/sandwich.Rmd")
