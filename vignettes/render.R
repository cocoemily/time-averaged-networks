##Rscript for knitting RMarkdown files on an HPC

#run this line to render the Prignano sensitivity analysis document
#rmarkdown::render("vignettes/Sensitivity_Analysis_Prignano.Rmd")

#run this line to render the ICRATES sensitivity analysis document
#rmarkdown::render("vignettes/Sensitivity_Analysis_ICRATES.Rmd")

#run this line to render the subsample of the ICRATES sensitivity analysis document
rmarkdown::render("vignettes/Sensitivity_Analysis_ICRATES-sample.Rmd")

#run this line to render the Chaco sensitivity analysis document
#rmarkdown::render("vignettes/Sensitivity_Analysis.Rmd")
