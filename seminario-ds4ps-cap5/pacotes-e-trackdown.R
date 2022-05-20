# Title: Base Project Packages
# Authors:  Alexsandra. Georgia e Mayres
# Date:  2022-05-20
# Description: Install all packages for the project and use collaborative package

# rm(list = ls())

# packages ---------------------------------------------------------------------
install.packages("rmarkdown")
install.packages("xaringan")
install.packages("xaringanthemer", dependencies = TRUE)
install.packages("pagedown")
install.packages("DT")
install.packages("here")
install.packages("trackdown")

#editar colaborativamente ---------
 
trackdown::upload_file(here("apresentacao", "apresentacao.Rmd")) #so a primeira vez

trackdown::download_file(here("apresentacao", "apresentacao.Rmd")) #baixar arquivo depois de editar no drive.

trackdown::update_file(here("apresentacao", "apresentacao.Rmd")) #atualizar arquivo do drive com edições do .Rmd