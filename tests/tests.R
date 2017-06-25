library(testthat)
setwd("C:/Users/Usuario/Documents/Kevin Cursos/Kevin Rstudio/Especializacion/Curso 3/week2/tarea/data")
#1 fars_read()
expect_that(fars_read("accident_2014.csv.bz2"), is_a("data.frame"))

#2 make_filename()
expect_that(make_filename(2014), is_a("character"))

#3 fars_read_years()
expect_that(fars_read_years(2014), is_a("list"))

#4 fars_summarize_years()
expect_that(fars_summarize_years(2014), is_a("data.frame"))

#5 fars_map_state(1,2014)
expect_that(fars_map_state(1,2014), is_a("NULL"))
