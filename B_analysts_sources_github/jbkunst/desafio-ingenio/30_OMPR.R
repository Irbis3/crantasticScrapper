# packages ----------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(stringr)
library(ompr)

# https://dirkschumacher.github.io/ompr/articles/problem-course-assignment.html
# devtools::install_github("dirkschumacher/ompr")
# devtools::install_github("dirkschumacher/ompr.roi")
# install.packages("ROI.plugin.glpk")

# data --------------------------------------------------------------------
load("data/initial_data.RData")

carrrs$id <- seq(1, nrow(carrrs))


# model -------------------------------------------------------------------
# students
n <- length(unique(scores$alumno))

# course
m <- nrow(carrrs)
capacity <- carrrs$cupos # rep.int(11, m) # all have equal capacities

# preferences <- memoise::memoise(function(student) sample(seq_len(m), 3))
# preferences(1)

preferences2 <- function(student = 1) {
  
  prefrc %>% 
    filter(alumno == student) %>% 
    arrange(preferencia) %>% 
    # head(3) %>% 
    left_join(carrrs, by = "carrera") %>% 
    .$id
  
}


# if the course is not among the preferences, the weight is -100000
weight <- function(student = 1, course = 1) {
  p <- which(as.numeric(course) == preferences2(as.numeric(student)))
  as.integer(if (length(p) == 0) {
    -100000
  } else {
    1 - p
  })
}

preferences2(student = 1)
weight(student = 1, course = 7)
weight(student = 1, course = 3)
weight(student = 1, course = 10)
weight(student = 1, course = 11)


model <- MIPModel() %>%
  
  # 1 iff student i is assigned to course m
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  
  # maximize the preferences
  set_objective(sum_expr(weight(i, j) * x[i, j], i = 1:n, j = 1:m)) %>%
  
  # we cannot exceed the capacity of a course
  add_constraint(sum_expr(x[i, j], i = 1:n) <= capacity[j], j = 1:m) %>% 
  
  # each student needs to be assigned to one course
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n)

model


library(ompr.roi)
library(ROI.plugin.glpk)

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

matching <- result %>% 
  get_solution(x[i,j]) %>%
  tbl_df() %>% 
  filter(value > .9) %>%  
  select(i, j) %>% 
  rowwise() %>% 
  mutate(weight = weight(as.numeric(i), as.numeric(j)), 
         preferences = paste0(preferences2(as.numeric(i)), collapse = ",")) %>%
  ungroup() %>% 
  arrange(i)

matching
preferences2(1)

tail(matching)
preferences2(100)

count(matching, weight)

assigment_mip <- matching %>% 
  mutate(alumno = as.character(i),
         id = j) %>% 
  select(alumno, id) %>% 
  left_join(carrrs, by = "id") %>% 
  select(alumno, assigment = carrera)

assigment_mip2 <- assigment_mip %>% 
  left_join(matching %>% mutate(alumno = as.character(i)))

tail(assigment_mip2)

assigment_mip2
assigment_mip2 %>% filter(weight < 0)

assigment_vald(assigment_mip)
assigment_cnts(assigment_mip)

assigment_temp(assigment_mip)
assigment_temp(assigment_rndm())


# export ------------------------------------------------------------------
carrs <- c("Salud",
"Educacion",
"Filosofia",
"Ingenieria",
"Ciencias",
"Derecho",
"Publicidad",
"Matematica",
"Psicologia",
"Politica")

carrs <- tolower(carrs)

assig_mip_output <- assigment_mip %>% 
  mutate(value = 1,
         alumno = paste0("Alumno", alumno),
         alumno = factor(alumno, levels = alumno),
         assigment = factor(assigment, levels = carrs)) %>% 
  rename(Emparejamiento = assigment) %>% 
  spread(alumno, value)

glimpse(assig_mip_output)



