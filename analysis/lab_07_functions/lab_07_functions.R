# Lab 06 - Distributions, Simulation, Basic Functions

# Objectives:
# 1) simulate data:
# - 50 students in the class
# - 3 exams
# - There is 5% chance that a student will fail to show up to any given exam (independent)
# - Exam 1 has the mean of 80, Exam 2 has the mean of 70, Exam 3 has the mean of 60
# - Each exam has a standard deviation of 10

# 2) compose a function that puts each exam on the scale from 0 to 1

# 3) compose a function that graphs the distribution of a single exam with ggplot2
# --- clean-environment ------
rm( list = ls(all = TRUE)) # to clean the environment
cat("\014") # to clean the console

# ---- load-packages -------
library(magrittr) # pipes
library(dplyr)    # wrangling
library(ggplot2)  # graphing
library(tidyr)    # pivoting

# ---- basic-distributions ------
rnorm(n = 10)
rnorm(n=1000) %>% hist()

runif(n = 10)
runif(n = 1000) %>% hist()
rnorm(n = 1000, mean = 100, sd = 15) %>% hist()

exam1 <- rnorm(n = 100, mean = 80, sd = 10)
exam2 <- rnorm(n = 100, mean = 70, sd = 10)
exam3 <- rnorm(n = 100, mean = 60, sd = 10)
exam1 %>% hist(main = "exam1")
exam2 %>% hist(main = "exam2")
exam3 %>% hist(main = "exam3")

# viewing simulated distribution
a <- tibble(normdist = rnorm(n=1000, mean =100, sd=15))
ggplot(a, aes(x =normdist)) + geom_histogram()

b <- tibble(uniformdist = runif(100) < .1)
ggplot(b, aes(x = uniformdist))+geom_bar()

# ---- simulate-scenario -----------

class_size = 50
ds <- tibble::tibble(
  student_id = 1:class_size
  ,"exam1" = rnorm(n = class_size, mean = 80, sd = 10)
  ,"exam2" = rnorm(n = class_size, mean = 70, sd = 10)
  ,"exam3" = rnorm(n = class_size, mean = 60, sd = 10)
) %>%
  dplyr::mutate(
    exam1_missed = runif(class_size) < .05
    ,exam2_missed = runif(class_size) < .05
    ,exam3_missed = runif(class_size) < .05
    ,exam1 = ifelse(exam1_missed,NA,exam1)
    ,exam2 = ifelse(exam2_missed,NA,exam2)
    ,exam3 = ifelse(exam3_missed,NA,exam3)
  ) %>%
  dplyr::select(-exam1_missed, -exam2_missed, -exam3_missed)
ds %>% print()
# ds$exam1_missed %>% sum()
ds$exam1 %>% is.na() %>% sum()
ds$exam2 %>% is.na() %>% sum()
ds$exam3 %>% is.na() %>% sum()
# quickly visualize all three exams to compare them
ds %>%
  tidyr::pivot_longer(
    cols       = c("exam1","exam2","exam3")
    ,names_to  = "exam"
    ,values_to = "score"
  ) %>%
  ggplot(aes(x = score))+
  geom_histogram()+
  facet_grid(exam~.)

# ---- compose-rescale-function -----------------
# create a function that rescales from 0 to 1
set.seed(42)

# study example from the book
df <- tibble::tibble(
  a = rnorm(10)
)
df$a <- (df$a - min(df$a, na.rm = TRUE)) /
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df
df$a %>% hist()
# df$a_rescaled %>% hist()

# what does it do?
(min_a <- min(df$a, na.rm = TRUE))
(max_a <- max(df$a, na.rm = TRUE))
(range_a <- (max_a - min_a))
(df$a - min_a) / (range_a)
# a more efficient way
rng <- range(df$a); str(rng)
(df$a - min_a) / (rng[2] - rng[1])


rescale01 <- function(x) {
  # x   <- df$a
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  # min_x <- rng[1]
  # max_x <- rng[2]
  (x - rng[1]) / (rng[2] - rng[1]) # order of operations!
  # ( x - min_x / max_x - min_x)
  # return(x)
}
rescale01(c(-10, 0, 10, NA, Inf))
a <- c(-10, 0, 2, 5, NA, -Inf)
rescale01(a)

# function application
ds <- ds %>%
  mutate(
    exam1_rescaled = rescale01(exam1)
  )

# ---- compose-graphing-function -----------------
# create a function that prints a histogram of a given column

make_histogram <- function(d, variable_name) {
  # d <- ds
  # browser()
  d$exam1 %>% mean(na.rm = TRUE)
  number_missing = d[[variable_name]] %>% is.na() %>% sum()
  exam_mean = d[[variable_name]]%>% mean(na.rm = TRUE)

  g <- d %>%
    ggplot(aes_string(x = variable_name))+
    geom_histogram(bins = 20)+
    labs(
      title = paste0(
        "Distribution for ", toupper(variable_name),"\n",
        "N missing = ", number_missing,"\n",
        "Mean = ", exam_mean %>% round(2)
      )
    )
  # gvariable_name
  return(g)
}
# How to use
# make_histogram(d = ds, variable_name = "exam1_rescaled")
ds %>% make_histogram("exam1")

ds %>% ggplot(aes(x = exam1))+geom_histogram(bins = 20)
ds %>% ggplot(aes(x = exam1_rescaled))+geom_histogram(bins = 20)



