# Lab 07 - Distributions, Simulation, and Basic Functions

# Objectives:
# 1) simulate data:
# - 50 students in the class
# - 3 exams
# - There is 5% chance that a student will fail to show up to any given exam (independent)
# - Exam 1 has the mean of 80, Exam 2 has the mean of 70, Exam 3 has the mean of 60
# - Each exam has a standard deviation of 10

# 2) compose a function that puts each exam on the scale from 0 to 1

# 3) compose a function that graphs the distribution of a single exam with ggplot2

rm( list = ls(all = TRUE)) # to clean the environment
cat("\014") # to clean the console

# ---- load-packages ---------
library(magrittr) # pipes
library(dplyr)    # wrangling
library(ggplot2)  # graphing
library(tidyr)    # pivoting

# ---- basic-distributions ------

# Normal distribution
rnorm(n = 1000, mean = 100, sd = 15) %>% hist()

# Uniform distribution
runif(n = 1000) %>% hist()

b <- tibble::tibble(uniformdist =  runif(n = 5000) < .2 )
b %>% ggplot(aes(x = uniformdist))+ geom_bar()

# ---- simulate-scenario --------
class_size <- 1000
ds <- tibble::tibble(
  student_id = 1:class_size
  ,"exam1" = rnorm( n = class_size, mean = 80, sd = 10 )
  ,"exam2" = rnorm( n = class_size, mean = 70, sd = 10 )
  ,"exam3" = rnorm( n = class_size, mean = 60, sd = 10 )
  ,"exam1_missed" = runif(n = class_size ) < .05
  ,"exam2_missed" = runif(n = class_size ) < .05
  ,"exam3_missed" = runif(n = class_size ) < .05
) %>%
  dplyr::mutate(
    exam1  = ifelse(exam1_missed, NA, exam1)
    ,exam2 = ifelse(exam2_missed, NA, exam2)
    ,exam3 = ifelse(exam3_missed, NA, exam3)
  ) %>%
  dplyr::select(-exam1_missed, -exam2_missed, -exam3_missed)
# ds
ds$exam1 %>% is.na() %>% sum()
ds$exam2 %>% is.na() %>% sum()
ds$exam3 %>% is.na() %>% sum()

ds %>% ggplot(aes(x = exam1))+geom_histogram(bins =20)
ds %>% ggplot(aes(x = exam2))+geom_histogram(bins =20)
ds %>% ggplot(aes(x = exam3))+geom_histogram(bins =20)

ds %>%
  tidyr::pivot_longer(
    cols = c("exam1", "exam2", "exam3")
    ,names_to = "exam"
    ,values_to = "score"
  ) %>%
  ggplot( aes(x = score, fill = exam)) +
  geom_histogram(alpha = .3)+
  facet_grid(exam ~ .)

# ---- compose-rescale-function -----
set.seed(42)
df <- tibble::tibble(
  a = rnorm(10)
)
df$a_rescale <- (df$a - min(df$a, na.rm = TRUE)) /
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df

(min_a <- min(df$a, na.rm = TRUE))
(max_a <- max(df$a, na.rm = TRUE))
(df$a - min_a) / (max_a - min_a )
# more effient computation
rng <- range(df$a)
(df$a - rng[1]) / (rng[2] - rng[1] )


rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1] )
}
a <- c(-10, 0, 10, NA, Inf, -Inf)
rescale01(a)

ds <- ds %>%
  dplyr::mutate(
    exam1_rescaled  = rescale01(exam1)
    ,exam2_rescaled = rescale01(exam2)
    ,exam3_rescaled = rescale01(exam3)
  )
ds

# ---- composing-graphing-function ------

ds %>%
  ggplot(aes(x = exam1))+
  geom_histogram(bins = 20)

make_histogram <- function(d, variable_name) {
  # d <- ds; variable_name = "exam1" # for testing
  exam_mean = d[[variable_name]] %>% mean(na.rm = TRUE)
  number_missing = d[[variable_name]] %>% is.na() %>% sum()
  g <- d %>%
    ggplot(aes_string(x = variable_name))+
    geom_histogram(bins = 20, alpha = .4, fill = "salmon")+
    theme_bw()+
    labs(
      title = paste0(
        "Distribution of scores on ", toupper(variable_name),"\n",
        "Mean = ", exam_mean %>% round(1), "\n",
        "N missing = ",number_missing
      )
    )

  return(g)
}
# How to use:
ds %>% make_histogram(variable_name = "exam1")
ds %>% make_histogram(variable_name = "exam3")
ds %>% make_histogram(variable_name = "exam3_rescaled")


# Standard     = code as a reference to a value  (e.g. b)
# Non-Standard = code as a literal        value  (e.g. "exam")










































































