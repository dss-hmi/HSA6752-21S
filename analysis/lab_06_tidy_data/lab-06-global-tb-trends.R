rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting 
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default unless overwritten

# ---- load-packages -----------------------------------------------------------
library(magrittr)  # pipes
library(dplyr)     # data wrangling
library(ggplot2)   # graphs
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(wpp2019)

# ---- load-sources ------------------------------------------------------------

source("./scripts/common-functions.R")


# ---- declare-globals ---------------------------------------------------------
# custom function for HTML tables
neat <- function(x, output_format = "html"){ 
  # knitr.table.format = output_format
  if(output_format == "pandoc"){
    x_t <- knitr::kable(x, format = "pandoc")
  }else{
    x_t <- x %>%
      # x %>%
      # neat() %>%
      knitr::kable(format=output_format) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed","responsive"),
        # bootstrap_options = c( "condensed"),
        full_width = F,
        position = "left"
      )
  } 
  return(x_t)
}
# Note: when printing to Word or PDF use `neat(output_format =  "pandoc")`


prints_folder <- paste0("./analysis/lab_06_tidy_data/prints/")
if(!file.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}

ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey95", color = NA)
    )
)
quick_save <- function(g,name,...){
  ggplot2::ggsave(
    filename = paste0(name,".jpg"),
    plot     = g,
    device   = "jpg",
    path     = prints_folder,
    # width    = width,
    # height   = height,
    # units = "cm",
    dpi      = 'retina',
    limitsize = FALSE,
    ...
  )
}

print_all <- function(d){print(d, n = nrow(d))}

path_geo <- "https://raw.githubusercontent.com/andkov/covid19-country-response/master/data-public/metadata/world-geography.csv"

# ---- load-data ---------------------------------------------------------------
data("pop") #to load object from wpp2019
ds_geo <- read.csv(path_geo) %>% tibble::as_tibble()
ds_who <- who %>% tibble::as_tibble()
ds_popF <- popF %>% tibble::as_tibble() %>% select(1:3,starts_with("20"))
ds_popM <- popM %>% tibble::as_tibble() %>% select(1:3,starts_with("20"))


# ---- tweak-data --------------------------------------------------------------

ds_who_long <- ds_who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65
    , names_to       = "key"
    , values_to      = "case"
    , values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>% 
  separate(
    key, c("new", "type", "sexage"), sep = "_"
  ) %>% 
  separate(
    sexage, c("sex", "age"), sep = 1
  ) %>% 
  select(-new)

# To make data more simple and match population data
# 1. cobmine all types of diabetes
# 2. keep only years 2000, 2005, 2010, 2015, 2020

ds_who_long %>% distinct(type) # to see all possible values

ds_tb <- ds_who_long %>% 
  group_by(country, iso2, iso3, year, sex, age) %>% 
  summarize(
    case_count = sum(case, na.rm = T)
  ) %>% 
  ungroup() %>% 
  filter(
    year %in% seq(2000,2020,5)
  ) %>% 
  mutate(
    sex = case_when( sex == "f" ~ "Female",sex =="m" ~ "Male")
  )


# to combine population data for F and M in a single data set
ds_popFM <- dplyr::bind_rows(
  ds_popF %>% mutate(sex = "Female")
  ,ds_popM %>% mutate(sex = "Male")
) %>% 
  select(country_code, name, age, sex, everything())

ds_popFM_long <- ds_popFM %>% 
  pivot_longer(
    cols = starts_with("20")
    ,names_to = "year"
    ,values_to = "population"
  ) %>% 
  mutate(
    year = as.integer(year) # to make compatible for joining with ds_tb
  )

# to study the structure on a small subset
# ds_popFM_long %>% filter(name == "Albania") %>% View()

# collapse the values of the age to match the ones in ds_tb
ds_who_long %>% distinct(age) %>% print_all()
ds_popFM_long %>% distinct(age) %>% print_all()

ds_pop_long <- ds_popFM_long %>% 
  mutate(
    age = forcats::fct_collapse(
      age
      ,"014"  = c("0-4","5-9","10-14")
      ,"1524" = c("15-19","20-24")
      ,"2534" = c("25-29","30-34")
      ,"3544" = c("35-39","40-44")
      ,"4554" = c("45-49","50-54")
      ,"5564" = c("55-59","60-64")
      ,"65"   = c("65-69","70-74","75-79","80-84","85-89","90-94","95-99","100+")
      ) %>% 
      as.character() # to make compatible for joining to ds_tb
  )
ds_pop_long %>% distinct(age)
# We collapsed the age categories, but population numbers are still for original categories of age
# ds_pop_long %>% filter(name == "Albania") %>% arrange(age, year) %>% View()

# to add up the population values, keeping only unique combinations of non-numeric variables
ds_pop <- ds_pop_long %>% 
  # filter(name == "Albania") %>%  # to test on a smaller set
  group_by(country_code, name, age, sex, year) %>%  # one value for each unique combination of these
  summarize( # to reduce the number of rows
    population = sum(population, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    
  )
# to study the results on a small subset
# ds_pop %>% filter(name == "Albania") %>% arrange(age, year, sex) %>% View()

# ---- join-tables -------

# notice the types of the variables on which you will join
# the type must match in the tables you are joining on this variable
ds_tb  # TB cases
ds_geo # helper table for geography
ds_pop # Population

variables_selected <- c(
  "continent_code", "continent_name"
  , "country_number","country_name", "country_label"
  ,"iso2", "iso3"
  ,"year", "sex", "age", "case_count"
  )
ds_tb_geo_pop <- 
  ds_tb %>%  # left-hand side table
  left_join(
    ds_geo # right-hand side table
   ,by = c("iso2" = "country_code2", "iso3" = "country_code")
  ) %>% 
  left_join(
    ds_pop # right-hand side table
    ,c("country_number" = "country_code", "sex", "age","year")
  ) %>% 
  select(
    continent = continent_name
    ,country = country_label
    ,iso2
    ,iso3
    ,year
    ,sex
    ,age
    ,tb_case_count = case_count
  )

ds0 <- ds_tb_geo_pop

# to clear up the environment from the dataset we no longer need
rm(
  ds_geo   
  ,ds_pop   
  ,ds_pop_long        
  ,ds_popF      
  ,ds_popFM      
  ,ds_popFM_long           
  ,ds_popM    
  ,ds_tb               
  ,ds_tb_geo_pop            
  ,ds_who        
  ,ds_who_long            
  ,pop      
  ,popF  
  ,popFT     
  ,popM               
  ,popMT   
  )

# ---- table-1 -----------------------------------------------------------------
ds0 %>% glimpse()

# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------


# ----- publish ----------------------------------------------------------------
path <- "./analysis/lab_06_tidy_data/lab-06-global-tb-trends.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
