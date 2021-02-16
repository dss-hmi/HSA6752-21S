path_file <- "data-public/raw/Tableau 10 Training Practice Data.xlsx"
sheet_names <- readxl::excel_sheets(path_file)
dto <- list()
for(sheet_i in sheet_names){
  # i <- sheet_names[1]
  dto[[sheet_i]] <- readxl::read_xlsx(path_file,sheet = sheet_i)
}
ds <- dto$`13 - Cancer Deaths by State` %>% tibble::as_tibble()

ds


# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",
#                    "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
#
# library("ggplot2")
# theme_set(theme_bw())
# library("sf")
# library("rnaturalearth")
# library("rnaturalearthdata")
# world <- ne_countries(scale = "medium", returnclass = "sf")
# class(world)

ds_states <- map_data("state")
ds_states %>% head()

StatePopulation <- read.csv("https://raw.githubusercontent.com/ds4stats/r-tutorials/master/intro-maps/data/StatePopulation.csv", as.is = TRUE)

MergedStates <- dplyr::inner_join(
  ds_states
  ,ds %>% dplyr::mutate(State = tolower(State))
  ,by = c("region" = "State")
) %>% tibble::as_tibble()

p <- ggplot()
p <- p + geom_polygon( data=MergedStates,
                       aes(x=long, y=lat, group=group, fill = `Crude Death Rate`),
                       color="white", size = 0.2)
p
