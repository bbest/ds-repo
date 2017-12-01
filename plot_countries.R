# libraries ----
library(tidyverse)

# data ----
url <- 'https://raw.githubusercontent.com/OHI-Science/data-science-training/master/data/gapminder.csv'
gapminder <- read_csv(url) # View(gapminder)

# function: ----
country_plot <- function(cntry, folder = "."){ # cntry <- "Canada"
  
  #browser()
  png <- paste0("gdp_", cntry, ".png")
  cat("country_plot(", cntry, ") -> ", png, "\n")
  
  g <- gapminder %>%
    filter(country == cntry) %>%
    ggplot(aes(x = year, y = gdpPercap)) +
    geom_point() +
    geom_smooth() +
    labs(title = cntry)
  
  ggsave(file.path(folder, png), g)
}
#country_plot("Mexico")
country_plot("Mexico", "developing")

#countries <- c("United States", "Peru", "New Zealand")
#countries <- unique(gapminder$country)

dir.create("developed")
dir.create("developing")

is_developed <- function(cntry, threshold=12000){ # cntry <- "Peru"
  gapminder %>%
    filter(country == cntry) %>%
    summarise(
      mean_gdp = mean(gdpPercap)) %>%
    .$mean_gdp >= threshold
}
is_developed("Peru", threshold = 2000)
is_developed("United States")

for (k in countries){
  cat(k, "\n")
  
  if (is_developed(k)){
    country_plot(k, "developed")  
    
  } else {
    country_plot(k, "developing")  
  }
  
}


