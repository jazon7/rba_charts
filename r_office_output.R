#install required packages and load libraries 
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(ggthemes)) install.packages('ggthemes')
library(ggthemes)

if (!require(officer)) install.packages('officer')
library(officer)

plot1 <- ggplot(data = iris ) +
  geom_point(mapping = aes(Sepal.Length, Petal.Length))

plot1


pp <- read_pptx()
pp <- add_slide(pp, layout = "Title and Content", master = "Office Theme")
pp <- on_slide(pp, index = 1)
pp <- ph_with(x = pp, "THIS IS AWESOME",
              location = ph_location_type(type="title"))


pp <- ph_with(pp, value = plot1, location = ph_location_right())

print(pp, target = "./rbacharts.pptx" )
