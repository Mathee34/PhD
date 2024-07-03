library(readxl)
library(tidyverse)
library(janitor)

#Read in file 

CFU_infection_data <- clean_names(read_excel("CFU raw data mock infection with hormone treatment April 2024 R spreadsheet.xlsx",
                                             #sheet we want to read from
                                             sheet = "Hormone vs untreated counts Jun")
                                  #cell range we want to read from
                                  ,case='none')


  
CFU_infection_data_cfu_ml <- CFU_infection_data |> 
  mutate(cfu_ml = (CFU_infection_data$Colonies * CFU_infection_data$Dilution)/(5*10^-2)) %>%
  mutate(Timepoint = as.factor(CFU_infection_data$Timepoint))

CFU_infection_data_cfu_ml$Timepoint <- factor(CFU_infection_data_cfu_ml$Timepoint,
                                             levels = c("4", "24", "48", "72"))
CFU_infection_data_cfu_ml |> 
  filter(Hormone == "DHT") |> 
  ggplot(aes(x = Timepoint, y = cfu_ml)) +
  geom_point() +
  facet_wrap(~Concentration) +
  ggtitle("DHT")

CFU_infection_data_cfu_ml |> 
  filter(Hormone == "E2") |> 
  ggplot(aes(x = Timepoint, y = cfu_ml)) +
  geom_point() +
  facet_wrap(~Concentration) +
  ggtitle("E2")
E2 <- subset(CFU_infection_data_cfu_ml, CFU_infection_data_cfu_ml$Hormone=="E2")
E2 |> 
  ggplot(aes(x = Timepoint, y = cfu_ml, color=Concentration))+
  geom_point()

CFU_infection_data_cfu_ml |> 
  mutate(Concentration = as.factor(Concentration)) |> 
  ggplot(aes(x = Timepoint, y = cfu_ml, colour = Concentration, shape = Hormone)) +
  geom_point()

CFU_infection_data_cfu_ml |> 
  mutate(Concentration = as.factor(Concentration)) |> 
  ggplot(aes(x = Timepoint, y = cfu_ml, colour = Concentration, shape = Hormone)) +
  geom_bar()

