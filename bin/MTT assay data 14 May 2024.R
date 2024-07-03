library(readxl)
library(tidyverse)
library(janitor)


#Always control S to save 
# Read in files -----------------------------------------------------------


#4 hours
raw_data_4hrs <- clean_names(read_excel("Endpoint @ 570 2024-05-14 15-15-04 - Copy4h.xlsx",
                                        #sheet we want to read from
                                        sheet = "Raw data 570 nm",
                                        #cell range we want to read from
                                        range = "A5:D101"),case='none')


#24 hours
raw_data_24hrs <- clean_names(read_excel("Endpoint @ 570 2024-05-15 11-09-53 - Copy24h.xlsx",
                                         #sheet we want to read from
                                         sheet = "Raw data 570 nm",
                                         #cell range we want to read from
                                         range = "A5:D101"),case='none')

#48 hours
raw_data_48hrs <- clean_names(read_excel("Endpoint @ 570 2024-05-16 11-14-54 - Copy48h.xlsx",
                                         #sheet we want to read from
                                         sheet = "Raw data 570 nm",
                                         #cell range we want to read from
                                         range = "A5:D101"), case='none')

#72 hours
raw_data_72hrs <- clean_names(read_excel("Endpoint @ 570 2024-05-17 11-33-39 - Copy72h.xlsx",
                                         #sheet we want to read from
                                         sheet = "Raw data 570 nm",
                                         #cell range we want to read from
                                         range = "A5:D101"),case='none')


#Key table

#Placing in my environment 
new_table <- read_excel("MTT key spreadsheet.xlsx")



# Merge tables ------------------------------------------------------------


# Make sure column names are the same in both columns
merged_table_4hrs <- merge(new_table,raw_data_4hrs, by = "Well")
Blank <- merged_table_4hrs |> filter(Hormone=='Blank') |> summarise(mean=mean(Raw_absorbance))
merged_table_4hrs <- merged_table_4hrs |> mutate(Absorbance=Raw_absorbance-Blank$mean)


merged_table_24hrs <- merge(new_table,raw_data_24hrs, by = "Well")
Blank <- merged_table_24hrs |> filter(Hormone=='Blank') |> summarise(mean=mean(Raw_absorbance))
merged_table_24hrs <- merged_table_24hrs |> mutate(Absorbance=Raw_absorbance-Blank$mean)

merged_table_48hrs <- merge(new_table,raw_data_48hrs, by = "Well")
Blank <- merged_table_48hrs |> filter(Hormone=='Blank') |> summarise(mean=mean(Raw_absorbance))
merged_table_48hrs <- merged_table_48hrs |> mutate(Absorbance=Raw_absorbance-Blank$mean)


merged_table_72hrs <- merge(new_table,raw_data_72hrs, by = "Well")
Blank <- merged_table_72hrs |> filter(Hormone=='Blank') |> summarise(mean=mean(Raw_absorbance))
merged_table_72hrs <- merged_table_72hrs |> mutate(Absorbance=Raw_absorbance-Blank$mean)

rm(Blank,raw_data_4hrs,raw_data_24hrs,raw_data_48hrs,raw_data_72hrs,new_table)

#Calculate mean of DHT and E2 technical replicates and group by flask

DHT_4hrs_0 <- merged_table_4hrs |> 
  filter(Hormone=='Methanol') |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))


DHT_4hrs_1 <- merged_table_4hrs |> 
  filter(Hormone=='DHT' & Concentration==1) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_4hrs_10 <- merged_table_4hrs |> 
  filter(Hormone=='DHT' & Concentration==10) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_4hrs_100 <- merged_table_4hrs |> 
  filter(Hormone=='DHT' & Concentration==100) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_24hrs_0 <- merged_table_24hrs |> 
  filter(Hormone=='Methanol') |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))


DHT_24hrs_1 <- merged_table_24hrs |> 
  filter(Hormone=='DHT' & Concentration==1) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_24hrs_10 <- merged_table_24hrs |> 
  filter(Hormone=='DHT' & Concentration==10) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_24hrs_100 <- merged_table_24hrs |> 
  filter(Hormone=='DHT' & Concentration==100) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_48hrs_0 <- merged_table_48hrs |> 
  filter(Hormone=='Methanol') |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_48hrs_1 <- merged_table_48hrs |> 
  filter(Hormone=='DHT' & Concentration==1) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_48hrs_10 <- merged_table_48hrs |> 
  filter(Hormone=='DHT' & Concentration==10) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_48hrs_100 <- merged_table_48hrs |> 
  filter(Hormone=='DHT' & Concentration==100) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_72hrs_1 <- merged_table_72hrs |> 
  filter(Hormone=='DHT' & Concentration==1) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_72hrs_0 <- merged_table_72hrs |> 
  filter(Hormone=='Methanol') |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_72hrs_10 <- merged_table_72hrs |> 
  filter(Hormone=='DHT' & Concentration==10) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

DHT_72hrs_100 <- merged_table_72hrs |> 
  filter(Hormone=='DHT' & Concentration==100) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

E2_4hrs_0 <- merged_table_4hrs |> 
  filter(Hormone=='Ethanol') |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))


E2_4hrs_1 <- merged_table_4hrs |> 
  filter(Hormone=='E2' & Concentration==1) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))


E2_4hrs_10 <- merged_table_4hrs |> 
  filter(Hormone=='E2' & Concentration==10) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))


E2_4hrs_100 <- merged_table_4hrs |> 
  filter(Hormone=='E2' & Concentration==100) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

E2_24hrs_0 <- merged_table_24hrs |> 
  filter(Hormone=='Ethanol') |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

E2_24hrs_1 <- merged_table_24hrs |> 
  filter(Hormone=='E2' & Concentration==1) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

E2_24hrs_10 <- merged_table_24hrs |> 
  filter(Hormone=='E2' & Concentration==10) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

E2_24hrs_100 <- merged_table_24hrs |> 
  filter(Hormone=='E2' & Concentration==100) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

E2_48hrs_0 <- merged_table_48hrs |> 
  filter(Hormone=='Ethanol') |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))


E2_48hrs_1 <- merged_table_48hrs |> 
  filter(Hormone=='E2' & Concentration==1) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

E2_48hrs_10 <- merged_table_48hrs |> 
  filter(Hormone=='E2' & Concentration==10) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

E2_48hrs_100 <- merged_table_48hrs |> 
  filter(Hormone=='E2' & Concentration==100) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

E2_72hrs_0 <- merged_table_72hrs |> 
  filter(Hormone=='Ethanol') |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))


E2_72hrs_1 <- merged_table_72hrs |> 
  filter(Hormone=='E2' & Concentration==1) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

E2_72hrs_10 <- merged_table_72hrs |> 
  filter(Hormone=='E2' & Concentration==10) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

E2_72hrs_100 <- merged_table_72hrs |> 
  filter(Hormone=='E2' & Concentration==100) |>
  group_by(Flask) |> 
  summarise(mean=mean(Absorbance))

# Viability calculation DHT, treated/untreated control
# In R, Cbind () — column bind function is used for merging two data frames, 
# given that the number of rows in both the data frames are equal. cbind can 
# append vectors, matrices, or any data frame by columns.

cbind(Flask=DHT_4hrs_0$Flask,'0'=DHT_4hrs_0$mean/DHT_4hrs_0$mean)
DHT_4hcontrol <- cbind(Flask=DHT_4hrs_0$Flask,'0'=DHT_4hrs_0$mean/DHT_4hrs_0$mean)

cbind(Flask=DHT_4hrs_0$Flask,'1'=DHT_4hrs_1$mean/DHT_4hrs_0$mean)
cbind(Flask=DHT_4hrs_0$Flask,'10'=DHT_4hrs_10$mean/DHT_4hrs_0$mean)
cbind(Flask=DHT_4hrs_0$Flask,'100'=DHT_4hrs_100$mean/DHT_4hrs_0$mean)

cbind(Flask=DHT_24hrs_0$Flask,'0'=DHT_24hrs_0$mean/DHT_24hrs_0$mean)
DHT_24hcontrol <- cbind(Flask=DHT_24hrs_0$Flask,'0'=DHT_24hrs_0$mean/DHT_24hrs_0$mean)

cbind(Flask=DHT_24hrs_0$Flask,'1'=DHT_24hrs_1$mean/DHT_24hrs_0$mean)
cbind(Flask=DHT_24hrs_0$Flask,'10' =DHT_24hrs_10$mean/DHT_24hrs_0$mean)
cbind(Flask=DHT_24hrs_0$Flask,'100'=DHT_24hrs_100$mean/DHT_24hrs_0$mean)

cbind(Flask=DHT_48hrs_0$Flask,'0'=DHT_48hrs_0$mean/DHT_48hrs_0$mean)
DHT_48hcontrol <- cbind(Flask=DHT_48hrs_0$Flask,'0'=DHT_48hrs_0$mean/DHT_48hrs_0$mean)

cbind(Flask=DHT_48hrs_0$Flask,'1'=DHT_48hrs_1$mean/DHT_48hrs_0$mean)
cbind(Flask=DHT_48hrs_0$Flask,'10'=DHT_48hrs_10$mean/DHT_48hrs_0$mean)
cbind(Flask=DHT_48hrs_0$Flask,'100'=DHT_48hrs_100$mean/DHT_48hrs_0$mean)

cbind(Flask=DHT_4hrs_0$Flask,'0'=DHT_72hrs_0$mean/DHT_72hrs_0$mean)
DHT_72hcontrol <- cbind(Flask=DHT_72hrs_0$Flask,'0'=DHT_72hrs_0$mean/DHT_72hrs_0$mean)

cbind(Flask=DHT_72hrs_0$Flask,'1'=DHT_72hrs_1$mean/DHT_72hrs_0$mean)
cbind(Flask=DHT_72hrs_0$Flask,'10'=DHT_72hrs_10$mean/DHT_72hrs_0$mean)
cbind(Flask=DHT_72hrs_0$Flask,'100'=DHT_72hrs_100$mean/DHT_72hrs_0$mean)

# Assigning the % viability to data tables
DHT_4hrs_1nMtable <- cbind(Flask=DHT_4hrs_0$Flask,'1'=DHT_4hrs_1$mean/DHT_4hrs_0$mean) %>% as.data.frame()
DHT_4hrs_10nMtable <- cbind(Flask=DHT_4hrs_0$Flask,'10'=DHT_4hrs_10$mean/DHT_4hrs_0$mean) %>% as.data.frame()
DHT_4hrs_100nMtable <- cbind(Flask=DHT_4hrs_0$Flask,'100'=DHT_4hrs_100$mean/DHT_4hrs_0$mean) %>% as.data.frame()

DHT_24hrs_1nMtable <- cbind(Flask=DHT_24hrs_0$Flask,'1'=DHT_24hrs_1$mean/DHT_24hrs_0$mean) %>% as.data.frame()
DHT_24hrs_10nMtable <- cbind(Flask=DHT_24hrs_0$Flask,'10'=DHT_24hrs_10$mean/DHT_24hrs_0$mean) %>% as.data.frame()
DHT_24hrs_100nMtable <- cbind(Flask=DHT_24hrs_0$Flask,'100'=DHT_24hrs_100$mean/DHT_24hrs_0$mean) %>% as.data.frame()

DHT_48hrs_1nMtable <- cbind(Flask=DHT_48hrs_0$Flask,'1'=DHT_48hrs_1$mean/DHT_48hrs_0$mean) %>% as.data.frame()
DHT_48hrs_10nMtable <- cbind(Flask=DHT_48hrs_0$Flask,'10'=DHT_48hrs_10$mean/DHT_48hrs_0$mean) %>% as.data.frame()
DHT_48hrs_100nMtable <- cbind(Flask=DHT_48hrs_0$Flask,'100'=DHT_48hrs_100$mean/DHT_48hrs_0$mean) %>% as.data.frame()

DHT_72hrs_1nMtable <- cbind(Flask=DHT_72hrs_0$Flask,'1'=DHT_72hrs_1$mean/DHT_72hrs_0$mean) %>% as.data.frame()
DHT_72hrs_10nMtable <- cbind(Flask=DHT_72hrs_0$Flask,'10'=DHT_72hrs_10$mean/DHT_72hrs_0$mean) %>% as.data.frame()
DHT_72hrs_100nMtable <- cbind(Flask=DHT_72hrs_0$Flask,'100'=DHT_72hrs_100$mean/DHT_72hrs_0$mean) %>% as.data.frame()


#Viability calculation E2, treated/untreated control
cbind(Flask=E2_4hrs_0$Flask,'0'=E2_4hrs_0$mean/E2_4hrs_0$mean)
E2_4hcontrol <- cbind(Flask=E2_4hrs_0$Flask,'0'=E2_4hrs_0$mean/E2_4hrs_0$mean)

cbind(Flask=E2_4hrs_0$Flask,'1'=E2_4hrs_1$mean/E2_4hrs_0$mean)
cbind(Flask=E2_4hrs_0$Flask,'10'=E2_4hrs_10$mean/E2_4hrs_0$mean)
cbind(Flask=E2_4hrs_0$Flask,'100'=E2_4hrs_100$mean/E2_4hrs_0$mean)

cbind(Flask=E2_24hrs_0$Flask,'0'=E2_24hrs_0$mean/E2_24hrs_0$mean)
E2_24hcontrol <- cbind(Flask=E2_24hrs_0$Flask,'0'=E2_24hrs_0$mean/E2_24hrs_0$mean)

cbind(Flask=E2_24hrs_0$Flask,'1'=E2_24hrs_1$mean/E2_24hrs_0$mean)
cbind(Flask=E2_24hrs_0$Flask,'10'=E2_24hrs_10$mean/E2_24hrs_0$mean)
cbind(Flask=E2_24hrs_0$Flask,'100'=E2_24hrs_100$mean/E2_24hrs_0$mean)

cbind(Flask=E2_48hrs_0$Flask,'0'=E2_48hrs_0$mean/E2_48hrs_0$mean)
E2_48hcontrol <- cbind(Flask=E2_48hrs_0$Flask,'0'=E2_48hrs_0$mean/E2_48hrs_0$mean)


cbind(Flask=E2_48hrs_0$Flask,'1'=E2_48hrs_1$mean/E2_48hrs_0$mean)
cbind(Flask=E2_48hrs_0$Flask,'10'=E2_4hrs_10$mean/E2_48hrs_0$mean)
cbind(Flask=E2_48hrs_0$Flask,'100'=E2_4hrs_100$mean/E2_48hrs_0$mean)

cbind(Flask=E2_72hrs_0$Flask,'0'=E2_72hrs_0$mean/E2_4hrs_0$mean)
E2_72hcontrol <- cbind(Flask=E2_72hrs_0$Flask,'0'=E2_4hrs_0$mean/E2_72hrs_0$mean)


cbind(Flask=E2_72hrs_0$Flask,'1'=E2_72hrs_1$mean/E2_72hrs_0$mean)
cbind(Flask=E2_72hrs_0$Flask,'10'=E2_72hrs_10$mean/E2_72hrs_0$mean)
cbind(Flask=E2_72hrs_0$Flask,'100'=E2_72hrs_100$mean/E2_72hrs_0$mean)

# Assigning the % viability to data tables

E2_4hrs_1nMtable <- cbind(Flask=E2_4hrs_0$Flask,'1'=E2_4hrs_1$mean/E2_4hrs_0$mean) %>% as.data.frame()
E2_4hrs_10nMtable <- cbind(Flask=E2_4hrs_0$Flask,'10'=E2_4hrs_10$mean/E2_4hrs_0$mean) %>% as.data.frame()
E2_4hrs_100nMtable <- cbind(Flask=E2_4hrs_0$Flask,'100'=E2_4hrs_100$mean/E2_4hrs_0$mean) %>% as.data.frame()

E2_24hrs_1nMtable <- cbind(Flask=E2_24hrs_0$Flask,'1'=E2_24hrs_1$mean/E2_24hrs_0$mean) %>% as.data.frame()
E2_24hrs_10nMtable <- cbind(Flask=E2_24hrs_0$Flask,'10'=E2_24hrs_10$mean/E2_24hrs_0$mean) %>% as.data.frame()
E2_24hrs_100nMtable <- cbind(Flask=E2_24hrs_0$Flask,'100'=E2_24hrs_100$mean/E2_24hrs_0$mean) %>% as.data.frame()

E2_48hrs_1nMtable <- cbind(Flask=E2_48hrs_0$Flask,'1'=E2_48hrs_1$mean/E2_48hrs_0$mean) %>% as.data.frame()
E2_48hrs_10nMtable <- cbind(Flask=E2_48hrs_0$Flask,'10'=E2_48hrs_10$mean/E2_48hrs_0$mean) %>% as.data.frame()
E2_48hrs_100nMtable <- cbind(Flask=E2_48hrs_0$Flask,'100'=E2_48hrs_100$mean/E2_48hrs_0$mean) %>% as.data.frame()

E2_72hrs_1nMtable <- cbind(Flask=E2_72hrs_0$Flask,'1'=E2_72hrs_1$mean/E2_72hrs_0$mean) %>% as.data.frame()
E2_72hrs_10nMtable <- cbind(Flask=E2_72hrs_0$Flask,'10'=E2_72hrs_10$mean/E2_72hrs_0$mean) %>% as.data.frame()
E2_72hrs_100nMtable <- cbind(Flask=E2_72hrs_0$Flask,'100'=E2_72hrs_100$mean/E2_72hrs_0$mean) %>% as.data.frame()


library(data.table)

# Combining the average viability of all concentrations for each timepoint
combined_tableE4 <- cbind(E2_4hcontrol, 
                        E2_4hrs_1nMtable %>%
                          select(`1`), 
                        E2_4hrs_10nMtable %>%
                          select(`10`), 
                        E2_4hrs_100nMtable %>%
                          select(`100`)) 

combined_tableE24 <- cbind(E2_24hcontrol, 
                        E2_24hrs_1nMtable %>%
                          select(`1`), 
                        E2_24hrs_10nMtable %>%
                          select(`10`), 
                        E2_24hrs_100nMtable %>%
                          select(`100`)) 

combined_tableE48 <- cbind(E2_48hcontrol, 
                        E2_48hrs_1nMtable %>%
                          select(`1`), 
                        E2_48hrs_10nMtable %>%
                          select(`10`), 
                        E2_48hrs_100nMtable %>%
                          select(`100`)) 

combined_tableE72 <- cbind(E2_72hcontrol, 
                        E2_72hrs_1nMtable %>%
                          select(`1`), 
                        E2_72hrs_10nMtable %>%
                          select(`10`), 
                        E2_72hrs_100nMtable %>%
                          select(`100`)) 

combined_tableDHT4 <- cbind(DHT_4hcontrol, 
                        DHT_4hrs_1nMtable %>%
                          select(`1`), 
                        DHT_4hrs_10nMtable %>%
                          select(`10`), 
                        DHT_4hrs_100nMtable %>%
                          select(`100`)) 

combined_tableDHT24 <- cbind(DHT_24hcontrol, 
                        DHT_24hrs_1nMtable %>%
                          select(`1`), 
                        DHT_24hrs_10nMtable %>%
                          select(`10`), 
                        DHT_24hrs_100nMtable %>%
                          select(`100`)) 

combined_tableDHT48 <- cbind(DHT_48hcontrol, 
                        DHT_48hrs_1nMtable %>%
                          select(`1`), 
                        DHT_48hrs_10nMtable %>%
                          select(`10`), 
                        DHT_48hrs_100nMtable %>%
                          select(`100`)) 

combined_tableDHT72 <- cbind(DHT_72hcontrol, 
                        DHT_72hrs_1nMtable %>%
                          select(`1`), 
                        DHT_72hrs_10nMtable %>%
                          select(`10`), 
                        DHT_72hrs_100nMtable %>%
                          select(`100`)) 


# #pivot_longer() "lengthens" data, increasing the number of rows and decreasing the number of columns. 
# The inverse transformation is pivot_wider()
# 2:5 telling it which columns you're interested in pivoting

combined4hrsDHT_pivot <- pivot_longer(combined_tableDHT4, 2:5, names_to = "Concentration", values_to = "Viability")
ggplot(combined4hrsDHT_pivot, aes(x = Concentration, y = Viability, col = Concentration)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  geom_line()

combined24hrsDHT_pivot <- pivot_longer(combined_tableDHT24, 2:5, names_to = "Concentration", values_to = "Viability")
ggplot(combined24hrsDHT_pivot, aes(x = Concentration, y = Viability, col = Concentration)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  geom_line()

combined48hrsDHT_pivot <- pivot_longer(combined_tableDHT48, 2:5, names_to = "Concentration", values_to = "Viability")
ggplot(combined48hrsDHT_pivot, aes(x = Concentration, y = Viability, col = Concentration)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  geom_line()

combined72hrsDHT_pivot <- pivot_longer(combined_tableDHT72, 2:5, names_to = "Concentration", values_to = "Viability")
ggplot(combined72hrsDHT_pivot, aes(x = Concentration, y = Viability, col = Concentration)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  geom_line()

combined72hrsDHT_pivot <- pivot_longer(combined_tableDHT72, 2:5, names_to = "Concentration", values_to = "Viability")
ggplot(combined72hrsDHT_pivot, aes(x = Concentration, y = Viability, col = Concentration)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  geom_line()


#This is just a tester

ggplot(combined72hrsDHT_pivot, aes(x = Group, y = mean_value)) +
  geom_bar(stat = "identity",) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.2) +
  labs(title = "Bar Graph with Error Bars", x = "Concentration", y = "Viability") +
  theme_minimal()

#Script carries on from here


combined4hrsE2_pivot <- pivot_longer(combined_tableE4, 2:5, names_to = "Concentration", values_to = "Viability")
ggplot(combined4hrsE2_pivot, aes(x = Concentration, y = Viability, col = Concentration)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  geom_line()


combined24hrsE2_pivot <- pivot_longer(combined_tableE24, 2:5, names_to = "Concentration", values_to = "Viability")
ggplot(combined24hrsE2_pivot, aes(x = Concentration, y = Viability, col = Concentration)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  geom_line()

combined48hrsE2_pivot <- pivot_longer(combined_tableE48, 2:5, names_to = "Concentration", values_to = "Viability")
ggplot(combined48hrsE2_pivot, aes(x = Concentration, y = Viability, col = Concentration)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  geom_line()

combined72hrsE2_pivot <- pivot_longer(combined_tableE72, 2:5, names_to = "Concentration", values_to = "Viability")
ggplot(combined72hrsE2_pivot, aes(x = Concentration, y = Viability, col = Concentration)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  geom_line()

#Check this because the viability of 0nM should be 100% and not more!!

combined4hrsDHT_pivot <- pivot_longer(combined_tableDHT4, 2:5, names_to = "Concentration", values_to = "Viability")
ggplot(combined4hrsDHT_pivot, aes(x = Concentration, y = Viability, col = Concentration)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  geom_line()

cp <- combined4hrsDHT_pivot

library(ggplot2)
library(plyr)
library(dplyr)

# Install the Rmisc package if you haven't already
install.packages("Rmisc")

# Load the Rmisc package
library(Rmisc)

# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval 
cps <- summarySE(cp, measurevar="Viability", groupvars=c("Concentration"))

ggplot(cps, aes(x=Concentration, y=Viability)) + 
  geom_errorbar(aes(ymin=Viability-se, ymax=Viability+se), width=.1) +
  geom_line() +
  geom_point()

# Error bars represent standard error of the mean
ggplot(cps, aes(x=Concentration, y=Viability)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Viability-se, ymax=Viability+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

geom_boxplot() +
  theme_bw() +
  geom_point() +
  geom_line()

# Merge all timepoints and corresponding concenntrations for DHT 
# merge(x, y, by, all): Merges two data frames x and y by a common column specified by 'by'
# The all = TRUE argument ensures that all rows are included (full outer join).

# Using reduce to merge multiple data frames
library(dplyr)
library(purrr)




# summarySE provides the standard deviation, standard error of the mean, 
# and a (default 95%) confidence interval

# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
DHT4summary <- summarySE(combined4hrsDHT_pivot, measurevar="Viability", groupvars=c("Concentration"))

# mutate - adding a column 
combined4hrsDHT_pivot_timepoint <- combined4hrsDHT_pivot |> mutate(Timepoint=4)
combined24hrsDHT_pivot_timepoint <- combined24hrsDHT_pivot |> mutate(Timepoint=24)
combined48hrsDHT_pivot_timepoint <- combined48hrsDHT_pivot |> mutate(Timepoint=48)
combined72hrsDHT_pivot_timepoint <- combined72hrsDHT_pivot |> mutate(Timepoint=72)

# rbind - merging by rows
rbind(combined4hrsDHT_pivot,combined24hrsDHT_pivot, combined48hrsDHT_pivot,combined72hrsDHT_pivot)
merged_DHT <- rbind(combined4hrsDHT_pivot_timepoint,combined24hrsDHT_pivot_timepoint, combined48hrsDHT_pivot_timepoint,combined72hrsDHT_pivot_timepoint)

# summary
merged_DHTsummary <- summarySE(merged_DHT, measurevar="Viability", groupvars=c("Timepoint","Concentration"))

pd <- position_dodge(3) # move them .05 to the left and right

merged_DHTsummary |> 
  ggplot(aes(x = Timepoint, y = Viability, colour = Concentration)) +
  geom_errorbar(aes(ymin=Viability-sd, ymax=Viability+sd), width=.1, position=pd) +
  # geom_point(position=pd) +
  geom_line(position=pd) +
  theme_bw() 

