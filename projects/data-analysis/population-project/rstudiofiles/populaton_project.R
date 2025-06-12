#Required Library Packages
library(tidyverse)
library(readr)
library(dplyr)
library(visdat)
library(tidyr)
library(visdat)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(readr)

#Dataset
data("population")

#dataset tag
population_original_long<-population

#Long data to wide data
population_original_wide<-pivot_wider((population_original_long), names_from=year, values_from=population)

#View dataset
View(population_original_long)
View(population_original_wide)


#Readability
##Column_rename
names(population_original_long)[3]<-"total_population"

#Assessment of Data Quality
##Structure View
str(population_original_long)
summary(population_original_long)

##Identify Missing Information
vis_miss(population_original_long)
vis_miss(population_original_wide)

##list missing data
###missing on wide form data
missing_population_wide_info<-population_original_wide %>% 
  filter(if_any(everything(),is.na))
View(missing_population_wide_info)

###missing on long form data

missing_population_long_info<-population_original_long %>% 
  filter(if_any(everything(),is.na))
View(missing_population_long_info)

###No missing data

###Note:
#### Different reading for missing data in long form (no missing data) and wide form (missing data)
#####Reason: Different locations has different start year and end year

#### Not Deleting the rows
##### Entirely New location may be developed.
##### Old locaiton may be separated to form two or more New locations
##### Location might have been renamed and shown as two rows

##adding new column to indicate incomplete data for easy filter
###wide form data
population_original_wide_info<-population_original_wide %>% 
  mutate(missing_value=if_any(everything(), is.na))
View(population_original_wide_info)
###long form data
population_original_long_info<-population_original_long %>% 
  mutate(missing_value=if_any(everything(), is.na))
View(population_original_long_info)

##finding duplicate
####wide form data
duplicated_location_wide<-population_original_wide_info[duplicated(population_original_wide_info), ]
View(duplicated_location_wide)

####no duplicated information

####long form data
duplicated_location_long<-population_original_long_info[duplicated(population_original_long_info), ]
View(duplicated_location_long)

####no duplicated information

##Completeness of data
###count no of rows in year variable greater than 0 and less than 100
population_original_wide_info_summary<-population_original_wide_info %>%
  summarise(across('1995':'2013',~sum(.x>0&.x<100,na.rm=TRUE)))
View(population_original_wide_info_summary)

####no rows identified with population range from 0 to 100

###count no of rows in year variable greater than 0 and less than 100
population_original_long_info_summary <- population_original_long_info %>%
  group_by(year) %>%
  summarise(count_population_long_between_0_and_100 = sum(total_population > 0 & total_population < 100))
View(population_original_long_info_summary)

####no rows identified with population range from 0 to 100

#Final Data Rename
##wide form
population_cleaned_wide<-population_original_wide_info
View(population_cleaned_wide)

##long form
population_cleaned_long<-population_original_long_info
View(population_cleaned_long)


#Data Summary
##Average Total Population of all location year-wise to trace population growth pattern

###wide data
mean_population_per_year_all_location_wide<-population_cleaned_wide %>% 
  summarise(across(`1995`:`2013`,~mean(.x,na.rm=TRUE))) %>% #ignores empty or na
  mutate(Particulars = "mean_population_yearwise_converted") %>% #Add 'Particulars' column 
  select(Particulars, everything()) #Reorder columns to put 'Particulars' first

View(mean_population_per_year_all_location_wide)
####Gives mean of all country population year-wise

###Conversion of mean_population_per_location_wide to long form
mean_population_per_year_all_location_long_i<-pivot_longer(mean_population_per_year_all_location_wide, cols=-Particulars, names_to="year", values_to="mean_population_yearwise_converted")
View(mean_population_per_year_all_location_long_i)

###long data
mean_population_per_year_all_location_long_ii<-population_cleaned_long %>% 
  group_by(year) %>% 
  summarise(
    mean_population_yearwise=mean(total_population)
  )
View(mean_population_per_year_all_location_long_ii)

###compare between mean_population_per_year_all_location_long_i & mean_population_per_year_all_location_long_ii

mean_population_per_year_all_location_long_ii<-mean_population_per_year_all_location_long_ii %>% 
  mutate(year=as.character(year))

combined_population <- inner_join(mean_population_per_year_all_location_long_i,mean_population_per_year_all_location_long_ii, by = "year") %>% 
  mutate(difference=mean_population_yearwise_converted-mean_population_yearwise)

View(combined_population)

####Reason of Comparison: to ensure the average population location wise using both way is same

###long data
mean_population_per_location_all_year_long<-population_cleaned_long %>% 
  group_by(country) %>% 
  summarise(
    mean_population_countrywise=mean(total_population)
    )
View(mean_population_per_location_all_year_long)

##Line-Chart of Mean Population for all location year-wise
ggplot(mean_population_per_year_all_location_long_ii,aes(x=year,y=mean_population_yearwise/1000000,group = 1))+
  geom_line(color="blue")+
  geom_point(color="red")+
  labs(
    title = "Mean Population Yearwise (1995-2013)",
    x="Year",
    y="Average Total Population (Millions)"
  )

##Choropleth Map of Mean Population of all location for 13 years
world <- ne_countries(scale = "medium", returnclass = "sf")
world_data <- left_join(world, mean_population_per_location_all_year_long, by = c("name" = "country"))
ggplot(world_data) +
  geom_sf(aes(fill = mean_population_countrywise/1e6)) +
  scale_fill_viridis_c(name = "Avg Population (Millions)", na.value = "grey90") +
  labs(title = "Average Population by Country (1995-2012)") +
  theme_minimal()

##Growth Analysis
###Calculation of Growth rate
mean_population_per_year_all_location_long_ii_sorted<- mean_population_per_year_all_location_long_ii %>%
  arrange(year) #sort year-wise

mean_population_per_year_all_location_long_ii_sorted<-mean_population_per_year_all_location_long_ii_sorted %>% 
  mutate(
    previous_year_population=lag(mean_population_yearwise),
    growth_rate=((mean_population_yearwise- previous_year_population)/previous_year_population)*100
    )

View(mean_population_per_year_all_location_long_ii_sorted)

###Line-Chart of Growth Rate
mean_population_per_year_all_location_long_ii_sorted_clean<-mean_population_per_year_all_location_long_ii_sorted %>%
  filter(!is.na(growth_rate))

ggplot(mean_population_per_year_all_location_long_ii_sorted_clean,aes(x=year,y=growth_rate,group = 1))+
          geom_line(color="blue")+
          geom_point(color="green")+
          labs(
            title="Year-wise Growth Rate",
            x="Year",
            y="Percentage %"
          )

##All Location Population Trend in same graph
###Plotting population trend for all locations
ggplot(population_cleaned_long,aes(x=year,y=total_population/1000000,group=country))+
  geom_line()+
  geom_point()+
  labs(
    title="Population trend for all location",
    x="Year",
    y="Population in Millions"
  )+
  theme_minimal()

# Save all viewed data frames to CSV
write_csv(population_original_long, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/population_original_long.csv")
write_csv(population_original_wide, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/population_original_wide.csv")
write_csv(missing_population_wide_info, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/missing_population_wide_info.csv")
write_csv(missing_population_long_info, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/missing_population_long_info.csv")
write_csv(population_original_wide_info, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/population_original_wide_info.csv")
write_csv(population_original_long_info, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/population_original_long_info.csv")
write_csv(duplicated_location_wide, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/duplicated_location_wide.csv")
write_csv(duplicated_location_long, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/duplicated_location_long.csv")
write_csv(population_original_wide_info_summary, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/population_original_wide_info_summary.csv")
write_csv(population_original_long_info_summary, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/population_original_long_info_summary.csv")
write_csv(population_cleaned_wide, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/population_cleaned_wide.csv")
write_csv(population_cleaned_long, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/population_cleaned_long.csv")
write_csv(mean_population_per_year_all_location_wide, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/mean_population_per_year_all_location_wide.csv")
write_csv(mean_population_per_year_all_location_long_i, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/mean_population_per_year_all_location_long_i.csv")
write_csv(mean_population_per_year_all_location_long_ii, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/mean_population_per_year_all_location_long_ii.csv")
write_csv(combined_population, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/combined_population.csv")
write_csv(mean_population_per_location_all_year_long, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/mean_population_per_location_all_year_long.csv")
write_csv(mean_population_per_year_all_location_long_ii_sorted, "/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/05. Population Project/population-project/extracted-data-subsets/mean_population_per_year_all_location_long_ii_sorted.csv")