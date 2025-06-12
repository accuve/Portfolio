# Loading required libraries
library(tidyverse)
library(readr)
library(dplyr)
library(visdat)
library(ggradar)
library(treemap)

#Loading datasets diamonds for intial preview
data("diamonds")

#Saving Original dataset
diamond_original<-write.csv(diamonds,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_original.csv")

#reading saved datatsets
diamond_original<-read_csv("/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_original.csv")

#Viewing & Analysing Dataset
View(diamond_original) #View the diamond dataset
glimpse(diamond_original) #Summary of dataset
str(diamond_original) #data class
sapply(diamond_original,class) #another way for data class

#Data Analysis
## Correct data structure used
###cut, color and clarity are character
###carat, depth, table, price,x,y,z are number

## Wrong Name for First Column created during reading csv file and Ambiguous name for depth, x,y,z

#Correcting preliminary findings
colnames(diamond_original)[1]<-"sn"
colnames(diamond_original)[6]<-"depth_percent"
colnames(diamond_original)[9]<-"length"
colnames(diamond_original)[10]<-"width"
colnames(diamond_original)[11]<-"depth"

#Finding missing information using visdat package
vis_miss(diamond_original)

#Removing SN Column created during reading csv file for easy duplicate finding as each SN is a unique number
diamond_original<-diamond_original %>% 
  select(-sn)

#finding and Viewing duplicate rows
duplicate_rows<-diamond_original[duplicated(diamond_original), ]
View(duplicate_rows)

#removing duplicate rows
diamond_complete<-diamond_original %>% 
  distinct()
diamond_cleaned<-diamond_complete #renaming dataset

#Saving Altered dataset
diamond_cleaned<-write.csv(diamond_complete,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_cleaned.csv")

#reading cleaned dataset after saving
diamond_cleaned<-read_csv("/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_cleaned.csv")
View(diamond_cleaned)

#renaming column 1 to "sn" and deleting "sn" column as reading csv file creates first column as "sn"
colnames(diamond_cleaned)[1]<-"sn"
diamond_cleaned<-diamond_cleaned %>% 
  select(-sn)

#identifying if any column has field value 0
lowvalue_carat<-subset(diamond_cleaned,subset =!(carat>0.001))
lowvalue_depth_percent<-subset(diamond_cleaned,subset =!(depth_percent>0.001))
lowvalue_table<-subset(diamond_cleaned,subset =!(table>0.001))
lowvalue_price<-subset(diamond_cleaned,subset =!(price>0.001))
lowvalue_length<-subset(diamond_cleaned,subset =!(length>0.001))
lowvalue_width<-subset(diamond_cleaned,subset =!(width>0.001))
lowvalue_depth<-subset(diamond_cleaned,subset =!(depth>0.001))

#removing rows with any column value=0, ideal was to correct the value but in given scenario we donot have correct value
##bsed on above identification, length, width and depth has field value 0
diamond_cleaned<-diamond_cleaned[diamond_cleaned$length>0.001, ]
diamond_cleaned<-diamond_cleaned[diamond_cleaned$width>0.001, ]
diamond_cleaned<-diamond_cleaned[diamond_cleaned$depth>0.001, ]

#summary
summary(diamond_cleaned)

#saving final cleaned datasets
diamond_cleaned<-write.csv(diamond_cleaned,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_cleaned.csv")

#reading cleaned dataset after saving
diamond_cleaned<-read_csv("/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_cleaned.csv")
View(diamond_cleaned)

#renaming column 1 to "sn" and deleting "sn" column as reading csv file creates first column as "sn"
colnames(diamond_cleaned)[1]<-"sn"
diamond_cleaned<-diamond_cleaned %>% 
  select(-sn)

##Now this data is ready for data analysis

#Price Comparison based on Color
#mean_color_wide
mean_color_wide<-diamond_cleaned %>% 
  group_by(color) %>% 
  summarise(
    mean_price=round(mean(price),2),
    mean_table=round(mean(table),2),
    mean_length=round(mean(length),2),
    mean_width=round(mean(width),2),
    mean_depth=round(mean(depth),2),
    mean_depth_percent=round(mean(depth_percent),2),
    mean_volume=round(mean(mean_length*mean_width*mean_depth),2), #for simplification, best is to calculate volume for per diamond
    mean_price_per_volume=round(mean(mean_price/mean_volume,2))
  )

View(mean_color_wide)

#mean_cut_wide
mean_cut_wide<-diamond_cleaned %>% 
  group_by(cut) %>% 
  summarise(
    mean_price=round(mean(price),2),
    mean_table=round(mean(table),2),
    mean_length=round(mean(length),2),
    mean_width=round(mean(width),2),
    mean_depth=round(mean(depth),2),
    mean_depth_percent=round(mean(depth_percent),2),
    mean_volume=round(mean(mean_length*mean_width*mean_depth),2), #for simplification, best is to calculate volume for per diamond
    mean_price_per_volume=round(mean(mean_price/mean_volume),2)
  )
View(mean_cut_wide)

#mean_clarity_wide
mean_clarity_wide<-diamond_cleaned %>% 
  group_by(clarity) %>% 
  summarise(
    mean_price=round(mean(price),2),
    mean_table=round(mean(table),2),
    mean_length=round(mean(length),2),
    mean_width=round(mean(width),2),
    mean_depth=round(mean(depth),2),
    mean_depth_percent=round(mean(depth_percent),2),
    mean_volume=round(mean(mean_length*mean_width*mean_depth),2), #for simplification, best is to calculate volume for per diamond
    mean_price_per_volume=round(mean(mean_price/mean_volume),2)
  )
View(mean_clarity_wide)

#mean_carat_wide
mean_carat_wide<-diamond_cleaned %>%
  mutate(carat_clarity=paste(carat,clarity,sep="_")) %>% #joing carat and clarity and creating new column
  group_by(carat,clarity,carat_clarity) %>% 
  summarise(
    mean_price=round(mean(price),2),
    mean_table=round(mean(table),2),
    mean_length=round(mean(length),2),
    mean_width=round(mean(width),2),
    mean_depth=round(mean(depth),2),
    mean_depth_percent=round(mean(depth_percent),2),
    mean_volume=round(mean(mean_length*mean_width*mean_depth),2), #for simplification, best is to calculate volume for per diamond
    mean_price_per_volume=round(mean(mean_price/mean_volume),2),
    .groups='drop' #suppressing grouping message
  )
View(mean_carat_wide)

#ggplot2 price comparison
##based on cut
ggplot(mean_cut_wide,aes(x=cut,y=mean_price,fill=cut))+
  geom_col()+
  geom_text(aes(x=cut,y=mean_price,label=mean_price),
            vjust=-0.5)+
  labs(
    title="Mean Price vs Diamond Cut",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Cut",
    y="Mean Price(USD)"
  )

##based on Color
ggplot(mean_color_wide,aes(x=color,y=mean_price,fill=color))+
  geom_col()+
  geom_text(aes(x=color,y=mean_price,label=mean_price),
            vjust=-0.5)+
  labs(
    title="Mean Price vs Diamond Color",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Color",
    y="Mean Price(USD)"
  )

##based on Clarity, ignoring size scale
ggplot(mean_clarity_wide,aes(x=clarity,y=mean_price,fill=clarity))+
  geom_col()+
  geom_text(aes(x=clarity,y=mean_price,label=mean_price),
            vjust=-0.5)+
  labs(
    title="Mean Price vs Diamond Clarity",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Clarity",
    y="Mean Price(USD)"
  )

#Analysis on based on Clarity
##based on Clarity, ignoring size scale
ggplot(mean_clarity_wide,aes(x=clarity,y=mean_price,fill=clarity))+
  geom_col()+
  geom_text(aes(x=clarity,y=mean_price,label=mean_price),
            vjust=-0.5)+
  labs(
    title="Mean Price vs Diamond Clarity,size of Diamond Ingored",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Clarity",
    y="Mean Price(USD)"
  )

##based on Clarity, Volume of the Diamond is considered in comparison
ggplot(mean_clarity_wide,aes(x=clarity,y=mean_price,fill=mean_volume))+
  geom_col()+
  geom_text(aes(x=clarity,y=mean_price,label=mean_price),
            vjust=-0.5)+
  scale_size_continuous(name="Mean_Volume")+
  labs(
    title="Mean Price vs Diamond Clarity, Colored: Volume",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Clarity",
    y="Mean Price(USD)"
  )
##based on Clarity, Volume of the Diamond is considered as fill,scatter plot used
ggplot(mean_clarity_wide,aes(x=clarity,y=mean_price,fill=mean_volume))+
  geom_point(size=5)+
  labs(
    title="Mean Price vs Diamond Clarity Scatter Plot",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Clarity",
    y="Mean Price(USD)"
  )

##based on Clarity, Price/Volume of the Diamond is considered in comparison
ggplot(mean_clarity_wide,aes(x=clarity,y=mean_price_per_volume,fill=clarity))+
  geom_col()+
  geom_text(aes(x=clarity,y=mean_price_per_volume,label=mean_price_per_volume),
            vjust=-0.5)+
  labs(
    title="Mean Price per Volume vs Diamond Clarity",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Clarity",
    y="Mean Price per Volume(USD/mm^3)"
  )
###The result matched more with the Diamond Clarity Concept, better the clarity more is the price of diamond

##based on Clarity sorted ascending order of clarity, Price/Volume of the Diamond is considered in comparison
###sorting based on clarity
mean_clarity_wide$clarity<-factor(mean_clarity_wide$clarity,levels = c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1","IF"))
mean_clarity_wide_sorted<-mean_clarity_wide %>%
  arrange(clarity)
View(mean_clarity_wide_sorted)
###plotting in bar
ggplot(mean_clarity_wide_sorted,aes(x=clarity,y=mean_price_per_volume,fill=clarity))+
  geom_col()+
  geom_text(aes(x=clarity,y=mean_price_per_volume,label=mean_price_per_volume),
            vjust=-0.5)+
  labs(
    title="Mean Price per Volume vs Diamond Clarity",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Clarity",
    y="Mean Price per Volume(USD/mm^3)"
  )
###plotting in line chart
ggplot(mean_clarity_wide_sorted,aes(x=clarity,y=mean_price_per_volume,group=1,label=round(mean_price_per_volume)))+
  geom_line()+
  geom_point(size=5)+
  geom_text(vjust=-1.5)+
  labs(
    title="Mean Price per Volume vs Diamond Clarity",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Clarity",
    y="Mean Price per Volume(USD/mm^3)"
  )

###the line chart looks somewhat OK but deviation for VVS1 is improper, price of VVS1>VVS2 but it is not so in the case, reason:effect of carat, cut and color.

#Analysis for color, no ranking required
##plotting in bar
ggplot(mean_color_wide,aes(x=color,y=mean_price_per_volume,fill=color))+
  geom_col()+
  geom_text(aes(x=color,y=mean_price_per_volume,label=mean_price_per_volume),
            vjust=-0.5)+
  labs(
    title="Mean Price per Volume vs Diamond Color",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Color",
    y="Mean Price per Volume(USD/mm^3)"
  )
##plotting in line chart
ggplot(mean_color_wide,aes(x=color,y=mean_price_per_volume,group=1,label=mean_price_per_volume))+
  geom_line()+
  geom_point(size=5)+
  geom_text(vjust=-2.1)+
  labs(
    title="Mean Price per Volume vs Diamond Color",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Color",
    y="Mean Price per Volume(USD/mm^3)"
  )
##This did not result as expected, Price of D should have been higher than price of J, reason affect of carat and clarity plays more role than color.

#Similarly, Analysis based on carat
##Relation of Carat with Volume
ggplot(mean_carat_wide,aes(x=carat,y=mean_volume,group=1))+
  geom_line()+
  geom_point(size=0.5)+
  labs(
    title="Diamond Carat vs Mean Volume",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Carat",
    y="Mean Volume"
  )
##Few irregularities in volume to carat identified this are to be corrected with correct value, since we donot have correct value, it is not corrected
##Relation of Carat with Price using scatter plot with facet wrap
ggplot(mean_carat_wide,aes(x=carat,y=mean_price,color=clarity))+
  geom_point()+
  facet_wrap(~clarity)+
  labs(
    title="Mean Price Vs Carat (facet wrap and Coloured-Clarity)",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Carat",
    y="Mean Price (USD)"
  )
##Relation of Carat with Price using scatter plot
ggplot(mean_carat_wide,aes(x=carat,y=mean_price,color=clarity))+
  geom_point()+
  labs(
    title="Mean Price vs Carat (Colored-Clarity)",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Carat",
    y="Mean Price (USD)"
  )
##Relation of Carat with Price using scatter plot and adding regression line
ggplot(mean_carat_wide,aes(x=carat,y=mean_price,color=clarity))+
  geom_point()+
  geom_smooth(method = "lm")+ #add a linear regression line
  labs(
    title="Mean Price vs Carat (Colored-Clarity) with regression line",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Carat",
    y="Mean Price (USD)"
  )
##Relation of Carat with Price using scatter plot using log scale
  ggplot(mean_carat_wide,aes(x=carat,y=mean_price,color=clarity))+
    geom_point()+
    scale_y_log10()+ #use log scale for mean price
  labs(
    title="Mean Price(Log scale) Vs Carat",
    subtitle="CA. Anup Acharya",
    caption="Diamond Dataset",
    x="Diamond Carat",
    y="Mean Price (USD)"
  )
  
#Distribution of Carat Size
  ggplot(diamond_cleaned,aes(x=carat))+
    geom_histogram(bins=40,color="black",fill="steelblue")+
    labs(
      title="Histogram of Carat",
      subtitle="CA. Anup Acharya",
      caption="Diamond Dataset",
      x="Carat Size",
      y="No. of Diamonds"
    )
 #Scatter Plot of Carat with Price- Correlation Analysis
  ggplot(diamond_cleaned,aes(x=carat,y=price,color=cut))+
    geom_point()+
    labs(
      title="Scatter Plot of Carat and Price (Colour- Cut",
      subtitle="CA. Anup Acharya",
      caption="Diamond Dataset",
      x="Carat Size",
      y="No. of Diamonds"
    )
  #Scatter Plot of Carat with Price- Correlation Analysis
  ggplot(diamond_cleaned,aes(x=carat,y=price,color=color))+
    geom_point()+
    labs(
      title="Scatter Plot of Carat and Price (Colour- Color of Diamond",
      subtitle="CA. Anup Acharya",
      caption="Diamond Dataset",
      x="Carat Size",
      y="No. of Diamonds"
    )
  #Qualitative Analysis
  ##boxplot to visualize the distribution of price per clarity rating
  ###Sorting based on clarity
  diamond_cleaned$clarity<-factor(diamond_cleaned$clarity,levels = c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1","IF"))
  diamond_cleaned_clarity_sorted<-diamond_cleaned %>%
    arrange(clarity)
  View(diamond_cleaned_clarity_sorted)
  ###plotting
  ggplot(diamond_cleaned_clarity_sorted,aes(x=clarity,y=price,fill=clarity))+
    geom_boxplot()+
    labs(
      title="Clarity Box Plot",
      subtitle="CA. Anup Acharya",
      caption="Diamond Dataset",
      x="Clarity Rating",
      y="Price (USD)"
    )
  
  ##boxplot to visualize the distribution of price per cut type
  ###Sorting based on cut
  diamond_cleaned$cut<-factor(diamond_cleaned$cut,levels = c("Fair","Good","Very Good","Premium","Ideal"))
  diamond_cleaned_cut_sorted<-diamond_cleaned %>%
    arrange(cut)
  View(diamond_cleaned_cut_sorted)
  ###plotting
  ggplot(diamond_cleaned_cut_sorted,aes(x=cut,y=price,fill=cut))+
    geom_boxplot()+
    labs(
      title="Diamond Cut Box Plot",
      subtitle="CA. Anup Acharya",
      caption="Diamond Dataset",
      x="Cut Type",
      y="Price (USD)"
    )
  
  ##boxplot to visualize the distribution of price per color type
  ###Sorting based on diamond color
  diamond_cleaned$color<-factor(diamond_cleaned$color,levels = c("D","E","F","G","H","I","J"))
  diamond_cleaned_color_sorted<-diamond_cleaned %>%
    arrange(color)
  View(diamond_cleaned_color_sorted)
  ###plotting
  ggplot(diamond_cleaned_color_sorted,aes(x=color,y=price,fill=color))+
    geom_boxplot()+
    labs(
      title="Diamond Color Box Plot",
      subtitle="CA. Anup Acharya",
      caption="Diamond Dataset",
      x="Diamond Color",
      y="Price (USD)"
    )
  
#Number Visualization- Treemap based on distribution of Color,cut and clarity
##Create Aggregate Dataset
  diamond_aggregate<-diamond_cleaned %>% 
    group_by(cut,color,clarity) %>% 
    summarise(count=n())
  .groups='drop'
  View(diamond_aggregate)
  
  ##ploting treemap
  treemap(diamond_aggregate,
             index=c("cut","color","clarity"),
             vSize="count",
             title="No. of Diamond by Cut, Color and Clarity"
             )
  #Heatmap with specific clarity and color rating
  ggplot(diamond_aggregate,aes(x=clarity,y=color,fill=count))+
    geom_tile(color="grey")+
    scale_fill_gradient(low="white",high="red")+
    labs(
      title="Heat Map- No of Diamonds by cut and color",
      subtitle="CA. Anup Acharya",
      caption="Diamond Dataset",
      x="clarity",
      y="Color"
    )
  
#Savind all dataframes
#Saving Original dataset
diamond_original<-write.csv(diamonds,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_original.csv")
diamond_cleaned<-write.csv(diamond_cleaned,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_cleaned.csv")
diamond_aggregate<-write.csv(diamond_aggregate,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_aggregate.csv")
diamond_cleaned_clarity_sorted<-write.csv(diamond_cleaned_clarity_sorted,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_cleaned_clarity_sorted.csv")
diamond_cleaned_cut_sorted<-write.csv(diamond_cleaned_cut_sorted,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_cleaned_cut_sorted.csv")
diamond_cleaned_color_sorted<-write.csv(diamond_cleaned_color_sorted,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_cleaned_color_sorted.csv")
diamond_complete<-write.csv(diamond_complete,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/diamond_complete.csv")
duplicate_rows<-write.csv(duplicate_rows,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/duplicate_rows.csv")
lowvalue_depth<-write.csv(lowvalue_depth,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/lowvalue_depth.csv")
lowvalue_length<-write.csv(lowvalue_length,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/lowvalue_length.csv")
lowvalue_width<-write.csv(lowvalue_width,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/lowvalue_width.csv")
mean_carat_wide<-write.csv(mean_carat_wide,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/mean_carat_wide.csv")
mean_cut_wide<-write.csv(mean_cut_wide,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/mean_cut_wide.csv")
mean_color_wide<-write.csv(mean_color_wide,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/mean_color_wide.csv")
mean_clarity_wide<-write.csv(mean_clarity_wide,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/mean_clarity_wide.csv")
mean_clarity_wide_sorted<-write.csv(mean_clarity_wide_sorted,file="/Users/creation/Library/CloudStorage/GoogleDrive-longlivenepal48@gmail.com/My Drive/1. Anup Acharya/Learning/R Training/04. Diamond Project/mean_clarity_wide_sorted.csv")
