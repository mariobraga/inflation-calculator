library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(labeling)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(scales)
library(viridis)
library(ggpmisc)
library(zoo)



install.packages("ggrepel")

library(ggrepel)
library(xts)

library(tibble)
library(nlme)

#Step 1 - Cleaning the Data

#File 1 - IPCA 12 month rate by month from 1995 to 2019

historicalSeries <- read.csv("1IPCA12m95-19.csv")
historicalSeries <- historicalSeries[, -1]

historicalSeries$IPCA12M = as.numeric(historicalSeries$IPCA12M)

historicalSeries <-
  historicalSeries %>% mutate(Date = as.Date(Month,
                                             format = "%d/%m/%Y"))


#File 2 - IPCA annual rate by Metropolitan Area from 2012 to 2018

metropolitanAreas2 <- read.csv("2IPCA12mRM12-18.csv")
metropolitanAreas2 <- metropolitanAreas2[, -1]

#File 2 - IPCA monthly rate by Metropolitan Area from 2012 to 2018




#File 3.1 - IPCA Monthly Group Weight by Metropolitan Area from 2012 to 2018

groupWeightMetroArea <-
  read.csv("3.1GroupMonthlyWeightByMetropolitanArea12-19.csv")
groupWeightMetroArea <- groupWeightMetroArea[, -1]
groupWeightMetroArea <- groupWeightMetroArea[-767, ]
groupWeightMetroArea <- groupWeightMetroArea[-766, ]
groupWeightMetroArea <- groupWeightMetroArea %>%
  mutate(Date = as.Date(Month, format = "%d/%m/%Y"))


#File 3.2 - IPCA Monthly Group Weight by Metropolitan Area from 2012 to 2018

groupVariationMetroArea <-
  read.csv("3.2GroupMonthlyVariationByMetropolitanArea12-19.csv")
groupVariationMetroArea <- groupVariationMetroArea[, -1]
groupVariationMetroArea <- groupVariationMetroArea %>%
  mutate(Date = as.Date(Month, format = "%d, %m, %Y"))


#File 4.1 - Items annual variation December 2018

Items2018Variation <- read.csv("4ItemsAnnualVariationDec2018.csv")
Items2018Variation <- Items2018Variation[-55, -1]
Items2018Variation <- Items2018Variation[-2,-2]
Items2018Variation <- Items2018Variation[-1, ]


#File 4.2 - Items annual weight January 2018

Items2018Weight <- read.csv("4ItemsAnnualWeightJan2018.csv")
Items2018Weight <- Items2018Weight[-55, -1]
Items2018Weight <- Items2018Weight[-2,-2]
Items2018Weight <- Items2018Weight[-1, ]


#File 5 - IPCA 12 month rate by month and Metro area from 2012 to 2018

AnnualPerMonthMetroArea <- read.csv("5IPCA12mMonthlyRM12-18.csv")
AnnualPerMonthMetroArea <- AnnualPerMonthMetroArea[, -1]
AnnualPerMonthMetroArea <-
  AnnualPerMonthMetroArea %>% mutate(Date = as.Date(Month,
                                                    format = "%d/%m/%Y"))


#Step 2 - Plots

# Plot 1
#YEARMON

IPCA <- read.csv("1IPCA12m95-19.csv")
IPCA <- IPCA[, -3]
IPCA <- IPCA[, -1]
IPCA$Date <- as.yearmon(paste(IPCA$Month, IPCA$Year), "%m %Y")

IPCA  %>%
  ggplot() +
  geom_line(aes(x = Date,
                y = IPCA12M),
            size = 1.25) +
  labs(
    title = "Brazilian IPCA inflation from 1995 to 2018",
    y = "variation in %",
    x = ""
  ) +
  theme_minimal(base_size = 12)


#Plot 2 - Target System

Target <- read.csv("IPCATargetSystem.csv")
Target <- Target[, -3]
Target <- Target[, -1]
Target$Date <-
  as.yearmon(paste(Target$Month, Target$Year), "%m %Y")

Target %>%
  ggplot() +
  geom_line(aes(x = Date,
                y = IPCA12M),
            size = 1.25,
            color = I("black")) +
  
  geom_line(aes(
    x = Date,
    y = Upper.Limit,
    color = I("red")
  )) +
  
  geom_line(aes(
    x = Date,
    y = Bottom.Limit,
    color = I("red")
  )) +
  
  geom_line(linetype = "dashed") +
  (aes(
    x = Date,
    y = Target,
    color = I("#46A729")
  )) +
  
  labs(
    title = "Brazilian Inflation Target System",
    subtitle = "actual rate, central target and bottom and upper limits",
    y = "variation in %",
    x = ""
  ) +
  theme_minimal(base_size = 12)


# Plot 3 - Faceting | Working |
# Inflation by Metropolitan Area from 2012 to 2018

metropolitanAreas <- read.csv("2IPCA12mRM12-18.csv")
metropolitanAreas <- metropolitanAreas[, -1]

metropolitanAreas %>%
  ggplot() +
  geom_line(aes(x = Year,
                y = IPCA)) +
  facet_wrap( ~ MetropolitanArea) +
  labs(
    title = "Inflation in Metropolitan Areas",
    subtitle = "IPCA annual rate from 2012 to 2018",
    y = "variation in %",
    x = ""
  ) +
  theme_minimal()




# Plot 4 - ScatterPlot 2018 |

Group2018 <- read.csv("Group2018.csv")
Group2018 <- Group2018[, -1]
Group2018$Contribution <-
Group2018$Variation * Group2018$Weight / 100

Group2018 %>%
  ggplot(aes(x = Contribution,
             y = Variation,
             size = Weight,
             fill = Category))  +
  scale_fill_manual(values = c(
      "#ffff00",
      "#000066",
      "#ff0000",
      "#00cc00",
      "#999966",
      "#ff33cc",
      "#0099ff",
      "#990099",
      "#ff9900")) +
  geom_point(shape = 21, colour = "transparent") +
  labs(title = "Impact each group of goods had on the 3.75% inflation of 2018",
    y = "variation in %",
    x = "contribution in p.p.") +
  geom_hline(yintercept = 0) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none") +
  annotate("text", x = .98, y = 3.6, label = "Food and") +
  annotate("text", x = .98, y = 3.3, label = "Beverages") +
  annotate("text", x = .76, y = 3.8, label = "Transportation") +
  annotate("text", x = .74, y = 5.1, label = "Housing") +
  annotate("text", x = .48, y = 4.3, label = "Healthcare") +
  annotate("text", x = .34, y = 2.7, label = "Personal Expenses") +
  annotate("text", x = .26, y = 5.1, label = "Education") +
  annotate("text", x = .14, y = 4.1, label = "Household Goods") +
  annotate("text", x = .04, y = 0.9, label = "Clothing") +
  annotate("text", x = .06, y = -0.25,label = "Communication")


#Plot 5 - Bar Chart

Group2018$Percentage <-
  Group2018$Contribution * 100 / 3.75

Group2018 %>%
  ggplot() +
  geom_bar(
    aes(x = reorder(Category,-Percentage),
        y = Percentage),
    stat = "identity",
    fill = c(
      "#000066",
      "#00cc00",
      "#ff9900",
      "#0099ff",
      "#999966",
      "#990099",
      "#ff0000",
      "#ff33cc",
      "#ffff00"
    )
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Impact each group had on the 3.75% inflation of 2018",
    subtitle = "as a percentage of the total rate",
    y = "contribution in %",
    x = ""
  )

#Plot 6 - ScatterPlot faceted by Year

Groups <- read.csv("6GroupWeightVariation.csv")
Groups <- Groups[, -1]
Groups$Contribution <-
  Groups$Variation * Groups$Weight / 100

Groups %>%
  filter(!Year == "2012") %>%
  ggplot(aes(
    x = Contribution,
    y = Variation,
    size = Weight,
    fill = Category
  ))  +
  scale_fill_manual(
    values = c( "#ffff00", "#000066", "#ff0000", "#00cc00",
      "#999966", "#ff33cc", "#0099ff", "#990099", "#800000")) +
  geom_point(shape = 21, colour = "transparent") +
  labs(title = "Impact of Each Group on The Annual Inflation Rate",
       y = "price variation in %",
       x = "group contribution in p.p.") +
  geom_hline(yintercept = 0) +
  facet_wrap( ~ Year) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

#Plot 7 - Groups per year for gif

summary(Groups$Contribution)
summary(Groups$Variation)


Groups %>%
  filter(Year == "2018") %>%
  ggplot(aes(
    x = Contribution,
    y = Variation,
    size = Weight,
    fill = Category
  ))  +
  geom_point(shape = 21, colour = "transparent") +
  labs(title = "Impact of Each Group on The Inflation of 2018 (3.75%)",
       y = "variation in %",
       x = "contribution in p.p.") +
  scale_fill_manual(
    values = c(
      "#ffff00",
      "#000066",
      "#ff0000",
      "#00cc00",
      "#999966",
      "#ff33cc",
      "#0099ff",
      "#990099",
      "#ff9900")) +
  geom_hline(yintercept = 0) +
  xlim(-0.5, 3.3) +
  ylim(-2, 20) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

#Plot 8 - Group 2018 facefaceted by Metro Area
summary(Groups$Contribution)
summary(Groups$Variation)


GroupMetro2018 <-
  read.csv("6GroupWeightVariation.csv")
GroupMetro2018 <- GroupMetro2018[, -1]
GroupMetro2018$Contribution <-
  (GroupMetro2018$Variation * GroupMetro2018$Weight) / 100
GroupMetro2018$ContributionScale <-
  rescale(GroupMetro2018$Contribution, to = c(0, 1))

GroupMetro2018 %>%
  filter(Year == 2018) %>%
  ggplot(aes(x = Contribution,
    y = Variation,
    size = Weight,
    fill = ContributionScale)) +
  geom_point(shape = 21) +
  scale_fill_gradient(low = "#d8dcf3", high = "#1a2454") +
  xlim(-1, 3.5) +
  ylim(-5, 25) +
  facet_wrap( ~ Category) +
  geom_hline(yintercept = 0) +
  labs(title = "Impact of Each Group on The Inflation of 2018 (3.75%)",
       y = "price variation in %",
       x = "contribution in p.p.") +
    theme_minimal(base_size = 12) +
  theme(legend.position = "none")


#Plot 9 - Items
#Basile, I tried adding the "scale_x_log10()" to this scatter plot
#but then it eliminates all the items below zero in both axes 
#I tried to force a "xlim() / ylim()" but then I got an error msg
#that there cannot be two different scales.

Items2018 <- read.csv("Items2018.csv")
Items2018 <- Items2018[, -1]
Items2018$Contribution <-
  (Items2018$Variation * Items2018$Weight) / 100

summary(Items2018$Variation)
summary(Items2018$Contribution)


Items2018 %>%
  ggplot(aes(
    x = Contribution,
    y = Variation,
    size = Weight,
    fill = Category,
    alpha = 0.1)) +
  geom_point(shape = 21, colour = "transparent") +
  geom_hline(yintercept = 0) +
  labs(title = "Impact of Each Item on the 2018 inflation",
       y = "variation in %",
       x = "impact in p.p.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none") +
  annotate("text", x = .41, y = 14, label = "Healthcare") +
  annotate("text", x = .415, y = 12, label = "plan") +
  annotate("text", x = .389, y = 5.8, label = "Fuel forVehicle") +
  annotate("text", x = .37, y = 8.7, label = "Public Transport") +
  annotate("text", x = .28, y = 10.8, label = "Electricity bill") +
  annotate("text", x = .325, y = 3.45, label = "Eating Out") +
  annotate("text", x = .20, y = 1.7, label = "Personal Services") +
  annotate("text", x = .26, y = 6.5, label = "Rent") +
  annotate("text", x = .23, y = 37.5, label = "Roots and legumes") +
  annotate("text", x = .18, y = 10, label = "Regular") +
  annotate("text", x = .18, y = 8, label = "Courses") +
  annotate("text", x = -0.09, y = 1.5, label = "Personal") +
  annotate("text", x = -0.09, y = -1, label = "hygiene") +
  annotate("text", x = -0.005, y = -4, label = "Sugar")



#Plot 10 - Scatterplot Subitems
Subitems2018 <- read.csv("Subitems2018.csv")
Subitems2018 <- Subitems2018[, -4]
Subitems2018 <- Subitems2018[, -1]
Subitems2018$Contribution <-
  (Subitems2018$Variation * Subitems2018$Weight) / 100

#The same happened to this one regarding the log scale

Subitems2018 %>%
  ggplot(aes(
    x = Contribution,
    y = Variation,
    size = Weight,
    fill = Category,
    alpha = 1.9)) +
  geom_point(shape = 21, colour = "transparent") +
  scale_fill_manual(values = 
      c("#ff33cc", "#ffff00", "#ff0000", "#00cc00", 
        "#999966", "#000066", "#0099ff", "#990099", "#800000")) +
  geom_hline(yintercept = 0) +
  labs(title = "Impact of Each Subitem on The 2018 Inflation",
       y = "variation in %",
       x = "impact in p.p.") +
  theme_minimal(base_size = 12) +
  annotate("text", x = .415, y = 20, label = "Healthcare") +
  annotate("text", x = .415, y = 15.5, label = "plan") +
  annotate("text", x = .36,  y = 9.2, label = "Electricity bill") +
  annotate("text", x = .285, y = 8, label = "Gas") +
  annotate("text", x = .165, y = 12, label = "Urban bus") +
  annotate("text", x = .222,  y = 3.9, label = "Domestic worker") +
  annotate("text", x = .055, y = 73, label = "Tangerine") +
  annotate("text", x = .155, y = 71, label = "Tomato") +
  annotate("text", x = .031, y = -27.3, label = "Avocado") +
  annotate("text", x = -0.085, y = -13, label = "Perfume") +
  theme(legend.position = "none")


#Plot 11 #Bar chart - Subitems

Subitems2018$Percentage <-  Subitems2018$Contribution * 100 / 3.75
Subitems2018$Percentage <-  as.numeric(Subitems2018$Percentage)


Subitems2018 %>%
  group_by(Subitem) %>%
  arrange(desc(Percentage)) %>% head (10) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Subitem, -Percentage),
               y = Percentage), stat = "identity") +
  ylim(0, 15) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Impact each subitem had on the 3.75% inflation of 2018",
    y = "in % of the total rate",
    x = ""
  )



#Plot 12 Deflatinary subitems
Subitems2018 %>%
  group_by(Subitem) %>%
  arrange(desc(Percentage)) %>% tail (10) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Subitem, -Percentage),
               y = Percentage), stat = "identity") +
  ylim(-2.5, 0) +
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Impact each subitem had on the 3.75% inflation of 2018",
    y = "in % of the total rate",
    x = ""
  )

#summary(Items2018)
#gganimate

        
              