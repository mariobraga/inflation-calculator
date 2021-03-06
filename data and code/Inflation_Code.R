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
library(ggpmisc)
library(zoo)
library(ggrepel)
library(xts)
library(tibble)
library(nlme)
library(plotly)
Sys.setenv("plotly_username"="mario_bmhv")
Sys.setenv("plotly_api_key"="uSV98egj0Rrp3B6ncwfA")


# Plot 1 - Line chart, Inflation trend

IPCA <- read.csv("1IPCA12m95-19.csv")
IPCA <- IPCA[, -3]
IPCA <- IPCA[, -1]
names(IPCA) <- c("Month", "Year", "IPCA")
IPCA$Date <- as.yearmon(paste(IPCA$Month, IPCA$Year), "%m %Y")

#to export to plot.ly, just added the line below.
#Plot1 <- 
#Will leave them all out below, so you can plot what I did in R

IPCA %>%
  ggplot() +
  geom_line(aes(x = Date,
                y = IPCA),
            size = 1.25) +
  labs(title = "Brazilian IPCA inflation from 1995 to 2018",
    y = "variation in %",
    x = "") +
  theme_minimal(base_size = 12) +
  labs(caption="Source: IBGE")

Plot1 <- ggplotly(Plot1)

api_create(Plot1, filename = "Plot1")



#Plot 2 - Line chart, Target System

Target <- read.csv("IPCATargetSystem.csv")
Target <- Target[, -3]
Target <- Target[, -1]
Target$Date <-
  as.yearmon(paste(Target$Month, Target$Year), "%m %Y")

#Plot2 <- 

Target %>%
  ggplot() +
  geom_line(aes(x = Date,
                y = IPCA12M),
            size = 1.25,
            color = I("black")) +
  geom_line(aes(x = Date,
    y = Upper.Limit,
    color = I("red"))) +
    geom_line(aes(x = Date,
    y = Bottom.Limit,
    color = I("red"))) +
  geom_line(linetype = "dashed") +
  (aes(x = Date,
    y = Target,
    color = I("#46A729"))) +
    labs(title = "Brazilian Inflation Target System",
    subtitle = "actual rate, central target and bottom and upper limits",
    y = "variation in %",
    x = "") +
    theme_minimal(base_size = 12) +
    labs(caption="Source: IBGE")

Plot2 <- ggplotly(Plot2)
api_create(Plot2, filename = "Plot2")



# Plot 3 - Facetd by Metropolitan Area

metropolitanAreas <- read.csv("2IPCA12mRM12-18.csv")
metropolitanAreas <- metropolitanAreas[, -1]

#Plot3 <- 

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

Plot3 <- ggplotly(Plot3)
api_create(Plot3, filename = "Plot3")



# Plot 4 - ScatterPlot - Groups

Group2018 <- read.csv("Group2018.csv")
Group2018 <- Group2018[, -1]
Group2018$Contribution <-
Group2018$Variation * Group2018$Weight / 100

#Plot4 <- 
  
  Group2018 %>%
  ggplot(aes(x = Contribution,
             y = Variation,
             size = Weight,
             fill = Category))  +
  scale_fill_manual(values = c("#ffff00",
      "#000066",
      "#ff0000",
      "#00cc00",
      "#999966",
      "#ff33cc",
      "#0099ff",
      "#990099",
      "#ff9900")) +
  geom_point(shape = 21, colour = "transparent") +
  labs(title = "Impact Each Group Had on The 3.75% Inflation of 2018",
    y = "variation in %",
    x = "contribution in p.p.") +
  geom_hline(yintercept = 0) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none") +
  annotate("text", x = .98, y = 3.6, label = "Food and\nBeverages") +
  annotate("text", x = .76, y = 3.8, label = "Transportation") +
  annotate("text", x = .74, y = 5.1, label = "Housing") +
  annotate("text", x = .48, y = 4.3, label = "Healthcare") +
  annotate("text", x = .34, y = 2.7, label = "Personal Expenses") +
  annotate("text", x = .26, y = 5.1, label = "Education") +
  annotate("text", x = .14, y = 4.1, label = "Household Goods") +
  annotate("text", x = .04, y = 0.9, label = "Clothing") +
  annotate("text", x = .06, y = -0.25,label = "Communication")


Plot4 <- ggplotly(Plot4)
api_create(Plot4, filename = "Plot4")



#Plot 5 - Bar Chart

Group2018$Percentage <-
  Group2018$Contribution * 100 / 3.75

#Plot5 <- 
  
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
      "#ffff00")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Impact each group had on the 3.75% inflation of 2018",
    subtitle = "as a percentage of the total rate",
    y = "contribution in %",
    x = "")

Plot5 <- ggplotly(Plot5)
api_create(Plot5, filename = "Plot5")


#Plot 6 - ScatterPlot faceted by Year

Groups <- read.csv("6GroupWeightVariation.csv")
Groups <- Groups[, -1]
Groups$Contribution <-
  Groups$Variation * Groups$Weight / 100

#Plot6 <- 

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
  labs(title = "Contribution of Each Group to the Annual Rates",
       y = "price variation in %",
       x = "group contribution in p.p.") +
  geom_hline(yintercept = 0) +
  facet_wrap( ~ Year) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

Plot6 <- ggplotly(Plot6)
api_create(Plot6, filename = "Plot6")


#Plot 8 - Group faceted per Year
#For this one, I filtered and exported one per year to make a gif

summary(Groups$Contribution)
summary(Groups$Variation)


GroupYear <-
  read.csv("6GroupWeightVariation.csv")
GroupYear <- GroupYear[, -1]
GroupYear$Contribution <-
  (GroupYear$Variation * GroupYear$Weight) / 100
GroupYear$ContributionScale <-
  rescale(GroupYear$Contribution, to = c(0, 1))

GroupYear %>%
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
#I tried adding the "scale_x_log10()" to this scatter plot
#but then it eliminates all the items below zero in both axes 
#I tried to force a "xlim() / ylim()" but then I got an error msg
#that there cannot be two different scales.

Items2018 <- read.csv("Items2018.csv")
Items2018$Contribution <-
  (Items2018$Variation * Items2018$Weight) / 100

#summary(Items2018$Variation)
#summary(Items2018$Contribution)
#Used the summary to set the same scales in all the plots


#Plot9 <- 

Items2018 %>%
  ggplot(aes(
    x = Contribution,
    y = Variation,
    size = Weight,
    fill = Category)) +
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


Plot9 <- ggplotly(Plot9)
api_create(Plot9, filename = "Plot9")



#Plot10 - Top contributors
Items2018$Percentage <-  Items2018$Contribution * 100 / 3.75
Items2018$Percentage <-  as.numeric(Items2018$Percentage)

  #Plot10 <- 

Items2018 %>%
  group_by(Item) %>%
  arrange(desc(Percentage)) %>% head (10) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Item, -Percentage),
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
                 "#ffff00",
                 "#ffff00"))   +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Impact of Each Item on the 2018 Inflation",
    y = "% of the total rate",
    x = "")
  
Plot10 <- ggplotly(Plot10)
api_create(Plot10, filename = "Plot10")

  
  
#Plot 11 - Deflationary items

#Plot11 <- 
  
Items2018 %>%
  group_by(Item) %>%
  arrange(desc(Percentage)) %>% tail (7) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Item, -Percentage),
               y = Percentage),
           stat = "identity",
           fill = c(
             "#000066",
             "#00cc00",
             "#ff9900",
             "#0099ff",
             "#999966",
             "#990099",
             "#ff0000"))   +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Impact of Each Item on the 2018 Inflation",
    y = "% of the total rate",
    x = "")

Plot11 <- ggplotly(Plot11)
api_create(Plot11, filename = "Plot11")


#Plots not used 

#Plot 7 - Groups per year for gif
#Had initially filtered and exported one per year to do a gif
#As I managed to make the interactive scatter plot on plot.ly, 
#I decided not to use this, since the facet interactive allows the reader
#to navigate through the details of each year

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



#Plot 10 - Scatterplot Subitems
Subitems2018 <- read.csv("Subitems2018.csv")
Subitems2018 <- Subitems2018[, -4]
Subitems2018 <- Subitems2018[, -1]
Subitems2018$Contribution <-
  (Subitems2018$Variation * Subitems2018$Weight) / 100

#same issue with log scale

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


#Plot 11 - Top 10 Inflationary subitems

Subitems2018$Percentage <-  Subitems2018$Contribution * 100 / 3.75
Subitems2018$Percentage <-  as.numeric(Subitems2018$Percentage)

Subitems2018 %>%
  group_by(Subitem) %>%
  arrange(desc(Percentage)) %>% head (10) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Subitem, -Percentage),
               y = Percentage),
           stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Impact Each Subitem Had on The 3.75% Inflation of 2018",
    y = "in % of the total rate",
    x = "")
   
#Plot 12 Top 10 Deflationary subitems

Subitems2018 %>%
  group_by(Subitem) %>%
  arrange(desc(Percentage)) %>% tail (10) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Subitem, -Percentage),
        y = Percentage),
    stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Impact Each Subitem Had on The 3.75% Inflation of 2018",
    y = "in % of the total rate",
    x = "")
