---
title: "Homework10"
author: "Laura Ludwig"
date: "November 7, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction and Problem Statement
Providing quality healthcare to veterans can be a challenge due to the geographic expanse of the United States. Evaluating location options for new VA facilities should include many factors. This analysis will examine the number of VA facilities compared to the geographic area of the continguous states. 

## Getting Data
```{r facilitiesdata } 
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
load("N-MHSS-2015-DS0001-data-r.rda")
```
Data on VA facilities is extracted from a data set gathered by the Substance Abuse & Mental Health Data Archive. More information is available [here](https://datafiles.samhsa.gov/study-dataset/national-mental-health-services-survey-2015-n-mhss-2015-ds0001-nid17098). There are 139 variables contained within this data set. For the purposes of this analysis, the state codes and the facility type are the key elements.

```{r dataclean}
# Reassign the data frame to be more usable
data <- mh2015_puf
data$LST <- strtrim(as.character(data$LST),2)
```
While looking at this data set, there was additional characters included in the state code ("LST"). It was cleaned to make it easier to work with during the analysis. 

```{r statelist}
# Extract list of states
states <- unique(data$LST)
```
A list of states is extracted from the overall data as a useful variable reference.

```{r vafacilities}
# Identify the VA facilities
'%!in%' <- function(x,y)!('%in%'(x,y))  # Create a 'not in' function
data$VAbin <- grepl("^V",data$FACILITYTYPE)  # Use a regular expression to assign a binary true/false for being a VA facility
VAonly <- filter(data, VAbin == "TRUE" & LST %!in% c("AK","HI","AS","GU","PR","VI"))  # Filter to create a VA-only dataframe for mainland
```
Because the question of interest is where to locate new VA facilities in the mainland of the United States, the full dataset is filtered down to understand more about what facilities the VA currently has. The FACILITYTYPE that starts with "V" is extracted (the only option of the 11 options that starts with "V" are VA facilities), and the data frame is filtered to show only the facilities in the mainland. 

## Exploring VA Facilities Data
```{r summary}
# Create summary table
VAcount <-count(VAonly, 'LST')
VAcount
```
*Count of VA Facilities by State*

```{r countplot1}
# Create plot
ggplot(VAcount, aes(x=LST,y=freq,fill=LST)) + geom_col() + 
  labs(title="VA Facilities by State in Lower 48", y="Count", x="State",fill="State") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x  = element_text(angle=90,vjust=0.5))
```

This data is useful, but makes the assumption that all states are the same size. For many veterans, getting to a VA facility is a full-day endeavor, and it would be helpful to look not just at the number of facilities, but their density based on the geographic spread of states.

## Getting More Data
```{r statedata}
# Get State Size data
statesize <- read.csv('statesize.csv')
```

The statesize.csv has several useful variables:

Variable      | Description
------------- | -------------
StateName | Full name of the state
SqMiles | Number of square miles of area the state covers
Abbrev | A two-letter state code
Region | A factor indicating a region for the state

```{r mergedata}
# Create merged data set
VAwSize <- merge(VAcount,statesize, by.x="LST",by.y="Abbrev")
```
The two-character state code is a useful tool for combining the data into one set. The state code from the statesize data set was clean coming from the source.

```{r createmetric}
# Create new variable for number of facilities per 1000 square miles
VAwSize$CountbyArea <- VAwSize$freq/(VAwSize$SqMiles/1000)
```
A metric was created to show a ratio of the number of VA facilities by area. However, the units of the two variables count and SqMiles are very different. In order to see differences in the calculated metric, the SqMiles variable was first divided by 1000 to move everything to the scale of Count of VA Facilities per 1000 sqare miles.

```{r countplot2}
# Create plot
ggplot(VAwSize, aes(x=reorder(LST,-CountbyArea),y=CountbyArea,fill=Region)) + geom_col() + 
  labs(title="VA Facilities per 1000 Square Miles by State in Lower 48", y="Count per 1000 Sq Miles", x="State",fill="Region") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x  = element_text(angle=90,vjust=0.5)) +
  scale_fill_manual(values = c("#009E73", "#F0E442", "#0072B2", "#D55E00"))
```

The above plot reveals a number of useful things. The most blatant of the observations to be made is that there is a high density of VA hospitals in the Northeast, and there do not seem to be lots of facilties in the West region.

## Conclusions and Recommendations
The ratio of count of VA facilities to 1000s of square miles should not be the only metric for narrowing down sites for additional VA facilities to be located. The size of the states in geographic terms does not factor in the population density of the states, or the residency of veterans throughout the states. Bringing in more information and detail around population density would be useful for identifying good candidate locations. 