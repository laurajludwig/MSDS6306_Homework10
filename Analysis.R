# Load libraries
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)

# Get health care facilities data
load("N-MHSS-2015-DS0001-data-r.rda")

# Reassign the data frame to be more usable
data <- mh2015_puf
data$LST <- strtrim(as.character(data$LST),2)
# In the write-up, include a comment on the reason for making this change to the data type

# Extract list of states
states <- unique(data$LST)

# Identify the VA facilities
'%!in%' <- function(x,y)!('%in%'(x,y))  # Create a 'not in' function
data$VAbin <- grepl("^V",data$FACILITYTYPE)  # Use a regular expression to assign a binary true/false for being a VA facility
VAonly <- filter(data, VAbin == "TRUE" & LST %!in% c("AK","HI","AS","GU","PR","VI"))  # Filter to create a VA-only dataframe for mainland

# Create summary table
VAcount <-count(VAonly, 'LST')

# Create plot
ggplot(VAcount, aes(x=LST,y=freq,fill=LST)) + geom_col() + 
  labs(title="VA Facilities by State in Lower 48", y="Count", x="State",fill="State") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x  = element_text(angle=90,vjust=0.5))

# Get State Size data
statesize <- read.csv('statesize.csv')

# Create merged data set
VAwSize <- merge(VAcount,statesize, by.x="LST",by.y="Abbrev")

# Create new variable for number of facilities per 1000 square miles
VAwSize$CountbyArea <- VAwSize$freq/(VAwSize$SqMiles/1000)

# Create plot
ggplot(VAwSize, aes(x=reorder(LST,-CountbyArea),y=CountbyArea,fill=Region)) + geom_col() + 
  labs(title="VA Facilities per 1000 Square Miles by State in Lower 48", y="Count per 1000 Sq Miles", x="State",fill="Region") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x  = element_text(angle=90,vjust=0.5)) +
  scale_fill_manual(values = c("#009E73", "#F0E442", "#0072B2", "#D55E00"))


