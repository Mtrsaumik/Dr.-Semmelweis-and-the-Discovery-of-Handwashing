library(tidyverse)
library(readr)
library(lubridate)

# txt to csv conversion 02yearbirthdeathclinic.txt
# Read the text file
my_data_02 <- read.table("/Users/abdullahibnmasud/python-vs-code/practice/LANG R/02yearbirthdeathclinic.txt", header = FALSE, sep = ",", stringsAsFactors = FALSE)
# Split the first column into two columns
my_data_02 <- cbind(do.call("rbind", strsplit(my_data_02$V1, "\\.")), my_data_02[-1])
# Rename the columns
colnames(my_data_02) <- c("year", "births", "deaths","clinic")
# Add a new column with row numbers (in sequence)
# my_data$row_number <- sprintf("%02d", seq_len(nrow(my_data)))
# View the resulting table
my_data_02
my_data_new_02 <- my_data_02[-1,]
# Save the table as a CSV file in a specific folder
write.csv(my_data_new_02, file = "/Users/abdullahibnmasud/python-vs-code/practice/LANG R/02yearbirthdeathclinic.csv", row.names = FALSE)
yearly_02 <- read_csv("/Users/abdullahibnmasud/python-vs-code/practice/LANG R/02yearbirthdeathclinic.csv") 


# txt to csv conversion 01datebirthdeath.txt
# Read the text file
my_data <- read.table("/Users/abdullahibnmasud/python-vs-code/practice/LANG R/01datebirthdeath.txt", header = FALSE, sep = ",", stringsAsFactors = FALSE)
# Split the first column into two columns
my_data <- cbind(do.call("rbind", strsplit(my_data$V1, "\\.")), my_data[-1])
# Rename the columns
colnames(my_data) <- c("date", "births", "deaths")
# Add a new column with row numbers (in sequence)
# my_data$row_number <- sprintf("%02d", seq_len(nrow(my_data)))
# View the resulting table
my_data
my_data_new <- my_data[-1,]
# Save the table as a CSV file in a specific folder
write.csv(my_data_new, file = "/Users/abdullahibnmasud/python-vs-code/practice/LANG R/01datebirthdeath.csv", row.names = FALSE)
yearly_01 <- read_csv("/Users/abdullahibnmasud/python-vs-code/practice/LANG R/01datebirthdeath.csv") 

#  Death at the clinics
yearly_proportion_deaths_02 <- yearly_02 %>%
  mutate(proportion_deaths = deaths/births)
options(repr.plot.width=7, repr.plot.height=4)
ggplot(yearly_proportion_deaths_02, aes(x = year, y= proportion_deaths , colour = clinic)) +geom_line()

# The handwashing begins and The effect of handwashing
yearly_proportion_deaths_01 <- yearly_01 %>%
  mutate(proportion_deaths = deaths/ births, yearly = year(as.Date(date)))
ggplot(yearly_proportion_deaths_01, aes(date,proportion_deaths))+ geom_line()+labs(x= "year", y="proportion_deaths")

# The effect of handwashing highlighted
handwashing_start = as.Date("1847-06-01")
handwash  <- yearly_proportion_deaths_01 %>%
  mutate(handwashing_started = date >= handwashing_start )
ggplot(handwash , aes(x = date, y = proportion_deaths, color = handwashing_started)) +
  geom_line()

# More handwashing, fewer deaths?
yearly_01_summary <- handwash %>%
  group_by(handwashing_started ) %>% summarise( mean(proportion_deaths))

# Calculating a 95% Confidence intrerval using t.test 
test_result <- t.test( proportion_deaths ~ handwashing_started, data = handwash)
test_result

# The data Semmelweis collected points to that:
doctors_should_wash_their_hands <- TRUE
