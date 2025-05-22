
install.packages("rvest")

# Load necessary libraries
library(rvest)
library(httr)
library(dplyr)


# Read the HTML content of the website
webpage <- read_html("https://www.nationalredlist.org/assessments")

# Select the table using CSS selector
table_node <- html_nodes(webpage, "table")

# Extract the table content
table_content <- html_table(table_node)

# Print the table
head(table_content)


# URL of the website
url <- "https://www.nationalredlist.org/assessments"

# Read the HTML code of the page
html_code <- read_html(url)

# Use the html_nodes function to extract the table
table_html <- html_code %>% html_nodes("table") 

# Use the html_table function to convert the table 
# HTML code into a data frame
table_df <- table_html %>% html_table()

# Inspect the first few rows of the data frame
df <- as.data.frame(table_df)

