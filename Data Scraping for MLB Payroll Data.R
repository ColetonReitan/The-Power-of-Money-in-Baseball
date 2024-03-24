library(rvest)
library(dplyr)
library(httr)


# Specify the years you are interested in
years <- c(2021, 2022, 2023)

# List of MLB teams (replace with the actual list of teams or scrape it from the website)
teams <- c("arizona-diamondbacks", "atlanta-braves", "baltimore-orioles",
           "boston-red-sox", "chicago-white-sox", "chicago-cubs",
           "cincinnati-reds", "cleveland-guardians", "colorado-rockies",
           "detroit-tigers", "houston-astros", "kansas-city-royals",
           "los-angeles-angels", "los-angeles-dodgers", "miami-marlins",
           "milwaukee-brewers", "minnesota-twins", "new-york-yankees",
           "new-york-mets", "oakland-athletics", "philadelphia-phillies",
           "pittsburgh-pirates", "san-diego-padres", "san-francisco-giants",
           "seattle-mariners", "st-louis-cardinals", "tampa-bay-rays",
           "texas-rangers", "toronto-blue-jays", "washington-nationals")

# List of target comments
target_comments <- c("IR", "DEAD", "DEFERRED", "ACTIVE", "RESTRICTED LIST")

# Loop through each team
for (team_slug in teams) {
  # Loop through each year
  for (year in years) {
    # Construct the URL for the specific team and year
    url <- paste0("https://www.spotrac.com/mlb/", team_slug, "/payroll/", year, "/")
    
    # Read HTML content from the URL
    page <- read_html(url)
    
    # Loop through each target comment
    for (target_comment in target_comments) {
      # Find the comment node
      comment_node <- html_nodes(page, xpath = paste0("//comment()[contains(., '", target_comment, "')]"))
      
      # Check if the comment node exists
      if (length(comment_node) > 0) {
        # Find the first following sibling table
        table_after_comment <- xml2::xml_find_first(comment_node, ".//following-sibling::table") %>%
          html_table()
        
        # Convert the list to a data frame
        df <- bind_cols(
          data.frame(Team = team_slug, Year = year, Type = target_comment),
          bind_rows(table_after_comment)
        )
        
        # Rename the player column to "Player"
        df <- rename(df, Player = starts_with("Player"))
        df <- rename(df, Player = starts_with("Active"))
        df <- rename(df, Adj.Salary = starts_with("Adj."))
        
        # Create a data frame name
        df_name <- paste0(team_slug, "_", target_comment, "_", year, "_df")
        
        # Assign the data frame to the environment
        assign(df_name, df)
      }
    }
  }
}



#Cleaning the data so that all numeric variables are of same type (integer)

# List all data frames in the environment
all_data_frames <- ls(pattern = "_df")

# Function to remove non-numeric characters and convert to numeric
clean_and_numeric <- function(column) {
  as.integer(gsub("[^0-9.]", "", column))
}

# Loop through each data frame
for (df_name in all_data_frames) {
  df <- get(df_name)
  
  # Check if 'Lux. Tax Salary' column exists in the data frame
  if ('Lux. Tax Salary' %in% colnames(df)) {
    # Remove non-numeric characters and convert to numeric
    df$`Lux. Tax Salary` <- clean_and_numeric(df$`Lux. Tax Salary`)
  }
  
  # Check if 'Total Salary' column exists in the data frame
  if ('Total Salary' %in% colnames(df)) {
    # Remove non-numeric characters and convert to numeric
    df$`Total Salary` <- clean_and_numeric(df$`Total Salary`)
  }
  
  # Check if 'Base Salary' column exists in the data frame
  if ('Base Salary' %in% colnames(df)) {
    # Remove non-numeric characters and convert to numeric
    df$`Base Salary` <- clean_and_numeric(df$`Base Salary`)
  }
  
  # Check if 'Payroll Salary' column exists in the data frame
  if ('Payroll Salary' %in% colnames(df)) {
    # Remove non-numeric characters and convert to numeric
    df$`Payroll Salary` <- clean_and_numeric(df$`Payroll Salary`)
  }
  # Check if 'Signing Bonus' column exists in the data frame
  if ('Signing Bonus' %in% colnames(df)) {
    # Remove non-numeric characters and convert to numeric
    df$`Signing Bonus` <- clean_and_numeric(df$`Signing Bonus`)
  }
  
  # Check if 'Incentives' column exists in the data frame
  if ('Incentives' %in% colnames(df)) {
    # Remove non-numeric characters and convert to numeric
    df$`Incentives` <- clean_and_numeric(df$`Incentives`)
  }
  
  # Check if 'Adj. Salary' column exists in the data frame
  if ('Adj.Salary' %in% colnames(df)) {
    # Remove non-numeric characters and convert to numeric
    df$`Adj.Salary` <- clean_and_numeric(df$`Adj.Salary`)
  }
  
  # Assign the modified data frame back to the environment
  assign(df_name, df)
}
#Give this a second to run through all df's before running next line of code


#Combining the corresponding comments dataframes together

# Remaking list of data frames to combine
all_data_frames <- ls(pattern = "_df")

# Initialize empty data frames for Active and Deferred
active_data <- data.frame()
deferred_data <- data.frame()
ir_data <- data.frame()
restricted_data <- data.frame()
dead_data <- data.frame()

# Loop through each team data frame
for (team_df in all_data_frames) {
  
  if (grepl("ACTIVE", team_df)) {
    active_data <- bind_rows(active_data, get(team_df))
  }
  
  if (grepl("IR", team_df)) {
    ir_data <- bind_rows(ir_data, get(team_df))
  }
  
  if (grepl("DEAD", team_df)) {
    dead_data <- bind_rows(dead_data, get(team_df))
  }
  
  if (grepl("RESTRICTED", team_df)) {
    restricted_data <- bind_rows(restricted_data, get(team_df))
  }
  
  if (grepl("DEFERRED", team_df)) {
    deferred_data <- bind_rows(deferred_data, get(team_df))
  }
}



#Combining 4 of the 5 different tables we have (excluding deferred, for now)

# Specify the names of your data frames
data_frame_names <- c("active_data", "ir_data", "restricted_data", "dead_data")

# Create a list of data frames from the specified names
list_of_data_frames <- lapply(data_frame_names, get)

# Combine the data frames using bind_rows
combined_df <- bind_rows(list_of_data_frames)





#Bringing in Overall Yearly Payroll Data

# Specify the years you are interested in
years <- c(2021, 2022, 2023)

# List to store the data frames for each year
all_data <- list()

# Loop through each year
for (year in years) {
  # Construct the URL for the specific year
  url <- paste0("https://www.spotrac.com/mlb/payroll/", year, "/")
  
  # Read HTML content from the URL
  page <- read_html(url)
  
  # Find all tables on the page
  tables <- html_table(html_nodes(page, "table"))
  
  # Check if tables are found
  if (length(tables) > 0) {
    # Assuming the first table is the one of interest, you can adjust if needed
    df <- data.frame(
      Year = year,
      do.call(bind_rows, tables[[1]])
    )
    
    # Rename the player column to "Player"
    df <- rename(df, TotalPayroll = starts_with("X20"))
    
    # Create a unique name for each data frame
    df_name <- paste0("payroll_", year, "_df")
    
    # Assign the data frame to the environment
    assign(df_name, df)
    
    
    # Add the data frame to the list with a unique name
    all_data[[df_name]] <- df
  } else {
    print("No tables found for year:", year)
  }
}



#Combining the total payroll dataframes into one

# Specify the names of your data frames
payroll_data_frame_names <- c("payroll_2021_df", "payroll_2022_df", "payroll_2023_df")

# Create a list of data frames from the specified names
payroll_list_of_data_frames <- lapply(payroll_data_frame_names, get)

# Combine the data frames using bind_rows
payroll_combined_df <- bind_rows(payroll_list_of_data_frames)



#Need to update team name column so they are both the same so we can merge for following part
# Function to clean and standardize team names
clean_team_names <- function(team_name) {
  team_name <- gsub("\n.*$|\t.*$", "", team_name, perl = TRUE)  # Remove anything after \n or \t
  team_name <- gsub(" ", "-", team_name)  # Replace spaces with dashes
  return(tolower(team_name))  # Convert to lowercase
}
# Apply the function to both dataframes
combined_df$Team <- clean_team_names(combined_df$Team)
payroll_combined_df$Team <- clean_team_names(payroll_combined_df$Team)



#Combining the two dataframes to be the way they are in the Excel sheet

# Merge based on Team and Year using dplyr
result_df <- combined_df %>%
  left_join(payroll_combined_df, by = c("Team", "Year"))

#Adding League Average column to the df

# Extract league-average data from payroll_combined_df
league_average_df <- payroll_combined_df %>%
  filter(Team == "league-average", Win. !="NA") %>%
  select(Year, LeagueAverage = TotalPayroll)

# Merge result_df with league_average_df based on Year
result_df <- result_df %>%
  left_join(league_average_df, by = "Year")


# Adding the previous year payroll to the result df

# Convert Year column to numeric for easier manipulation
result_df$Year <- as.numeric(result_df$Year)

# Function to get the previous year's TotalPayroll for a given team and year
getPreviousYearPayroll <- function(team, year) {
  PreviousYear <- year - 1
  PreviousYearPayroll <- result_df$TotalPayroll[result_df$Team == team & result_df$Year == PreviousYear]
  return(ifelse(length(PreviousYearPayroll) > 0, PreviousYearPayroll, NA))
}

# Apply the function to create the PreviousYearPayroll column
result_df$PreviousYearPayroll <- mapply(getPreviousYearPayroll, result_df$Team, result_df$Year)


# Adding the percent change column to the result df

# Convert TotalPayroll and PreviousYearPayroll columns to numeric
result_df$TotalPayroll <- as.numeric(gsub("[$,]", "", result_df$TotalPayroll))
result_df$PreviousYearPayroll <- as.numeric(gsub("[$,]", "", result_df$PreviousYearPayroll))

# Calculate percent change
result_df$PercentChange <- ifelse(!is.na(result_df$PreviousYearPayroll),
                                  ((result_df$TotalPayroll - result_df$PreviousYearPayroll) / result_df$PreviousYearPayroll) * 100,
                                  NA)

# Adding the difference from previous column

# Create a new column "DifferenceFromPrevious"
result_df$DifferenceFromPrevious <- result_df$TotalPayroll - result_df$PreviousYearPayroll


# Adding the difference from average column

# Create a new column "DifferenceFromAverage"
result_df$LeagueAverage <- as.numeric(gsub("[$,]", "", result_df$LeagueAverage))
result_df$DifferenceFromAverage <- result_df$TotalPayroll - result_df$LeagueAverage


#Adding highest and lowest payroll binary columns

# Create binary columns HighestPayroll and LowestPayroll
result_df <- result_df %>%
  group_by(Year) %>%
  mutate(HighestPayroll = ifelse(TotalPayroll == max(TotalPayroll, na.rm = TRUE), "Y", "N"),
         LowestPayroll = ifelse(TotalPayroll == min(TotalPayroll, na.rm = TRUE), "Y", "N"))



#Remaining Columns in Original Excel: 
# Wins
# Playoffs
# WSWin