# Set the working directory to the folder containing your files
setwd("/Users/Emily/Documents/git-repos/nanwakolas-watersheds-timeseries/working-directory")

# List all files in the working directory
file_list <- list.files()

# Loop through each file in the list
for (file in file_list) {
  
  # Check if the file has a .csv extension
  if (tolower(substr(file, nchar(file) - 3, nchar(file))) == ".csv") {
    
    # Print the current file being processed
    cat("Processing file:", file, "\n")
    
    ###################################################################
    # Load and preprocess data
    ###################################################################
    
    # Load libraries
    lapply(c("tidyverse", "lubridate", "reshape", "stringr", "plotly", "roll", "data.table"), library, character.only = TRUE)
    
    # Load data - read headers
    fileheaders <- read.csv("fulmore-2023-qc.csv", nrows = 1, as.is = TRUE, header = FALSE)
    
    # Read in data, drop redundant header rows
    df <- read.csv("fulmore-2023-qc.csv", header = FALSE, skip = 4, stringsAsFactors = FALSE)
    
    # Add headers to dataframe
    colnames(df) <- fileheaders
    names(df)
    glimpse(df)
    
    # Your existing code for loading and restructuring data goes here
    # Rename date and wrangle column headers
    colnames(df)[1] <- "date"
    
    # Remove unnecessary columns
    df <- df %>%
      select(date, -contains("level") | -contains("unesco"))
    
    colnames(df) <- tolower(colnames(df))
    colnames(df) <- str_replace_all(colnames(df), "_tb1_", "tb1_")
    colnames(df) <- str_replace_all(colnames(df), "_q_flags", "_qflag")
    colnames(df) <- str_replace_all(colnames(df), "_tb1_", "tb1_")
    colnames(df) <- str_replace_all(colnames(df), "depth", "depth_")
    colnames(df) <- str_replace_all(colnames(df), "twtr", "twtr_")
    
    # Separate into character data frame
    char_df <- df %>% 
      select(date, contains("flag"))
    
    # Pivot the data using a regular expression in cols
    result_df <- pivot_longer(char_df, cols = -date, 
                              names_to = c("variable", "site", "blank"), 
                              names_sep = "_") %>% 
      select(-contains("blank"))
    
    # Check if "value" column exists and rename it to "flag"
    if ("value" %in% colnames(result_df)) {
      colnames(result_df)[colnames(result_df) == "value"] <- "flag"
    }
    
    # Print the resulting data frame
    print(result_df)
    
    # Separate into numeric data frame
    num_df <- df %>% 
      select("date", contains("avg"))
    
    # Modify column names
    colnames(num_df) <- gsub("_avg", "", colnames(num_df))
    
    # Pivot
    numresult_df <- pivot_longer(num_df, cols = -date, 
                                 names_to = c("variable", "site", "blank"), 
                                 names_sep = "_") %>% 
      select(-contains("blank"))
    
    # Merge numeric and character data frames
    merged_df <- merge(result_df, numresult_df, by = c("date", "site", "variable"))
    
    # Save the merged dataframe to a new CSV file
    write.csv(merged_df, paste0("merged_", gsub(".csv", "_df.csv", tools::file_path_sans_ext(file))))
    
    # Print a message indicating completion
    cat("Processing complete for file:", file, "\n\n")
  }
}

# Print a message indicating the end of the loop
cat("All files processed.")
