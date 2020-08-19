# Cleaning unknowns #

  # Separating unknown days
      invalid_days <- filter(dates_df_fix, grepl('X',X1))
  
  # Separating unknown months
      invalid_months <- filter(dates_df_fix, grepl('X',X2))
  
  # Counting
      n_days <- nrow(invalid_days)  
      n_months <- nrow(invalid_months)
  
  # Randomizing
    
      set.seed(1964)
      random_days <- data.frame(sample(01:28, n_days, replace = TRUE)) #Only goes up to 28 because of February

      set.seed(1964)
      random_months <- data.frame(sample(01:12, n_months, replace = TRUE))
      
      # Replacing unknown days and months with random data  
      dates_df_fix[grepl('X',dates_df_fix$X1), "X1"] <- random_days[]
      dates_df_fix[grepl('X',dates_df_fix$X2), "X2"] <- random_months[]
      
# Adding zeros #
    
  # Counting number of characters in day and month fields in dates_df_fix
      D <- data.frame(n_D = nchar(dates_df_fix$X1))
      M <- data.frame(n_M = nchar(dates_df_fix$X2))
    
    # Binding dates_df_fix and counter columns together
      dates_df_fix <- cbind(dates_df_fix,D,M)
  
    # Pasting '0' to all days with character length of 1
      dates_df_fix <- mutate(dates_df_fix, X1 = ifelse(n_D == 1,paste0('0', dates_df_fix$X1), X1))  
  
    # Pasting '0' to all months with character length of 1  
      dates_df_fix <- mutate(dates_df_fix, X2 = ifelse(n_M == 1,paste0('0', dates_df_fix$X2), X2))  
    
      # Removing counter columns
      dates_df_fix <- select(dates_df_fix,-n_D,-n_M)
      
      # Removing old dates from countries_df
      countries_df <- select(countries_df,-date)
      
       # Concatenating days, months, years
      date = data.frame(date = paste0(dates_df_fix$X3,"-",dates_df_fix$X2,"-",dates_df_fix$X1))

      # Binding new dates to countries_df
      countries_df <- cbind(countries_df, date)
      
      # Removing unknown years
      countries_df <- filter(countries_df,!grepl('X',date))


    
    