dates_df_fix = str_split_fixed(countries_df$date, "/", Inf)%>%
  data.frame()

# Cleaning unknowns #

  # Separating unknown days
      invalid_days = dates_df_fix%>%
        filter(grepl('X',X1))
  
  # Separating unknown months
      invalid_months = dates_df_fix%>%
        filter(grepl('X',X2))
  
  # Counting
      n_days = nrow(invalid_days)  
      n_months = nrow(invalid_months)
  
  # Randomizing
      set.seed(1964)
        random_days = sample(01:31, n_days, replace = TRUE)%>%
          data.frame()
  
      set.seed(1964)
        random_months = sample(01:12, n_months, replace = TRUE)%>%
          data.frame()
  
  # Replacing unknown days and months with random data  
      dates_df_fix[grepl('X',dates_df_fix$X1), "X1"] <- random_days[]
      dates_df_fix[grepl('X',dates_df_fix$X2), "X2"] <- random_months[]

# Adding zeros #
    
  # Counting number of characters in day and month fields in dates_df_fix
      D = data.frame(n_D = nchar(dates_df_fix$X1))
      M = data.frame(n_M = nchar(dates_df_fix$X2))
    
    # Binding dates_df_fix and counter columns together
      dates_df_fix = cbind(dates_df_fix,D,M)
  
    # Pasting '0' to all days with character length of 1
      dates_df_fix <- dates_df_fix%>% 
        mutate(X1 = ifelse(n_D == 1,paste0('0', dates_df_fix$X1), X1))  
  
    # Pasting '0' to all months with character length of 1  
      dates_df_fix <- dates_df_fix%>% 
        mutate(X2 = ifelse(n_M == 1,paste0('0', dates_df_fix$X2), X2))  
    
      # Removing counter columns
      dates_df_fix = dates_df_fix%>%
        select(-n_D,-n_M)
      
      # Removing old dates from countries_df
      countries_df = countries_df%>%
        select(-date)
      
       # Concatenating days, months, years
      date = data.frame(date = paste0(dates_df_fix$X1,"/",dates_df_fix$X2,"/",dates_df_fix$X3))

      # Binding new dates to countries_df
      countries_df = cbind(countries_df, date)
      
      # Removing unknown years
      countries_df = countries_df%>%
        filter(!grepl('X',date))

      head(countries_df)
          
    #Checking
    #count(countries_df$X1)
    #filter(countries_df,X3 == "1997")
    
    
    