# Calling Database #
url <- "https://docs.google.com/spreadsheets/d/1OTPqPYQ0NsXXT_bU33ygCrICfTq1WVExzf-0uv7CNfU/edit#gid=1046921816"
main_df <- gsheet2tbl(url)

# Converting to Data.Frame #
main_df <- main_df %>%
  data.frame()

# Fixing encoding #
Encoding(rownames(main_df)) = "UTF-8"

#Removing old columns
main_df <- main_df%>%
  select(-País.da.Ação,
         -Cidade.da.Ação,
         -País.Destino,
         -Cidade.Destino, 
         -Nomes.Entidades, 
         -Data.Ação, 
         -Status)

# Checking if alright #
head(main_df)
colnames(main_df)
dim(main_df)

# Preparing countries_df

  names_df = main_df%>% #separating names
    select(stndr_name)
  
  # Splitting Country Codes in Columns by Delimiter
  countries_df = str_split_fixed(main_df$cntry_action, ";", Inf)%>%
    data.frame()

  # Binding with names in Main DF and pivoting (Transforming Columns in to Rows)
  countries_df = cbind(countries_df,names_df)
  countries_df[countries_df==""]<-NA
  countries_df = countries_df %>%
    pivot_longer(-stndr_name, values_to = "Alpha.3.code", values_drop_na = TRUE)

  #Calling lookup table
  
  lookup = read.csv("./iso_conversion_table.csv",sep=";")

  # Creating dummy countries_df  
  new <- countries_df

  
  # Testing find and replace based on lookup table
  new$Alpha.3.code[] <- lookup$ISO3[match(unlist(countries_df$Alpha.3.code), lookup$ISO2)]
      new = new%>%
      data.frame()
      
      head(new)
      
# Returns list of unique values currently in countries_df$Alpha.3.code

iso2used = unique(countries_df$Alpha.3.code)%>%
              data.frame()
              names(iso2used)[names(iso2used) == "."] <- "cn"
              
# Comparing list of used isos with lookup table
              
check = purrr::map_df(iso2used, ~ .x %in% lookup$ISO2)
        names(check)[names(check) == "cn"] <- "match"

# Creaing table to check side-by-side
        verifier = cbind(iso2used,check)
        
        verifier = verifier%>% #filtering for easy error identification
            filter(match=="FALSE")

        verifier

# Comparing old and new country columns
        
        cn_old = count(countries_df$Alpha.3.code)
        cn_new = count(new$Alpha.3.code)
     
        verifier2 = cbind(cn_old, cn_new)
        
        verifier2

#Quick tool - find use of wrong codes in main df
filter(countries_df, Alpha.3.code == "")

