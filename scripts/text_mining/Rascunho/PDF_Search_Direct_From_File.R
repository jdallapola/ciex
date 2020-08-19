### PDF Search ###

library(pdfsearch)

# Setting directory to pdf file location
directory = "C:/Users/jdall/Dropbox/Fundação Getulio Vargas/Produção Intelectual/Authoritarian Diffusion Project/Authoritarian Diffusion/CIEX/Text-mining/pdfs"

        search_key = "contra"
        
        result <- keyword_directory(directory,
                                    keyword = search_key,
                                    surround_lines = 1, full_names = TRUE)
        
                result_to_csv = result %>%
                  select(ID,pdf_name)
                
                write.csv(result_to_csv,'C:/Users/jdall/Desktop/result_output.csv', row.names = FALSE)


# Attempting to use Titles Recorded Manually in Database as search keys #

  # Calling database from Google Sheets #

    library(gsheet)
    url = "https://docs.google.com/spreadsheets/d/1OTPqPYQ0NsXXT_bU33ygCrICfTq1WVExzf-0uv7CNfU/edit?usp=sharing"
    ciex_db = gsheet2tbl(url)
    
    ciex_db = ciex_db %>%
      data.frame()

    Encoding(rownames(ciex_db)) = "UTF-8"
    
    # Selecting Title Column #

    doc_titles = ciex_db[,c("title"),drop=FALSE]%>%
      na.omit(doc_titles)
    
    doc_titles = head(doc_titles)

