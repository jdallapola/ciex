# Loading Libraries #
library(tidyverse)
library(gsheet)
library(dplyr)
library(plyr)

# Calling Database #
url <- "https://docs.google.com/spreadsheets/d/1OTPqPYQ0NsXXT_bU33ygCrICfTq1WVExzf-0uv7CNfU/edit#gid=1046921816" 
intact_main_df <- data.frame(gsheet2tbl(url))

main_df = select(intact_main_df,!contains("obsolete"))


# Checking if alright #
head(main_df)
colnames(main_df)
dim(main_df)

# Checking for errors

count_ties = data.frame(t = str_count(main_df$ties, ";"))
count_ties[is.na(count_ties)] <- -1 
count_id = data.frame(id = str_count(main_df$ties_ids, ";"))
count_id[is.na(count_id)] <- -1 

error_ties = cbind(main_df,count_ties,count_id)%>%
          mutate(total_ties = t - id)%>%
          filter(total_ties != 0)

nrow(errors_ties) # Should be 0

###############

main_df = main_df%>%
  filter(stndr_name != "[Unidentified by CIEX]")%>%
  filter(!grepl('u.f.n.',stndr_name))%>%
  filter(!grepl('u.l.n.',stndr_name))%>%
  filter(!grepl('illegible',stndr_name))

###############

main_df$birth<-ifelse(is.na(main_df$iden_cntry_birth),main_df$birth<-main_df$assum_cntry_birth,main_df$birth<-main_df$iden_cntry_birth)

# Splitting Country Codes in Columns by Delimiter

    ties_df<-separate_rows(main_df, ties, sep = ";" , convert = TRUE)%>%
      data.frame()%>%
      select(stndr_name, ties,ties_ids)
      count(ties_df$ties) #Checking for anomalies
  
    ties_id_df<-separate_rows(main_df, ties_ids, sep = ";" , convert = TRUE)%>%
      data.frame()
      count(ties_id_df$ties_ids) #Checking for anomalies
      
    ties_df = mutate(ties_df, ties_ids = ties_id_df$ties_ids)
        count(ties_df$ties_ids)
        MIL_ties = filter(ties_df, ties_ids == 3)
      
            write.csv(MIL_ties, "./military_ties.csv",row.names = FALSE)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ## Attempts at network graphs
    
    # select year and country to analyze
    
    y = 1966
    cn = "Uruguay"
    cn_list = data.frame(cn = unique(main_df$birth))
    i=0
    
    
    while(i < 26){
      
      for(y in 1966:1986){
        
        i = i+1
        cn = cn_list[i,1]
        y = y
        
      }
    }  
    
    
    unique_id = data.frame(unique_id = paste0(ties_df$year,ties_df$entr,ties_df$n_pdf,ties_df$n_doc,".",ties_df$ties))   
    
    ties_df = cbind(ties_df, unique_id)
    
    ties_df = ties_df%>%
      select(unique_id, stndr_name, ties, year,birth)%>%
      filter(!is.na(ties))

    ties_df$ties <- trimws(ties_df$ties, which = c("both"))
    
    ties_df = ties_df%>%
      filter(birth == cn)%>%
      filter(year == y)
    


edge_list = ties_df%>% group_by(unique_id) %>%
            filter(n()>=2) %>% group_by(unique_id) %>%
            do(data.frame(t(combn(.$stndr_name, 2)), stringsAsFactors=FALSE))

nodes = data.frame(stndr_name = unique(ties_df$stndr_name))

nodes <- nodes %>% rowid_to_column("id")

edges <- edge_list %>% 
  left_join(nodes, by = c("X1" = "stndr_name")) %>% 
  dplyr::rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("X2" = "stndr_name")) %>% 
  dplyr::rename(to = id)

edges <- edges%>%
  select(from, to)

routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", 
                          ignore.eval = FALSE, 
                          loops = FALSE, 
                          directed = FALSE)
plot(routes_network, vertex.cex = 3)


