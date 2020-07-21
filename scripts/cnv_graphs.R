# Loading Libraries #
library(tidyverse)
library(gsheet)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(plyr)
library(reshape2)

# Calling Database and Converting to Data Frame #
    url = "https://docs.google.com/spreadsheets/d/1HdVdGZPFyExlThZAPd_RbAiz1oB033lcW8VyJwyfs7g/edit?usp=sharing"
    df_victims = gsheet2tbl(url)%>%
    data.frame()
    head(df_victims,5) # Checking if alright 

# Aggregating month data 
    
    months_list = data.frame("x" = c(0:441))
    
    death_by_month = df_victims%>%
      filter(type=="death")%>%
      select(M_Num)
    disap_by_month = df_victims%>%
      filter(type=="disap")%>%
      select(M_Num)
    dd_by_month = df_victims%>%
      select(M_Num)
    
      death_by_month = count(death_by_month$M_Num)
      disap_by_month = count(disap_by_month$M_Num)
      dd_by_month = count(dd_by_month$M_Num)
    
          death_by_month = merge(months_list,death_by_month, all =TRUE)%>%
            select(freq)
          disap_by_month = merge(months_list,disap_by_month, all =TRUE)%>%
            select(freq)
          dd_by_month = merge(months_list,dd_by_month, all =TRUE)%>%
            select(freq)
    
    victims_by_month = data.frame(months_list,death_by_month,disap_by_month,dd_by_month)
    names(victims_by_month) <- c('Month','Deaths', 'Disappearances', 'Total')
    victims_by_month[is.na(victims_by_month)] <- 0


              reord_month =reord_month%>%
                filter(variable=="Total")
                
              reord_month <- melt(victims_by_month, id=c("Month"))
              reord_month$variable <- factor(reord_month$variable, 
                                             levels = c("Total","Disappearances","Deaths"))
                
                # Plotting Month Graph 
              
                p_month = ggplot(reord_month, aes(x = Month, y = value)) + 
                  geom_line(aes(color = variable, 
                                linetype = variable),size=.5) + 
                  ggtitle("Deaths and Disappearances in the CNV",
                          subtitle="1950 - 1985") +
                  xlab("Month Number") +
                  ylab("Number of victims") + 
                  theme(text=element_text(size = 15,family = "A"),
                        axis.title.x = element_text(margin = margin(t=20,r=0,b=20,l=0)),
                        axis.title.y = element_text(margin = margin(t=00,r=20,b=0,l=20)),
                        plot.subtitle = element_text(size = 10, margin = margin(t=0,r=0,b=20,l=0)))

                  p_month


# Aggregating data for Year graph # 

  years_list = data.frame("x" = c(1950:1985))
    
  death_by_year = df_victims%>%
      filter(type=="death")%>%
      select(dd_Y)
    disap_by_year = df_victims%>%
      filter(type=="disap")%>%
      select(dd_Y)
    dd_by_year = df_victims%>%
      select(dd_Y)

      death_by_year = count(death_by_year$dd_Y)
      disap_by_year = count(disap_by_year$dd_Y)
      dd_by_year = count(dd_by_year$dd_Y)

      death_by_year = merge(years_list,death_by_year, all =TRUE)%>%
        select(freq)
      disap_by_year = merge(years_list,disap_by_year, all =TRUE)%>%
        select(freq)
      dd_by_year = merge(years_list,dd_by_year, all =TRUE)%>%
        select(freq)

      victims_by_year = data.frame(years_list,death_by_year,disap_by_year,dd_by_year)
      names(victims_by_year) <- c('Year','Deaths', 'Disappearances', 'Total')
      victims_by_year[is.na(victims_by_year)] <- 0

              reord_year <- melt(victims_by_year, id=c("Year"))
              reord_year$variable <- factor(reord_year$variable, 
                                            levels = c("Total","Disappearances","Deaths"))

              n_total = reord_year%>%
                        filter(variable == "Total")%>%
                        select(value)%>%
                        sum()
              
              n_disap = reord_year%>%
                filter(variable == "Disappearances")%>%
                select(value)%>%
                sum()
              
              n_death = reord_year%>%
                filter(variable == "Deaths")%>%
                select(value)%>%
                sum()

              # Plotting Month Graph 
              
              p_year = ggplot(reord_year, aes(x = Year, y = value)) + 
                geom_ribbon(aes(ymin = min(value), ymax = value, 
                                fill = variable), 
                                size = 1) + 
                theme_bw()+
                scale_fill_manual(values = alpha(c("#F0CFCD","#E09792","red"),.5),
                                  name = "Victim Type", 
                                  labels = c(paste("Total n =",n_total, sep=" "),
                                             paste("Disappearance n =",n_disap, sep = " "),
                                             paste("Death n =", n_death, sep = " ")))+
                ggtitle("Deaths and Disappearances in the CNV",
                        subtitle = "1950 - 1985") +
                xlab("Year") +
                ylab("Number of victims") + 
                theme(text=element_text(size = 15,family = "A"),
                      axis.title.x = element_text(margin = margin(t=20,r=0,b=20,l=0)),
                      axis.title.y = element_text(margin = margin(t=00,r=20,b=0,l=20)),
                      plot.subtitle = element_text(size = 10, margin = margin(t=0,r=0,b=20,l=0)))
              
                p_year
 
# Age histogram

               age_hist <- ggplot(df_victims, aes(age_dd, fill = factor(type))) +
                geom_histogram(colour = "#FFFFFF") +
                ggtitle("Frequency of Deaths and Disappearances by age", 
                        subtitle="1950 - 1985") +
                xlab("Age") +
                ylab("Number of victims") +
                labs(fill = "Victim type") + 
                theme(plot.subtitle = element_text(size = 10, 
                                                   margin = margin(t = 0,r = 0, b = 20, l = 0)),
                text=element_text(size = 15,family = "A"),
                axis.title.x=element_text(margin = margin(t = 20, r = 00, b = 0, l = 0)),
                axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

                age_hist


# Bar Graph on Ocupation 

                occupations = count(df_victims$ocup)
                occupations_over_10 = occupations%>%
                  filter(freq > 10)
  
               occupations_below_10 = occupations%>%
                                      filter(freq < 10)
                        occupations_below_10[occupations_below_10$freq < 10, "x"] <- "other"
                        other = data.frame(count(occupations_below_10$x))
                
                cleaned_ocup <- rbind(occupations_over_10, other)
                              attach(cleaned_ocup)
                              cleaned_ocup = cleaned_ocup[order(-freq),]
                       
                pct <- round(cleaned_ocup$freq/sum(cleaned_ocup$freq)*100)

                lbls <- paste(cleaned_ocup$x)
                              #lbls <- paste(pct, "% ",cleaned_ocup$x, sep="") # add percents to labels
                              #lbls <- paste(lbls,"%",sep="") # ad % to labels

            bar_ocup = ggplot(cleaned_ocup, aes(x = reorder(lbls,-freq),y = freq))+
              ggtitle("Victims by Occupation", subtitle="1950 - 1985")+
              xlab("Occupation") +
              ylab("Number of victims") + 
              geom_bar(stat="identity",fill ="#ff6961" ) +
              theme(plot.subtitle = element_text(size = 10, margin = margin(t = 0, r = 00, b = 20, l = 0)),
                    axis.text.x=element_text(angle = 45, vjust = 1, hjust=1),
                    axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 10, l = 0)),
                    text = element_text(size = 15, family = "A"),
                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

            bar_ocup

# Multiplot 

multiplot(p_year, bar_ocup, age_hist, cols=2)

