library(waffle)

wff_df= freq_groups[1:10,1:5]


blues = c('#c6dbef','#9ecae1','#75AADB','#6baed6','#4292c6','#2171b5','#08519c','#08306b',"black",'blue')

waffle_df1 = select(wff_df, Interval, per.Names)
waffle_df2 = select(wff_df, Interval, per.Mentions)

w1 = waffle(waffle_df1 , rows = 7, keep = TRUE, title = "Share of Names in Database by Mentions Intervals",
colors = blues, xlab="
1 square = 1%
          
          
    ")
w2 = waffle(waffle_df2 , rows = 7, keep = TRUE, title = "Share of Database entries by Mentions Intervals", 
            colors = blues, xlab = "
            1 square = 1%")

iron(w1, w2)



