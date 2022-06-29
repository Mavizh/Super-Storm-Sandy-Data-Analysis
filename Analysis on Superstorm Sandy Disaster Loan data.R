library(ggplot2)
library(readxl)
library(plyr)
library(ggforce)
library(tidyr)
library(treemapify)
library(ggplotify)
library(reshape2)
library(maps)
library(usmap)
library(ggsci)

#Read data from excel

df <- read_excel("C:\\Users\\Admin\\OneDrive - stevens.edu\\Documents\\Grad Courses\\FA 550 Data Viz\\new data.xlsx")
groupcolumns = c("State")
df <- as.data.frame(df) #Converting to dataframe

#Preparing data for graph 1

dataColumns = c("Total Verified Loss", "Verified Loss Real Estate","Verified Loss Content")
res_df <- ddply(df, groupcolumns, function(x) colSums(x[dataColumns]))
res_df <- pivot_longer(res_df, 'Verified Loss Real Estate':'Verified Loss Content', names_to = "State1", values_to = "Amount")
res_df <- res_df %>% mutate(Amount = Amount/1e6)
res_df <- res_df %>% mutate(`Total Verified Loss` = `Total Verified Loss`/1e6)
res_df$`Total Verified Loss` <- round(res_df$`Total Verified Loss` ,digit=1)

#plotting Graph 1

res_df$zoom <- ifelse(res_df$`Total Verified Loss` < 8.5, TRUE, FALSE)
ggplot(res_df, aes(x=reorder(State, -Amount), y= Amount, fill=State1, width=0.8, position = 'fill')) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text( vjust=0.6)) + 
  labs(title="Total Verified Loss Amount per State", subtitle = "Superstorm Sandy") +
  xlab("State") +
  ylab("Amount (In Millions)") +
  geom_col() + 
  facet_zoom(y= `Total Verified Loss`< 8.5,horizontal = TRUE, zoom.data = zoom("TRUE"), zoom.size = 0.5, show.area = FALSE, ylim = c(0,8.5)) + 
  geom_text(aes(State, `Total Verified Loss`, label = `Total Verified Loss`, fill = NULL, group = State), data = res_df, position = position_nudge(y = 0.05)) +
  scale_fill_manual(values = c( "gold1", "grey45")) +
  theme(panel.background = element_rect(fill = "gray85"), panel.grid.major = element_blank(), legend.title = element_blank()) 

#Preparing data for graph 2

dataColumns1 = c("Total Approved Loan Amount", "Approved Amount Real Estate","Approved Amount Content")
res_df1 <- ddply(df, groupcolumns, function(x) colSums(x[dataColumns1]))
res_df1 <- pivot_longer(res_df1, 'Approved Amount Real Estate':'Approved Amount Content', names_to = "State1", values_to = "Amount")
res_df1 <- res_df1 %>% mutate(Amount = Amount/1e6)
res_df1 <- res_df1 %>% mutate(`Total Approved Loan Amount` = `Total Approved Loan Amount`/1e6)
res_df1$`Total Approved Loan Amount` <- round(res_df1$`Total Approved Loan Amount` ,digit=1)

#plotting Graph 2

res_df1$zoom <- ifelse(res_df1$`Total Approved Loan Amount` < 3, TRUE, FALSE)
ggplot(res_df1, aes(x=reorder(State, -Amount), y= Amount, fill=State1, width=0.8)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text( vjust=0.6)) + 
  labs(title="Total Approved Loan Amount per State", subtitle = "Superstorm Sandy") +
  xlab("State") +
  ylab("Amount (In Millions)") +
  geom_col() + 
  facet_zoom(y= `Total Verified Loss`< 3,horizontal = FALSE, zoom.data = zoom("TRUE"), zoom.size = 0.5, show.area = FALSE, ylim = c(0,3)) + 
  geom_text(aes(State, `Total Approved Loan Amount`, label = `Total Approved Loan Amount`, fill = NULL, group = State), data = res_df1, position = position_nudge(y = 0.05)) +
  scale_fill_manual(values = c( "gold1", "grey45")) +
  theme(panel.background = element_rect(fill = "gray85"), panel.grid.major = element_blank(), legend.title = element_blank())


#Preparing data for graph 3

res_box <- pivot_longer(df, c('Total Verified Loss','Total Approved Loan Amount'), names_to = "Status", values_to = "Amount")


#plotting Graph 3

ggplot(res_box, aes(x=State, y=Amount, fill=Status )) +
  geom_boxplot(na.rm = TRUE) +
  ylim(c(0,100000)) +
  labs(title="Total Verified Loss vs Approved Loan Amount per State", subtitle = "Superstorm Sandy") +
  xlab("State") +
  ylab("Amount") +
  scale_fill_manual(values = c( "gold1", "grey45")) +
  theme(panel.background = element_rect(fill = "gray85"), panel.grid.major = element_blank(), legend.title = element_blank())

#Plotting graph 4

dataColumns2 = c("Total Approved Loan Amount","Total Verified Loss")
res_map <- ddply(df, groupcolumns, function(x) colSums(x[dataColumns2]))
res_map$percDiff <- apply(res_map[,c('Total Verified Loss','Total Approved Loan Amount')], 1, function(x) { (x[2]-x[1])/x[1] * 100 } )
colnames(res_map)[1] <- "abbr"
colnames(res_map)[4] <- "Amount"
res_map1 <- merge( res_map, statepop,by="abbr")

 
centroid_labels <- utils::read.csv(system.file("extdata", paste0("us_", "states", "_centroids.csv"), package = "usmap"), stringsAsFactors = FALSE)
res_map1 <- merge( res_map1, centroid_labels[c("x","y","abbr")],by.x ="abbr")
res_map1$Amount <- round(res_map1$Amount)

g + geom_text(data = res_map1, aes(x = x, y = y, label = paste0(abbr,"\n",Amount,"%")), color = "black", size=3) +
  labs(title="Percenatge Difference of Verified Loss vs Aproved Loan", subtitle = "Superstorm Sandy") 






