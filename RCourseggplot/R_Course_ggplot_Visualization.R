library(tidyverse)

#####Making more barplots and manipulating more data in R

####Making a barplot of proportions

#####a toy demonstration
#####a bowl of fruit
apple <- rep("apple",6)
orange <- rep("orange",3)
banana <- rep("banana",1)

###put together the fruits in a dataframe
###creates a single columns with fruits
fruit_bowl <- tibble("fruits"=c(apple,orange,banana))

########Let's calculate proportions instead

#####create a table that counts fruits in a second column
fruit_bowl_summary <- fruit_bowl %>% 
  group_by(fruits) %>% 
  summarize("count"=n())

fruit_bowl_summary

####calculate proportions
fruit_bowl_summary$proportion <- fruit_bowl_summary$count/sum(fruit_bowl_summary$count)

fruit_bowl_summary

####add the geom_bar, using "stat" to tell command to plot the exact value for proportion
ggplot(fruit_bowl_summary,aes(x=fruits,y=proportion))+
  geom_bar(stat="identity")

ggplot(fruit_bowl_summary,aes(x=fruits,y=proportion,fill=fruits))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("red","yellow","orange"))+
  guides(fill=FALSE)+
  labs(x="Fruits",y="Proportion of Fruits")





df <- data.frame(x = 1, y = 3:1, family = c("sans", "serif", "mono"))
ggplot(df, aes(x, y)) + 
  geom_text(aes(label = family, family = family))
##########
label <- data.frame(
  waiting = c(55, 80), 
  eruptions = c(2, 4.3), 
  label = c("peak one", "peak two")
)

ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_tile(aes(fill = density)) + 
  geom_label(data = label, aes(label = label))


###########may use in GreenMetric --->
data(Oxboys, package = "nlme")
ggplot(Oxboys, aes(age, height, group = Subject)) + 
  geom_line() + 
  geom_point() + 
  gghighlight::gghighlight(Subject %in% 1:3)

library(ggrepel)
Green <- read_csv("d:/bizdatawizard/quarto/RCourseggplot/GreenMetric.csv")
Green <- Green[,-1]
long_Green <- reshape2::melt(Green, id.vars = c("Rank_2023", "University"), variable.name = "Category", value.name = "Value")
long_Green %>%
  ggplot(aes(x=Category,y=Value, group = University))+
  geom_line()+
  geom_point()+
  gghighlight::gghighlight(University %in% "University of Sindh Jamshoro",line_label_type = "ggrepel_text")
  
###########3
diamonds %>%
  select(carat,price,cut)%>%
  group_by(cut)%>%
  ggplot(aes(carat,log10(price)))+
  geom_point()+
  facet_wrap(~cut,nrow=1)

ggplot(diamonds, aes(log10(carat), log10(price))) + 
  geom_bin2d() + 
  facet_wrap(vars(cut), nrow = 1)


mod_coef <- coef(lm(log10(price) ~ log10(carat), data = diamonds))

ggplot(diamonds, aes(log10(carat), log10(price))) + 
  geom_bin2d() + 
  geom_abline(intercept = mod_coef[1], slope = mod_coef[2], 
              colour = "white", linewidth = 1) + 
  facet_wrap(vars(cut), nrow = 1)
#############
kids = LETTERS[1:5]
x_vals = seq(1,5,1)
scores = runif(5, 10, 50)

df1 <- tibble(kids,x_vals, scores)

df1 %>%
  ggplot(aes(x_vals, scores,label=kids))+
  geom_point()+
  geom_text(nudge_y = 2)
#############3
cel %>%
  filter(congress == 115) %>%
  ggplot(aes(dwnom1,all_pass,label=thomas_name))+
  geom_point()+
  geom_text(data = filter(cel,congress==115 & all_pass > 8),check_overlap = T)
  
cel %>%
  filter(congress == 115) %>%
  ggplot(aes(dwnom1,all_pass,label=thomas_name))+
  geom_point()+
  ggrepel::geom_text_repel(filter(cel,congress==115 & all_pass>8),mapping=aes(x=dwnom1,y=all_pass,label=thomas_name))+
  annotate("rect",xmin=.05,xmax=.4,ymin=13,ymax=15,alpha=.2,fill="red")+
  annotate("text",x=.6,y=14,label="Most Passed",color="red")

cel %>%
  filter(congress == 115) %>%
  ggplot(aes(dwnom1,all_pass,label=thomas_name))+
  geom_point()+
  ggrepel::geom_text_repel(mapping=aes(x=dwnom1,y=all_pass,label=thomas_name))+
  annotate("rect",xmin=.05,xmax=.4,ymin=13,ymax=15,alpha=.2,fill="red")+
  annotate("text",x=.6,y=14,label="Most Passed",color="red")
##########3333333333
library(RColorBrewer)
library(ggthemes)
cces <- read_csv(url("https://www.dropbox.com/s/ahmt12y39unicd2/cces_sample_coursera.csv?raw=1"))
#################3


library(ggplot2)
library(hrbrthemes)

# Dummy data
data <- data.frame(
  var1 = rnorm(100),
  var2 = rnorm(100, mean=2)
)

# Chart
ggplot(data) +
  # Top
  geom_density( aes(x = var1, y=..density..), fill="#69b3a2" ) +
  geom_label( aes(x=4.5, y=0.25, label="variable1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = var2, y=-..density..), fill= "#404080") +
  geom_label( aes(x=4.5, y=-0.25, label="variable2"), color="#404080") +
  ggthemes::theme_wsj() +
  xlab("value of x")

############

Oxboys %>%
  ggplot(aes(age,height,group=Subject))+
  geom_boxplot()+
  geom_smooth()
Oxboys %>%
  ggplot(aes(age,height))+
  geom_boxplot(aes(group=Subject))+
  geom_smooth()
Oxboys %>%
  ggplot(aes(as.integer(Subject),height))+
  geom_boxplot(fill="royalblue")+
  geom_smooth()
