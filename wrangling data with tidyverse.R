library(tidyverse)  

library(forcats)
f1<-forcats::fct_relevel(factor(c("x","y","z","a","b")),"z","y","x",after = 1)
# 'after n': put n factors as were in last, those already factored
static_order <- fct_inorder(c("7","8","6"))
sort(static_order)

par1 <- par()
par(mar = c(5,15,4,2),las=2)
attach(chickwts)
m1 <- aov(weight ~ feed)
t1 <- TukeyHSD(m1)
plot(t1)           
lm1 <- lm(weight ~ feed)
broom::tidy(lm1)
broom::glance(lm1)
broom::augment(lm1)
#########OR USE BWPLOT
lattice::bwplot(data=chickwts, weight ~ reorder(feed,weight))
############ compare casein and sunflower
chick_sub <- subset(chickwts, feed %in% c("casein","sunflower"))
t.test(chick_sub$weight ~ chick_sub$feed ) ##no difference
#################### 
table(chickwts$feed)
library(janitor)
janitor::tabyl(chickwts$feed)

lvl_fact_by_freq<- forcats::fct_infreq(chickwts$feed) |> head()
lvl_fact_by_freq
lvl_fact_by_freq_rev <- forcats::fct_rev(lvl_fact_by_freq)
lvl_fact_by_freq_rev
#########################3

fct_reorder(feed,weight) # reorder by median of weight ascend
fct_reorder(feed,desc(weight))

chickwts |>
  mutate(new_fact = fct_reorder(feed,weight)) |>
  ggplot(aes(new_fact,weight)) + geom_boxplot()  #OR
chickwts |>ggplot(aes(fct_reorder(feed,weight),weight))+geom_boxplot()
# fct_reorder or reorder allows you to re-level a factor based on a secondary numeric variable
plotly::ggplotly()



