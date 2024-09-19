chickwts %>%
  mutate(wt_recode = ifelse(weight<=200,"low","hi"),
  
         wt_recode=factor(wt_recode)) 
  tabyl(wt_recode)
#################
  library(lubridate)
  
 my_date= mdy("september 29st 2020")
mdy_hms("july 20th 2020 20:20")  

wday(my_date, label=T)
year(my_date)
mday(my_date)
wday(my_date)
birth_day=make_date("1969","12","2")
wday(birth_day,label=T)
birth_day_w <- make_date("1984","4","8")
wday(birth_day_w,label=T)
p = (birth_day_w-birth_day)
lubridate::as.duration(p)
#######################################
library(tidyverse)
library(stringr)
str_c("a","b") #string combining
str_c("a","b",sep=" ")
A <- c(str_c(rep("abc",5),rep("cba",5),sep=" "))
str_length(A) # length of each string
str_sub(A,1,5) # from 1 to 5 of the each string sub
str_sub(A,-3,-1) #from right to left 3rd place -3 to last place -1
str_sort(c("c","a")) # all strings include
str_view(c("saima","sobia","any","palvisha"),"^s")
str_view(c("saima","sobia","any","palvisha"),"a$")
str_view(c("saima","sobia","any","palvisha","samna"),"ma$")
str_view(c("saima","sobia","any","palvisha","samna"),"m.a$")
  str_view(c("saima","sobia","any","palvisha","samna"),"mn?a$")
  str_count(c("sumaima","sobia","any","palvisha","samna"),"mn?a")
  str_detect(c("saima","sobia","any","palvisha","samna"),"mn?a$")
  str_subset(c("saima","sobia","any","palvisha","samna"),"mn?a$")
  str_extract(c("saima","sobia","any","palvisha","samna"),"mn?a$")
  str_replace(c("saima","sobia","maany","palvisha","samna"),"ma$","MA")
  
str_view_all(c("saima","sobia","any","palvisha","samna"),"[aeiou]")
str_view_all(c("saima2020","sobia","any","palvisha","samna"),"[^aeiou]")
str_view_all(c("saima 2020","sobia","any","palvisha","samna"),"\\d|\\s")
str_view_all(c("saima 2020","sobia","any","palvisha","samna"),".")

library(glue)

v1 <- "pre-history"
glue("my favourite topic is {v1}")
grade=80.2
glue("my grade adding ncc marks is {grade+2}")
############# TEST QUIZE
colors <- c('red','orange','yellow','green','blue','violet','#C8C8C8','#000000')
str_detect(colors,"\\d")
str_subset(colors,"[0-9]")
str_subset(colors,"^[a-z]")
str_count(colors,"[a-z]")
str_view_all(colors, "\\d+[A-Z]+") 
str_count(colors, '[a-zA-Z]') 
############################## Sentiment Analysis
library(tidytext)
carrots <- c("They say that carrots are good for your eyes",
             "They swear that they improve your sight",
             "But I'm seein' worse than I did last night -",
             "You think maybe I ain't usin' em right?")
library(tibble)
text_df <- tibble(line = 1:4, text = carrots)

text_df

a<-text_df %>%
  unnest_tokens(word,text) # long format i.e. tidytext and removes punctuations

get_sentiments(lexicon = "bing") # sentiments of words various lexicons
get_sentiments(lexicon= "nrc")

library(textdata)

text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc"))

text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc"))%>%
  count(sentiment, sort=T)
##################################### TF-IDF analysis
library(tibble)
invitation <- c("If you are a dreamer, come in,",
                "If you are a dreamer, a wisher, a liar", 
                "A hope-er, a pray-er, a magic bean buyer…",
                "If you’re a pretender, come sit by my fire",
                "For we have some flax-golden tales to spin.",
                "Come in!",
                "Come in!")

invitation <- tibble(line = 1:7, text = invitation, title = "Invitation")

masks <- c("She had blue skin.", 
           "And so did he.", 
           "He kept it hid", 
           "And so did she.", 
           "They searched for blue", 
           "Their whole life through",
           "Then passed right by—", 
           "And never knew")

masks <- tibble(line = 1:8, text = masks, title = "Masks")

masks

# add title to carrots poem
carrots <- text_df %>% mutate(title = "Carrots")

# combine all three poems into a tidy data frame
poems <- bind_rows(carrots, invitation, masks)

poem_words <- poems %>%
  tidytext::unnest_tokens(input = text, output=word) %>%
  count(title, word,sort=T)

total_words <- poem_words %>% 
  group_by(title) %>% 
  summarize(total = sum(n))

poem_words<- poem_words %>%
    left_join(total_words)
  # Note that there are a different number of total words in each document, 
  # which is important to consider when you’re comparing relative frequency between documents.

library(ggplot2)

poem_words %>%
  ggplot(aes(n/total, fill=title)) +
  geom_histogram(show.legend = F, bins=5)+
  facet_wrap(~title, scales="free_y")

freq_by_rank <- peom_words %>%
  group_by(title) %>%
  mutate(rank = row_number(),
         'term frequency' = (n/total))
# Rather, we’re interested in tf-idf - 
#   those words in a document that are unique relative to the other documents being analyzed.
poem_words<- poem_words %>%
  bind_tf_idf(document = title, n = n, term = word) %>%
  arrange(tf_idf) # e.g. 'for' comes equally in all titles hence 0 tf_idf

poem_words %>%
  bind_tf_idf(document = title, n = n, term = word) %>%
  arrange(desc(tf_idf))
# If we had removed stop words, we would have lost the fact that some common words 
# are really unique in one of these poems relative to the others.

poem_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, rev(unique(word)))) %>%
  group_by(title) %>%
  top_n(3) %>%
  ungroup() %>%
  ggplot(aes(word,tf_idf,  fill=title)) + 
    geom_col() +
  facet_wrap(~title,scales="free")+
  coord_flip()
#########################FUNCTIONAL PROGRAMMING
trees <- as_tibble(trees)
# create output vector
output <- vector("double", ncol(trees)) 

# loop through columns
for (i in seq_along(trees)) {          
  output[[i]] <- median(trees[[i]])      
}
output

# create function
col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- median(df[[i]])
  }
  output
}

# execute function
col_median(trees)
library(purrr)
map_dbl(trees, median)
map_dbl(trees, .f = mean)
map(trees, median)
map_dfr(trees, mean, na.rm = TRUE)
map2_dbl(trees$Girth, trees$Height, function(x,y){ pi * ((x/2)/12)^2 * y})
map2_dbl(trees$Girth, trees$Height, ~ pi * ((.x/2)/12)^2 * .y)

####################################EDA
library(here)
## here() starts at /Users/rdpeng/books/tidyversecourse

load(here::here("C://Users/Admin/OneDrive/PROGRAMMING/RSpace/2022-23/Project_coursera_reading_tidyverse_Case1/data/raw_data/case_study_1.rda"))
load(here::here("C://Users/Admin/OneDrive/PROGRAMMING/RSpace/2022-23/Proj_Coursera_tidyverse_Case2/data/raw_data/case_study_2.rda"))
#This loads all the data objects that we previously saved in our raw_data directory. Recall that this


####################
###############-----------------------------------------------------##########3333
#  Quiz 5
ed2 <- read_excel("data/raw_data/excel_data.xlsx",sheet=2)


ed2 |>
  select(X12) |>
  summarise(m=mean(X12)) #Q1 = -4.03
############################################3
ed1 <- read_excel("data/raw_data/excel_data.xlsx",sheet=1)

ed1 |>
  select(X5) |>
  cor(ed2$X8)  # Q2 = -0.0371
###############333####################3
library(RSQLite)
db <- RSQLite::dbConnect(SQLite(),dbname="data/raw_data/sqlite_data.db")
t1 <- dbReadTable(db,"Table1")

t1 |>
  
  filter(ID == 8  ) |>
  select(S2,S3) |>
  
  cor()
#Q3 = .219
###################4

library(jsonlite)
j2 <- fromJSON("data/raw_data/table2.json")
j2df <- as.data.frame(j2)

joined<-j2df |>
  inner_join(ed2, by='ID') 
summarize(m=mean(J2))
mean(joined$J2)  #Q4 6.93
#############################################3
cor(joined$X2,joined$J4) #Q5 .091

