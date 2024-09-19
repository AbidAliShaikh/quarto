library(xml2)
library(httr)
library(rvest)

r <- read_html("https://datatrail-jhu.github.io/stable_website/webscrape.html")
s <- html_nodes(r,"strong")
t <- html_text(s)
print(t)

r2 <- read_html("https://www.bbc.com")
s2 <- html_nodes(r2,".media__link") # can be tracked using extension named:s
t2 <- html_text(s2)
print(trimws(t2))
##################################################

gitresp <- GET("https://api.github.com/users/abidalishaikh/repos")
gitcontent <- content(gitresp)

lapply(gitcontent,function(x){
  df <- data_frame(repo = x$name,
                   address = x$html_url)}) %>%
  dplyr::bind_rows() 
##########################################

surv<- GET("https://raw.githubusercontent.com/fivethirtyeight/data/master/steak-survey/steak-risk-survey.csv")
df_surv <- content(surv,type="text/csv")
###### OR WE CAN ALSO USE READ_CSV

#downloading many files for offline scrapping
download.file("https://www.ibm.com/", destfile = "ibm.html")
root_nod1 <- rvest::read_html("ibm.html")

#root_html <- rvest::read_html(root_nod1,"html")
body_nod <- rvest::html_node(root_nod1,"body")
p_nod <- rvest::html_node(body_nod,"p")
p_content <- html_text(p_nod)
trimws(p_content)
########################
res <- read_html("Data required from departments-1.htm")
r_tab <- html_table(res)
...
#####################################################3
#READING AND WRITING IMAGES TO R
library(magick)
pic1 <- magick::image_read("textinpic.jpg")
pic1ocr <- magick::image_ocr(pic1)
cat(image_ocr(pic1))

##############CASE STUDY 1 HEALTH CARE DATA ##################
