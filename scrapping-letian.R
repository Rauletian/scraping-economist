library(rvest)
library(stringr)
library(tidyverse)
library(sentimentr)
library(tidytext)
read_csv("R/country-keyword-list.csv",col_names = FALSE)->country_list
country_list$X1->vector_country
url<-'https://www.economist.com/'
read_html(url)->webpg

headlines<-webpg%>%
  html_nodes('a.headline-link')%>%
  html_text()
urls<-webpg%>%
  html_nodes('a.headline-link')%>%
  html_attr('href')


HeadingUrlDF<-data.frame(heading=headlines,
                         url=urls)

paste('https://www.economist.com',HeadingUrlDF$url,sep="")->HeadingUrlDF$url

str_match(HeadingUrlDF$url,'^https://www.economist.com/')->proof

HeadingUrlDF%>%
   filter(is.na(proof)!=TRUE)->HeadingUrlDF

cbined<-tibble(x='first',y='first',
               z='first',a='first')
heading<-c()
body<-c()
for (i in HeadingUrlDF$url){
   
   download.file(i, destfile = "scrapedpage.html", quiet=TRUE)
   a<-read_html(i)
   
   a%>%
   html_nodes('.article__body-text--dropcap,.article__body-text')%>%
   html_text()->body_econo
   
   body<-append(body,body_econo)
   
   
   a%>%
   html_nodes('.article__headline ')%>%
   html_text()->title
   
   heading<-append(heading,title)
   a%>%
      html_nodes('.article__dateline-datetime')%>%
      html_text()->dateTime
   
   
   cbined%>%
      add_row(x=title,y=body_econo,z=i,
              a=dateTime)->cbined
}
cbined
cbined[-1,]->cbined
cbined%>%
   separate(z,into=c('former','section'),sep='https://www.economist.com/',
            remove = FALSE)%>%
   select(-former)->cbined

cbined%>%
   separate(section, into=c('section_key','rest'), sep='/')%>%
   select(-rest)->cbined



sum(sentiment(heading))/length(heading)->scoreHeading
sentiment(body)->sentimentBodyWord
sum(sentimentBodyWord$sentiment)/length(sentimentBodyWord$sentiment)->bodyScore



cbined%>%
   unnest_tokens(word,y)->DFwords

get_sentiments('bing')->bing

DFwords%>%
   inner_join(bing)->DFwords
DFwords

path='Users/Raúl/Documents/R'
write_csv(cbined, 
          path,
          append =TRUE)