---
title: "RWorksheet#5"
author: "Barrientos, Delfin, Infiesto"
date: "2024-11-06"
output: pdf_document
---


``` r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
library(rvest)
library(httr)
library(polite)

library(kableExtra)
```

```
## 
## Attaching package: 'kableExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     group_rows
```

``` r
polite::use_manners(save_as = 'polite_scrape.R')
```

```
## v Setting active project to "C:/PROJ/RAnalytics".
```

``` r
url <- 'https://www.imdb.com/chart/toptv/?ref_=nv_tvv_250'

session <- bow(url,
               user_agent = "Educational")
session
```

```
## <polite session> https://www.imdb.com/chart/toptv/?ref_=nv_tvv_250
##     User-agent: Educational
##     robots.txt: 35 rules are defined for 3 bots
##    Crawl delay: 5 sec
##   The path is scrapable for this user-agent
```

``` r
rank_title <- character(0)
links <- character(0)

title_list <- scrape(session) %>%
  html_nodes('h3.ipc-title__text') %>% 
  html_text

class(title_list)
```

```
## [1] "character"
```

``` r
title_list_sub <- as.data.frame(title_list[2:51])

head(title_list_sub)
```

```
##      title_list[2:51]
## 1     1. Breaking Bad
## 2  2. Planet Earth II
## 3     3. Planet Earth
## 4 4. Band of Brothers
## 5        5. Chernobyl
## 6         6. The Wire
```

``` r
tail(title_list_sub)
```

```
##    title_list[2:51]
## 45             <NA>
## 46             <NA>
## 47             <NA>
## 48             <NA>
## 49             <NA>
## 50             <NA>
```

``` r
colnames(title_list_sub) <- "ranks"

split_df <- strsplit(as.character(title_list_sub$ranks),".",fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))

split_df <- split_df[-c(3:4)] 

colnames(split_df) <- c("ranks","title") 

str(split_df) 
```

```
## 'data.frame':	50 obs. of  2 variables:
##  $ ranks: chr  "1" "2" "3" "4" ...
##  $ title: chr  " Breaking Bad" " Planet Earth II" " Planet Earth" " Band of Brothers" ...
```

``` r
class(split_df)
```

```
## [1] "data.frame"
```

``` r
head(split_df)
```

```
##   ranks             title
## 1     1      Breaking Bad
## 2     2   Planet Earth II
## 3     3      Planet Earth
## 4     4  Band of Brothers
## 5     5         Chernobyl
## 6     6          The Wire
```

``` r
rank_title <- data.frame(
  rank_title = split_df)

write.csv(rank_title,file = "title.csv")

 link_list <- scrape(session) %>%
  html_nodes('a.ipc-title-link-wrapper') %>% 
  html_attr('href') 
 
 head(link_list)
```

```
## [1] "/title/tt0903747/?ref_=chttvtp_t_1" "/title/tt5491994/?ref_=chttvtp_t_2"
## [3] "/title/tt0795176/?ref_=chttvtp_t_3" "/title/tt0185906/?ref_=chttvtp_t_4"
## [5] "/title/tt7366338/?ref_=chttvtp_t_5" "/title/tt0306414/?ref_=chttvtp_t_6"
```

``` r
 link_list[245:257]
```

```
##  [1] NA NA NA NA NA NA NA NA NA NA NA NA NA
```

``` r
 link <- as.vector(link_list[1:50])
names(link) <- "links"

head(link)
```

```
##                                links                                 <NA> 
## "/title/tt0903747/?ref_=chttvtp_t_1" "/title/tt5491994/?ref_=chttvtp_t_2" 
##                                 <NA>                                 <NA> 
## "/title/tt0795176/?ref_=chttvtp_t_3" "/title/tt0185906/?ref_=chttvtp_t_4" 
##                                 <NA>                                 <NA> 
## "/title/tt7366338/?ref_=chttvtp_t_5" "/title/tt0306414/?ref_=chttvtp_t_6"
```

``` r
tail(link)
```

```
## <NA> <NA> <NA> <NA> <NA> <NA> 
##   NA   NA   NA   NA   NA   NA
```

``` r
for (i in 1:250) {
  link[i] <- paste0("https://imdb.com", link[i], sep = "")
}

links <- as.data.frame(link)

rank_title <- data.frame(
  rank_title = split_df, link)

#2

library(rvest)

selected_links <- c(
  "https://www.imdb.com/title/tt5491994/",
  "https://www.imdb.com/title/tt2861424/",
  "https://www.imdb.com/title/tt1475582/",
  "https://www.imdb.com/title/tt1831164/",
  "https://www.imdb.com/title/tt0944947/"
)

scrape_reviews <- function(url) {
  page <- read_html(url)
  
  reviewer_name <- page %>%
    html_nodes(".display-name-date span") %>%
    html_text()
  
  review_date <- page %>%
    html_nodes(".display-name-date span:nth-child(2)") %>%
    html_text()
  
  user_rating <- page %>%
    html_nodes(".ipl-ratings-bar .ipl-rating-star__rating") %>%
    html_text() %>%
    as.numeric()
  
  review_title <- page %>%
    html_nodes(".title") %>%
    html_text()
  
  review_text <- page %>%
    html_nodes(".text.show-more__control") %>%
    html_text()
  
  reviews <- data.frame(
    Reviewer = reviewer_name,
    Date = review_date,
    Rating = user_rating,
    Title = review_title,
    Text = review_text
  )
  
  return(reviews)
}

all_reviews <- list()

for (link in selected_links) {
  show_reviews <- scrape_reviews(link)
  all_reviews[[link]] <- show_reviews
}
```



