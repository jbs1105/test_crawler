library(rvest)
library(lubridate)
library(tibble)
library(plyr)
library(dplyr)

# tar <- "https://news.joins.com/politics/assemgov/list/1?filter=OnlyJoongang&date=2019-09-06"
  
# max_page <- function(tar_url) {
#   read_html(tar_url) %>% 
#     html_nodes("div.page a") %>% 
#     html_text() %>% 
#     regexpr("(^[0-9]*$)") %>% 
#     as.numeric() %>% 
#     max()
# }

max_page1 <- function(tar_url) {
  read_html(tar_url) %>% 
    html_nodes("div.page a") %>% 
    html_text() -> a
  a[which(regexpr("(^[0-9]*$)", a)==1)] %>% as.numeric() %>% max()
}

tar_url <- tar
tar <- "http://www.donga.com/news/List/Politics/CWD?ymd=20190110&m="
max <- max_page1(tar)
articles <- c()


i<-1
for (i in 1:max){
  tar_url <- paste0("http://www.donga.com/news/List/Politics/CWD?p=",(1+(i-1)*20),"&prod=news&ymd=20190110&m=")
  print(tar_url)
  read_html(tar_url) %>% 
    html_nodes("div.articleList a") %>% 
    html_attr("href") %>% 
    unique() -> link_list
  # subject, time, contents 
  j <-1
  for (j in 1:length(link_list)) {
    tar <- link_list[j]
    print(paste0("i=",i,"/j:",j," URL ", tar))
    news <- read_html(tar)
    news %>% 
      html_nodes("h1.title") %>% 
      html_text() -> title
    news %>% 
      html_nodes("span.date01") %>% 
      as.character() %>%
      strsplit('>', fixed=T) %>% 
      unlist() %>% 
      .[2] %>% 
      # grep('입력', ., value=T) %>% 
      gsub("입력|</span","", .) %>% 
      trimws() %>% 
      ymd_hm(tz = "Asia/Seoul") -> news_dt
      # ymd_hm(locale = "ko_KR.utf8")
    
    news %>% 
      html_nodes("span.report") %>% 
      html_text() -> report
    
    news %>% 
      html_nodes("div.article_txt") %>% 
      # html_text() %>% 
      as.character() %>% 
      strsplit("<br>|</div>|<div>") %>% 
      .[[1]] %>% 
      trimws() -> body_temp

    body_temp <- body_temp[-grep("<div|</span>|<script", body_temp)]
    body <- body_temp[nchar(body_temp) > 1]
    
    tem <- tibble(title, news_dt, report, body)
    articles %>% 
      bind_rows(tem) -> articles
  }
}

articles %>% distinct()

readr::write_excel_csv(articles, "test_run.csv")

