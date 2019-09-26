library(stringr)
library(rvest)
library(httr)
library(XML)
library(dplyr)


library(lubridate)
library(tibble)
library(plyr)
library(dplyr)

api_url = "https://openapi.naver.com/v1/search/blog.xml"
client_id = "Oif0UgcpTELCO5KpP8T4"
client_secret = "c5oaCbzeB7"

display_ = "&display=50"
start_ = "&start=1"
sort_ = "&sort=date" # sort=sim
startDate_ = "&startDate=2018-09-01"
endDate_ = "&endDate=2018-09-25"
orderBy_ = "&orderBy=sim" # sort=sim
l_max = 10


query = URLencode(iconv("서울숲", to="UTF-8"))
query = str_c("?query=", query)
ah <- paste0("X-Naver-Client-Id = ",client_id," X-Naver-Client-Secret = ", client_secret)

articles <- c()

result = GET(str_c(api_url, query, display_,sort_, startDate_, endDate_), 
             add_headers("X-Naver-Client-Id" = client_id, "X-Naver-Client-Secret" = client_secret))
xml_ = xmlParse(result)

title <- xpathSApply(xml_, "/rss/channel/item/title", xmlValue)
link <- xpathSApply(xml_, "/rss/channel/item/link", xmlValue)
des <- xpathSApply(xml_, "/rss/channel/item/description", xmlValue)
pdate <- xpathSApply(xml_, "/rss/channel/item/postdate", xmlValue)

pdate


https://blog.naver.com/youju78/221658860881
https://blog.naver.com/youju78?Redirect=Log&logNo=221658860881

https://blog.naver.com/syo2102/221658861416
https://blog.naver.com/syo2102?Redirect=Log&logNo=221658861416

tar <- "https://blog.naver.com/youju78/221658860881"

for (i in 1:l_max){
  tar <- as.character(link[i])
  print(paste0("i=",i," url: ",tar))
  blog <- read_html(tar)
  blog <- GET(tar, ah)
  blog %>% 
    html_nodes("div.blog2_container span.se_publishDate.pcol2")

    /html/body/div[6]/div[1]/div[2]/div[2]/div[2]/div[1]/div/div/div[11]/div[1]/div/table[2]/tbody/tr/td[2]/div[1]/div/div/div[1]/div/div/div[3]/span[2]

  #SE-f04650dd-719e-4347-84c0-946367bf07a0 > div > div > div.blog2_container > span.se_publishDate.pcol2
  
  
}


tem <- tibble(title, link, des, pdate)
articles %>% 
  bind_rows(tem) -> articles


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



# ## 네이버 뉴스에서 원하는 키워드의 검색 결과를 웹크롤링(스크래핑)하는 코드
# ## 제작: hkim (dr-hkim.github.io)
# 
# ## 패키지 불러오기
# library(rvest)
# library(dplyr)
# 
# ## 변수 입력하기
# QUERY <- "미국+기준금리" # 검색키워드
# DATE  <- as.Date(as.character(20181119),format="%Y%m%d") # 검색시작날짜 & 검색종료날짜
# DATE <- format(DATE, "%Y.%m.%d")
# PAGE  <- 1
# 
# naver_url_1 <- "https://search.naver.com/search.naver?&where=news&query="
# naver_url_2 <- "&pd=3&ds="
# naver_url_3 <- "&de="
# naver_url_4 <- "&start="
# 
# ## 날짜 리스트 만들기
# DATE_START <- as.Date(as.character(20181119),format="%Y%m%d") # 시작일자
# DATE_END   <- as.Date(as.character(20181121),  format="%Y%m%d") # 종료일자
# DATE <- DATE_START:DATE_END
# DATE <- as.Date(DATE,origin="1970-01-01")
# 
# ## 게시물 번호 리스트 만들기
# PAGE <- seq(from=1,to=41,by=10) # 시작값과 종료값을 지정해줄 수 있습니다.
# PAGE <- seq(from=1,by=10,length.out=5) # 시작값과 원하는 갯수를 지정할 수도 있습니다.
# 
# ## 네이버 검색결과 url 리스트에서 관련기사 url 리스트 만들기
# news_url <- c()
# news_date <-c() 
# 
# for (date_i in DATE){
#   for (page_i in PAGE){
#     dt <- format(as.Date(date_i,origin="1970-01-01"), "%Y.%m.%d")
#     naver_url <- paste0(naver_url_1,QUERY,naver_url_2,dt,naver_url_3,dt,naver_url_4,page_i)
#     html <- read_html(naver_url)
#     temp <- unique(html_nodes(html,'#main_pack')%>% # id= 는 # 을 붙인다
#                      html_nodes(css='.news ')%>%    # class= 는 css= 를 붙인다 
#                      html_nodes(css='.type01')%>%
#                      html_nodes('a')%>%
#                      html_attr('href'))
#     news_url <- c(news_url,temp)
#     news_date <- c(news_date,rep(dt,length(temp)))
#   }
#   print(dt) # 진행상황을 알기 위함이니 속도가 느려지면 제외
# }
# 
# NEWS0 <- as.data.frame(cbind(date=news_date, url=news_url, query=QUERY))
# NEWS1 <- NEWS0[which(grepl("news.naver.com",NEWS0$url)),]         # 네이버뉴스(news.naver.com)만 대상으로 한다   
# NEWS1 <- NEWS1[which(!grepl("sports.news.naver.com",NEWS1$url)),] # 스포츠뉴스(sports.news.naver.com)는 제외한다  
# NEWS2 <- NEWS1[!duplicated(NEWS1), ] # 중복된 링크 제거 
# 
# 
# ## 뉴스 페이지에 있는 기사의 제목과 본문을 크롤링
# NEWS2$news_title   <- ""
# NEWS2$news_content <- ""
# 
# for (i in 1:dim(NEWS2)[1]){
#   html <- read_html(as.character(NEWS2$url[i]))
#   temp_news_title   <- repair_encoding(html_text(html_nodes(html,'#articleTitle')),from = 'utf-8')
#   temp_news_content <- repair_encoding(html_text(html_nodes(html,'#articleBodyContents')),from = 'utf-8')
#   if (length(temp_news_title)>0){
#     NEWS2$news_title[i]   <- temp_news_title
#     NEWS2$news_content[i] <- temp_news_content
#   }
# }
# 
# NEWS2$news_content <- gsub("// flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "", NEWS2$news_content)
# NEWS <- NEWS2 # 최종 결과 저장
# 
# save(NEWS, file="./DATA0/NEWS.RData") # working directory 아래 subfolder "DATA0" 에 저장




