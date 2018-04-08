library(httr)
library(xml2)



r <- GET("http://www.cninfo.com.cn/cninfo-new/information/companylist")

char_r <- content(r, "text")
tst <- xml2::read_html(r)
t <- xml_text(tst)
tt <- stringr::str_split(t, "\r\n")
tts <- tt[[1]][grepl("\t         \t   ", tt[[1]])]
ttss <- gsub("\t         \t   ", "", tts)

seccode <- gsub(" ", "", stringr::str_sub(ttss, 1, 6))



readr::write_csv(tibble::data_frame(seccode = paste0('s', as.character(seccode))), 
                 "/Users/JasonWang/Documents/Projects_R/Analy_Finan_Report/DATA/RAW/List_SECCODE.csv")


