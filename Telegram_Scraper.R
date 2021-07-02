###TELEGRAM SCRAPER###

library("RSelenium")
driver <- rsDriver(browser=c("firefox"), port=6023L)
remote_driver <- driver[["client"]]

remote_driver$navigate("https://t.me/s/spiegelonline") 

# unroll page

replicate(10, # How often should the scraper scroll up
          {
            # scroll up
            webElem <- remote_driver$findElement("css", "body")
            webElem$sendKeysToElement(list(key = "home"))
            # wait
            Sys.sleep(4)
          })

# scrape complete posts (text incl. meta infos)

texts <- unlist(texts)

texts_el <- remote_driver$findElements(using = 'xpath', value = "//div[@class='tgme_widget_message_bubble']")

texts<- list()
for (i in 1:length(texts_el)) {
  tryCatch({
    texts[i] <- texts_el[[i]]$getElementText()
  }, 
  error=function(e){})
}

texts <- unlist(texts)

library(stringr)

#extract user
sender <- sub("\\n.*", "", texts)

#extract clean text
text_clean <- sub(".*?\\n",'',texts)

# extract views
views <- c()
for (i in 1:length(texts)){
  views[i] <- str_match(texts[i], "\\n\\s*(.*?)\\s*\\nviews")[2]
}
views <- gsub("[[:punct:]]", "", views)
views <- as.numeric(gsub("K", "00", views))

# scrape dates

date_el <- remote_driver$findElements(using = 'xpath', value = "//time[@class='time']")

dates<- list()
for (i in 1:length(date_el)) {
  tryCatch({
    dates[i] <- date_el[[i]]$getElementAttribute("datetime")
  }, 
  error=function(e){})
}

dates <- unlist(dates)
dates <- as.Date(substr(dates, 1, 10), format="%Y-%m-%d") 

# extract #/Hashtags, @/Mentions, http/Links

hashtags <- str_extract_all(text_clean, "#\\S+") %>% lapply(function(x) if(identical(x, character(0))) NA_character_ else x) %>%
  lapply(paste, collapse=",") %>% unlist()

mentions <- str_extract_all(text_clean, "@\\S+") %>% lapply(function(x) if(identical(x, character(0))) NA_character_ else x) %>%
  lapply(paste, collapse=",") %>% unlist()

links <- str_extract_all(text_clean, "http\\S+") %>% lapply(function(x) if(identical(x, character(0))) NA_character_ else x) %>%
  lapply(paste, collapse=",") %>% unlist()

df <- data.frame("user"= sender, "text"= text_clean, dates, views, hashtags, mentions, links)

# remove channel notifications

df <- df[!df$text %in% grep(paste0("pinned", collapse = "|"), df$text, value = T),]
df <- df[!df$text %in% grep(paste0("Channel photo updated", collapse = "|"), df$text, value = T),]
df <- df[!df$text %in% grep(paste0("Channel created", collapse = "|"), df$text, value = T),]

df$text <- gsub('.{17}$', '', df$text)

# forwarded from

df$Forwarded <- grepl("Forwarded from \n", df$text, fixed = TRUE)

df$Forwarded_from <- c()

for (i in 1:nrow(df)){
  if (df$Forwarded[i]==T) {
  df$Forwarded_from[i] <- str_extract(sub('.*Forwarded from \n', '', df$text[i]), "[^\n]+")
} else {
  df$Forwarded_from[i] <- NA
}
}


