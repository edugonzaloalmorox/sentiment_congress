################################
# Get data from the Congress
################################

library(pdftools)
library(tidyverse)
library(tidytext)
library(readr)

# get the data


urls = sprintf("http://www.congreso.es/public_oficiales/L12/CONG/DS/PL/DSCD-12-PL-%s.PDF", rep(100:168))


raw_text <- map(urls, pdf_text)


my_data <- data_frame(document = urls, text = raw_text) %>%
  mutate(document = gsub("http://www.congreso.es/public_oficiales/L12/CONG/DS/PL/DSCD-12-PL-", "", document))


# clean and structure the data 

my_data_test = my_data %>% 
  unnest %>% # pdfs_text is a list
  group_by(document) %>%
  ungroup() %>%
  mutate(document = gsub("http://www.congreso.es/public_oficiales/L12/CONG/DS/PL/DSCD-12-PL-", "", document )) %>%
  mutate(page = row_number(), 
         date = str_extract(text, "celebrada el (.*)"),
         date = gsub("celebrada el", "", date),
         date = gsub("de", "", date),
         date = str_trim(date, "left"),
         sesion = str_extract(text, "Sesión plenaria núm. (.*)"),
         sesion = gsub("Sesión plenaria núm.", "", sesion),
         sesion= str_trim(sesion, "left"),
         inicio = str_extract(text, "Se abre la sesión a las (.*)"), 
         reanuda = str_extract(text, "Se reanuda la sesión a las (.*)"),
         inicio = ifelse(!is.na(reanuda), reanuda, inicio)) %>%
  select(-reanuda) %>%
  separate(date, into = c("weekday", "day", "month", "year"), sep = " ") %>%
  mutate(month = ifelse(!is.na(year), year, month), 
         month = str_to_upper(month),
         year = 2018) %>%
  unite("date_clean", c(day, month, year), sep = "-")  %>%
  mutate(date_clean = parse_date(date_clean,"%d-%B-%Y",locale=locale("es"))) %>%
  select(-weekday) 


my_data_test$date_clean = as.character(my_data_test$date_clean)

my_data_test = my_data_test %>%
  group_by(document) %>%
  fill(c(date_clean, sesion, inicio)) %>%
  mutate(session_type = str_detect(inicio, "Se abre la sesión"), 
         session_type = ifelse(session_type == TRUE, "inicio", "reanuda")) %>%
  select(-inicio) %>%
  filter(!is.na(session_type)) # get rid pages before inicio


# save data
write_csv(my_data_test, "./data/congress.csv")

write_csv(congress, "./data/congress.csv")


# descriptive  data 

library(lubridate)
library(hrbrthemes)

congress= congress %>%
  mutate(day_week = wday(date_clean, label = T), 
         month = month(date_clean, label = T))  %>%
  group_by(document) %>%
  mutate(total_pages = n()) 

# which days are the busiest in terms of activity

congress %>%
  group_by(document) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  group_by(day_week) %>%
  tally() %>%
  ggplot(., aes(day_week, n)) + 
  geom_col() + 
  scale_y_continuous(limits = c(0, 25), 
                     breaks = seq(0, 25, by = 3)) + 
  theme_ipsum(grid = "Y") + 
  labs(title = "# plenary meetings per day of the week", x= "",  y = "") 


# how is the monthly activity

  
congress %>%
  group_by(document) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  group_by(month) %>%
  tally() %>%
  mutate(Government = ifelse(month %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"), "PSOE", "PP")) %>%
  ggplot(., aes(month, n, fill = Government)) + 
  geom_col(alpha = 0.75) + 
  scale_y_continuous(limits = c(0, 10), 
                     breaks = seq(0, 10, by = 1)) + 
  scale_fill_manual(values = c("deepskyblue1", "firebrick2")) +
  theme_ipsum(grid = "Y") + 
  labs(title = "# plenary meetings per month", x= "",  y = "") 
  



# how long are the sessions (by number of pages in the pdf)

congress %>%
  group_by(document) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  group_by(month) %>%
  filter(month == "Aug")
  summarise(mean_page = mean(total_pages)) %>%
  mutate(Government = ifelse(month %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov"), "PSOE", "PP")) %>%
  ggplot(., aes(month, mean_page, fill = Government)) + 
  geom_col(alpha = 0.75) + 
  scale_fill_manual(values = c("deepskyblue1", "firebrick2")) +
  theme_ipsum(grid = "Y") + 
  labs(title = "# plenary meetings per month", x= "",  y = "") 

  
  





# clean information 


congress = read_csv("./data/congress.csv")









congress_long = congress %>% 
  unnest_tokens(word, text, to_lower = FALSE) %>%
  mutate(begin = ifelse(word == "SUMARIO", "start_session", NA)) %>%
  group_by(sesion) %>%
  fill(begin, .direction = "down") %>%
  filter(!is.na(begin), !word %in% c("DIARIO", "DE",  "SESIONES",  "DEL",  "CONGRESO",  "LOS",  "DIPUTADOS", 
"PLENO",  "Y", "DIPUTACIÓN PERMANENTE", "Pág", "Página", "cve", "DSCD", "PL", "señor", "señora"))


# dates 

library(lubridate)

congress_long = congress_long %>%
  mutate(day_week = wday(date_clean, label = T), 
         month = month(date_clean, label = T)) 










#interventions of the Presidenta 

congress_long = congress_long %>%
  mutate(intervention = ifelse("PRESIDENTA", "presidenta",
                               ifelse("RA")))



# stop words

devtools::install_github("quanteda/stopwords")
library(stopwords)

es_stop = stopwords::stopwords("es", source = "stopwords-iso")

es_stop = data.frame(word = es_stop)

congress_clean = congress_long %>% anti_join(es_stop, by = "word")


congress_clean %>%
  ungroup() %>%
  filter( !word %in% c("señor", "señora", "gracias", "Aplausos", "Por", "El", "La")) %>%
  count(word) %>%
  arrange(desc(n))


########################

library(rvest)
library(pdftools)

URL <- "http://www.congreso.es/portal/page/portal/Congreso/Congreso/Publicaciones/DiaSes/Pleno"

scraping_url <- read_html(URL)

members = scraping_url %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  as.data.frame()



dates = scraping_url %>%
  html_attr("<li>")  %>%
  as.data.frame()


%>%
  html_attr("href") %>%
  as.data.frame()



names(members) <- "url"


members_documents = members %>%
  filter(str_detect(url, "http://www.congreso.es/public_oficiales/L12/CONG/DS/PL/DSCD-12-PL-(.*)")) %>%
  as_tibble()

members_documents$url = as.character(members_documents$url)


raw_text <- map(members_documents$url, pdf_text)

my_data <- data_frame(document = members_documents$url, text = raw_text)  

# get the dates 

scraping_url %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  as.data.frame()




my_data_test = my_data %>% 
  unnest %>% # pdfs_text is a list
  group_by(document) %>%
  mutate(page = row_number())

  



  
