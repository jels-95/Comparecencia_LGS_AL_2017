library(stringr)
library(tidyverse)

t1 <- readClipboard()

t2 <- t1 %>% tolower() %>% sapply(strsplit,split = ' ') %>% unlist()

t3 <- t2 %>% table

t4 <- t3 %>% as_data_frame

names(t4) <- c('Palabra','Cantidad')



quitar <- c('de','que','la','en','el','no','se','los','del','lo','las','para','por','con','es','usted','un',c('pero','este','eso', 'esa','ese','esto','una','como'),c('esta','así','son','fue','hay','ser','muy','qué','nos','han','más'),'hacer','está','sido','hace','sus','era','hubo','hecho','era')


t5 <- t4 %>% 
  rowwise() %>% 
  mutate(Palabra = str_replace_all(Palabra,"[[:punct:]0-9]","")) %>%
  ungroup() %>%
  group_by(Palabra) %>%
  summarise(Total = sum(Cantidad)) %>%
  filter(nchar(Palabra)>1) %>%
  arrange(desc(Total)) %>%
  filter(nchar(Palabra)>2) %>%
  filter(! (Palabra %in% quitar )) %>%
  mutate(Palabra = ifelse(Palabra == 'todos','todo',Palabra))

wordcloud(t5$Palabra[1:100],t5$Total[1:100])




