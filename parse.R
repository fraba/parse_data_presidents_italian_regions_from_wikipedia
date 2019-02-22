base_url = "https://it.wikipedia.org/wiki/Categoria:Presidenti_delle_regioni_italiane"

require(rvest)
require(stringr)
require(jsonlite)

it_months <- 
  c("gennaio|febbraio|marzo|aprile|maggio|giugno|luglio|agosto|settembre|ottobre|novembre|dicembre")

links <- 
  read_html(base_url) %>%
  html_nodes(xpath = "//*/div[@class='CategoryTreeItem']/a") %>% 
  html_attr('href')

links <- links[2:21]

dat <- data.frame()

for (link in links) {
  these_links <-
    read_html(paste0("https://it.wikipedia.org/", link)) %>%
    html_nodes(xpath = "//*/div[@class='mw-category-group']/ul/li/a") %>% 
    html_attr('href')
  these_links <- these_links[!grepl("Presidenti", these_links)]
  
  for (presidente_link in these_links) {
    
    print(presidente_link)
    
    presidente_text <- 
      fromJSON(
        sprintf('https://it.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&explaintext=1&titles=%s',
                gsub("/wiki/", "", presidente_link)))[[3]][[2]][[1]][[4]]
    
    presidente_page <-
      read_html(paste0("https://it.wikipedia.org/", presidente_link))
    
    wikidata_id <- 
      presidente_page %>%
      html_node(xpath = "//*/li[@id='t-wikibase']/a") %>%
      html_attr('href') %>%
      str_extract("Q[0-9]+$")
    
    position_held <- 
      presidente_page %>%
      html_nodes(xpath = "//*/table[@class='sinottico']/tbody/tr") %>%
      html_text()
    
    which_i <- which(grepl("^Presidente della( )?(Regione|Giunta regionale|Valle)", position_held, ignore.case = T))
    if (length(which_i) == 1) {
      
      position_label <- str_trim(position_held[which_i])
      position_time <- 
        presidente_page %>%
        html_node(xpath = 
                    sprintf("//*/table[@class='sinottico']/tbody/tr[%s]", which_i+1)) %>%
        html_text() %>%
        str_trim()
      
      if (position_time == "In carica") {
        position_time <- 
          presidente_page %>%
          html_node(xpath = 
                      sprintf("//*/table[@class='sinottico']/tbody/tr[%s]", which_i+2)) %>%
          html_text() %>%
          str_trim()
      }
      
      position_time <- 
        str_extract_all(position_time, 
                        sprintf("[0-9]{1,2}(º|°)? (%s) [0-9]{4}", it_months))[[1]]
      
      if (length(position_time) == 0) {
        position_time <- 
          presidente_page %>%
          html_node(xpath = 
                      sprintf("//*/table[@class='sinottico']/tbody/tr[%s]", which_i+1)) %>%
          html_text() %>%
          str_trim() %>%
          str_extract_all("[0-9]{4}")
        position_time <- position_time[[1]]
      }
      
      dat <- 
        rbind(dat, data.frame(link = presidente_link,
                              wikidata_id,
                              position_label,
                              from = position_time[1],
                              to = position_time[2],
                              wikipage = presidente_text))
      
    } else {
      dat <- 
        rbind(dat, data.frame(link = presidente_link,
                              wikidata_id,
                              position_label = NA,
                              from = NA,
                              to = NA,
                              wikipage = presidente_text))
    }
    
  }
}

save(dat, file = 'wikipedia_data.RData')

# Enrich
dat$condannato <- grepl("condannat(o|a)", dat$wikipage, ignore.case = T)

regioni <- 
  read.csv("~/public_git/parse_data_presidents_italian_regions_from_wikipedia/regioni.csv",
           stringsAsFactors = F)
dat$regione <- NA
for (i in 1:20) {
  which_i <- grepl(regioni$regex[i], dat$position_label, ignore.case = T)
  dat$regione[which_i] <- regioni$name[i]
}

dat$from_year <- str_extract(dat$from, "[0-9]{4}")
dat$to_year <- str_extract(dat$to, "[0-9]{4}")

dat$to_year[is.na(dat$to_year) & !is.na(dat$from_year)] <- 2019

# Missing
dat$from_year[dat$link == "/wiki/Alberto_Pacher"] <- 2013
dat$to_year[dat$link == "/wiki/Alberto_Pacher"] <- 2014
dat$regione[dat$link == "/wiki/Alberto_Pacher"] <- "Trentino-Alto Adige"

dat$from_year[dat$link == "/wiki/Ugo_Rossi"] <- 2014
dat$to_year[dat$link == "/wiki/Ugo_Rossi"] <- 2016
dat$regione[dat$link == "/wiki/Ugo_Rossi"] <- "Trentino-Alto Adige"

dat$to_year[dat$link == "/wiki/Piero_Marrazzo"] <- 2010

dat$to_year[dat$link == "/wiki/Ottaviano_Del_Turco"] <- 2009

dat$to_year[dat$link == "/wiki/Luciano_D%27Alfonso"] <- 2019

dat$to_year[dat$link == "/wiki/Renato_Soru"] <- 2009

require(plyr)
dat <- rbind.fill(dat,
                  data.frame(link = "/wiki/Luis_Durnwalder",
                             wikidata_id = "Q9379",
                             regione = "Trentino-Alto Adige",
                             from_year = 2004,
                             to_year = 2008,
                             condannato = FALSE))

dat <- rbind.fill(dat,
                  data.frame(link = "/wiki/Renzo_Tondo",
                             wikidata_id = "Q1241606",
                             regione = "Friuli-Venezia Giulia",
                             from_year = 2001,
                             to_year = 2003,
                             condannato = FALSE))

dat <- rbind.fill(dat,
                  data.frame(link = "/wiki/Angelo_Michele_Iorio",
                             wikidata_id = "Q3767139",
                             regione = "Molise",
                             from_year = 2001,
                             to_year = 2013,
                             condannato = TRUE))

dat <- rbind.fill(dat,
                  data.frame(link = "/wiki/Marcello_Veneziale",
                             wikidata_id = "Q3845699",
                             regione = "Molise",
                             from_year = 1999,
                             to_year = 2000,
                             condannato = FALSE))

dat <- rbind.fill(dat,
                  data.frame(link = "/wiki/Vincenzo_Leanza",
                             wikidata_id = "Q4013344",
                             regione = "Sicilia",
                             from_year = 2000,
                             to_year = 2001,
                             condannato = FALSE))

dat <- rbind.fill(dat,
                  data.frame(link = "/wiki/Mario_Floris",
                             wikidata_id = "Q952871",
                             regione = "Sardegna",
                             from_year = 1999,
                             to_year = 2001,
                             condannato = FALSE))

# Falsi positivi
dat$condannato[dat$link == "/wiki/Enrico_Rossi"] <- FALSE

write.table(dat[dat$to_year > 1994 & dat$condannato == TRUE,c("link","condannato_frasi")], 
            file = "descrizione_condanna.txt", sep = "\t",
            row.names = FALSE)

# Get Sentence
for (i in 1:nrow(dat)) {
  sentences <- unlist(strsplit(as.character(dat$wikipage[i]), split="\\."))
  res <- sentences[grep(paste('(C|c)ondannat(a|o)', collapse="|"),sentences)]
  dat$condannato_frasi[i] <- paste(res, collapse = "\n") 
}


long_dat <- data.frame()
for (regione in regioni$name) {
  this_subset <- dat[dat$regione %in% regione,]
  long_dat <-
    rbind(new_long_dat, 
          data.frame(regione,
                     year = 1994:2018,
                      stringsAsFactors = F))
}

long_dat$condannato <- NA
for (i in 1:nrow(long_dat)) {
  print(i)
  this_subset <- dat[dat$regione %in% long_dat$regione[i] &
                       dat$from_year <= long_dat$year[i] &
                       dat$to_year > long_dat$year[i],]
  if(nrow(this_subset) == 0) {
    next
  } else {
    long_dat$condannato[i] <- this_subset$condannato
  }
}

long_dat$regione <- factor(long_dat$regione, levels = rev(regioni$name), ordered = TRUE)

require(ggplot2)
ggplot(long_dat, aes(y = regione, x = year, fill = condannato)) +
  geom_tile(colour = 'gray') + 
  coord_equal() +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'bottom') +
  labs(x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Set2", labels = c("Si", "No", "Nessuna informazione")) +
  guides(fill=guide_legend(title="Presidente di regione successivamente condannato"))

  


