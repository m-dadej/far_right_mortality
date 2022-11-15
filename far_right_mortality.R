library(tidyverse)
library(modelr)
library(ggforce)
library(hrbrthemes)
library(MetBrewer)
library(lubridate)
library(readxl)
library(bdl)
library(sandwich)
library(lmtest)
library(dplyr)
library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(broom.mixed)
library(sf)
library(MetBrewer)


age_dict <- bdl::get_variables(subjectId = "P2137") %>%
  filter(n2 == "ogółem" & n1 %in% c("0-14", "15-19", "25-29", "35-39", "40-44" ,"50-54",
                                    "30-34","45-49",  "55-59","20-24" , "60-64","65-69", 
                                    "70 i więcej"))

pop_age <- bdl::get_data_by_variable(varId = age_dict$id, unitLevel = 5, year = 2021)

powiaty_kod <- read_excel("kod_powiaty.xlsx", col_names = c("kod_regionu", "nazwa")) %>%
  mutate(powiat = tolower(nazwa))


#bdl::get_variables("P2425")
density <- bdl::get_data_by_variable(varId = 60559, unitLevel = 5, year = 2021)

density <- select(density, "powiat" = name, "year_density" = year, "density" = val) %>%
  mutate(powiat = str_remove_all(powiat, "Powiat "),
         powiat = tolower(str_replace_all(powiat, "m\\.", "Miasto "))) %>%
  filter(year_density == "2021")

# bdl::get_variables("P2430")
# 410600: mieszkania na 1000 mieszkańców

housing <- bdl::get_data_by_variable("410600", unitLevel = 5, year = 2020) 

housing <- select(housing, "powiat" = name, "housing" = val) %>%
            mutate(powiat = str_remove_all(powiat, "Powiat "),
                   powiat = tolower(str_replace_all(powiat, "m\\.", "Miasto ")))

#bdl::get_variables("P1601") 
# ludność korzystająca z  sieci kanalizacyjnej

sewage <- bdl::get_data_by_variable("9153", unitLevel = 5, year = 2021)

sewage <- select(sewage, "powiat" = name, "sewage" = val) %>%
  mutate(powiat = str_remove_all(powiat, "Powiat "),
         powiat = tolower(str_replace_all(powiat, "m\\.", "Miasto ")))


#bdl::get_variables("P3173")
#454186: lekarze pracujący wg podstawowego miejsca pracy na 10 tys. ludności
docs <- bdl::get_data_by_variable("454186", unitLevel = 5, year = 2020)

docs <- select(docs, "powiat" = name, "docs" = val) %>%
          mutate(powiat = str_remove_all(powiat, "Powiat "),
                 powiat = tolower(str_replace_all(powiat, "m\\.", "Miasto ")))


# tys. osob z wyksztalceniem wyzszym
# bdl::get_variables("P4215")

educ_census <- bdl::get_data_by_variable("1640891", unitLevel = 5, year = 2021)

educ_census <- select(educ_census, "powiat" = name, "educ_census" = val) %>%
  mutate(powiat = str_remove_all(powiat, "Powiat "),
         powiat = tolower(str_replace_all(powiat, "m\\.", "Miasto ")))


# bdl::get_variables("P2392")
# bezrobocie rejestrowane ogółem

unemp <- bdl::get_data_by_variable("60270", unitLevel = 5, year = 2020)

unemp <- select(unemp, "powiat" = name, "unemp" = val) %>%
          mutate(powiat = str_remove_all(powiat, "Powiat "),
                 powiat = tolower(str_replace_all(powiat, "m\\.", "Miasto "))) 

deaths_id <- bdl::get_variables(subjectId = "P1343") %>%
  filter(n1 == "ogółem" & n2 %in% c("0","1-4", "5-9","10-14", "15-19", "25-29", "35-39", "40-44" ,"50-54",
                                    "30-34","45-49",  "55-59","20-24" , "60-64","65-69", 
                                    "70 i więcej"))

deaths <- bdl::get_data_by_variable(varId = deaths_id$id, unitLevel = 5, year = 2010:2021)

deaths_bdl <- select(deaths, "powiat" = name, contains(c("year", "val"))) %>%
  pivot_longer(cols = -c(powiat, year)) %>%
  mutate(id = as.numeric(str_remove_all(name, "val_")),
         year = as.numeric(year),
         powiat = str_remove_all(powiat, "Powiat "),
         powiat = tolower(str_replace_all(powiat, "m\\.", "Miasto "))) %>%
  full_join(deaths_id, by = "id") %>%
  select(powiat, year, value, n2) %>%
  group_by(year, powiat) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(powiat) %>%
  nest() %>%
  mutate(models = map(data, \(x){lm(value ~ year, data = filter(x, year < 2020))}),
         expected_deaths = map2(data, models, modelr::add_predictions)) %>%
  unnest(c(expected_deaths)) %>%
  ungroup() %>%
  select(-c(data, models)) %>%
  filter(year > 2019) %>%
  mutate(year = paste0("covid_", year),
         covid = value - pred,
         covid_rel = value / pred - 1) %>%
  group_by(powiat) %>%
  summarise(covid = sum(covid),
            covid_rel = mean(covid_rel)) %>%
  ungroup() 

sum(deaths_bdl$covid)

votes_pierwsza <- read_excel("votes_powiaty1.xlsx")

votes_df <- select(votes_pierwsza, "powiat" = Powiat, 
                   "duda" = `Andrzej Sebastian DUDA`,
                   "total_votes" = `Liczba wyborców, którym wydano karty do głosowania`,
                   "bosak" = `Krzysztof BOSAK`,
                   "woj" = Województwo) %>%
  mutate(duda_share = duda / total_votes,
         bosak_share = bosak / total_votes,
         powiat = tolower(powiat))

lotniska <- c("miasto  st. warszawa", "miasto kraków", "miasto katowice", "miasto gdańsk", "miasto wrocław", "miasto poznań", "miasto rzeszów", "miasto szczecin", "miasto lublin", "miasto bydgoszcz", "miasto łódź", "miasto olsztyn", "miasto zielona góra", "miasto radom")

model_df <- select(pop_age, "powiat" = name, contains(c("year", "val"))) %>%
  pivot_longer(cols = -c(powiat, year)) %>%
  mutate(id = as.numeric(str_remove_all(name, "val_"))) %>%
  full_join(age_dict, by = "id") %>%
  select(powiat, year, value, n1) %>%
  filter(year == "2021") %>%
  group_by(powiat) %>%
  slice(1:13) %>% # because there are duplicated powiat's without differencing vars we take first duplicate                mutate(total_pop = sum(value)) %>%
  mutate(total_pop = sum(value)) %>%
  mutate(age_share = value / total_pop,
         powiat = str_remove_all(powiat, "Powiat "),
         powiat = tolower(str_replace_all(powiat, "m\\.", "Miasto "))) %>%
  ungroup() %>%
  select(-value) %>%
  pivot_wider(names_from = n1, values_from = age_share) %>%
  left_join(deaths_bdl, by = "powiat") %>%
  drop_na() %>%
  rename("age_year" = year) %>%
  mutate(lotnisko = powiat %in% lotniska) %>%
  left_join(density, by = "powiat") %>%
  left_join(docs, by = "powiat") %>%
  left_join(sewage, by = "powiat") %>%
  left_join(unemp, by = "powiat") %>%
  left_join(educ_census, by = "powiat") %>%
  mutate(powiat = str_remove_all(powiat, "miasto "),
         powiat = str_remove_all(powiat, "st. "),
         powiat = str_remove_all(powiat, " od 2013"),
         powiat = ifelse(powiat == " warszawa", "warszawa", powiat),
         powiat = ifelse(powiat == "karkonoski", "jeleniogórski", powiat),
         sewage = sewage / total_pop,
         covid = round(covid / total_pop * 10000)) %>%
#  left_join(vacc, by = "powiat") %>%
  full_join(votes_df, by = "powiat") %>%
  distinct_at("powiat", .keep_all = TRUE)


# spatial features --------------------

load("bdl.maps.2021.Rdata")

nbrs <- select(bdl.maps.2021$level5, geometry, powiat = name) %>%
  as_tibble() %>%
  mutate(powiat = str_remove_all(powiat, "Powiat "),
         powiat = tolower(str_replace_all(powiat, "m\\.", "Miasto ")),
         powiat = str_remove_all(powiat, "miasto "),
         powiat = str_remove_all(powiat, " od 2013"),
         powiat = ifelse(powiat == " warszawa", "warszawa", powiat),
         powiat = ifelse(powiat == "karkonoski", "jeleniogórski", powiat)) %>%
  left_join(model_df, by = "powiat") %>%
  distinct_at("powiat", .keep_all = TRUE) %>%
  select(geometry, covid, powiat) %>%
  mutate(nbrs_covid = NA,
         geometry  = sf::st_centroid(geometry))

for (i in 1:nrow(nbrs)) {
  
  nbrs[i, "nbrs_covid"] <- mutate(nbrs, dist = st_distance(geometry, nbrs$geometry[i])) %>%
    arrange(dist) %>%
    .[2:5,] %>%
    .$covid %>%
    mean(na.rm = TRUE)
}

model_df <- distinct_at(model_df, "powiat", .keep_all = TRUE)  %>%
              left_join(select(nbrs, powiat, nbrs_covid), by = "powiat")


# graphs  ------------

map_gg <- bdl.maps.2021$level5 %>%
  as_tibble() %>%
  mutate(powiat = str_remove_all(name, "Powiat "),
         powiat = tolower(str_replace_all(powiat, "m\\.", "")),
         powiat = str_remove_all(powiat, "st. "),
         powiat = str_remove_all(powiat, " od 2013"),
         powiat = ifelse(powiat == " warszawa", "warszawa", powiat),
         powiat = ifelse(powiat == "karkonoski", "jeleniogórski", powiat)) %>%
  left_join(model_df, by = "powiat") %>%
  mutate(young = `0-14` + `15-19`,
         adult = `20-24` + `25-29` + `30-34` ,
         suscep = (`35-39` + `40-44` + `45-49` + `50-54`) * total_pop,
         old = (`60-64` + `65-69` + `70 i więcej`) * 100,
         elder = `65-69` + `70 i więcej`,
         covid = ifelse(powiat == "wałbrzyski", NA, covid),
         sewage = ifelse(sewage >= 1, 1, sewage)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = covid)) +
  scale_fill_viridis_c(option = "D", direction = 1) +
  theme_void() +
  theme(legend.position = "top",
        plot.margin = margin(c(5, 5, 5, 5))) +
  labs(fill = "COVID-19 excess deaths \nper 10,000 in Poland")

ggsave(filename = "map_gg.png", plot = map_gg, width = 7, height =7)

summarise(deaths_bdl, sum(covid))

# bayesian modeling ----------------

model_bayes_df <- mutate(model_df, young = `0-14` + `15-19`,
                       adult = (`20-24` + `25-29` + `30-34` + `35-39` + `40-44`) * 100,
                       suscep = (`35-39` + `40-44`) * total_pop,
                       old = (`50-54` + `45-49` + `55-59` ) * 100,
                       elder = (`65-69` + `70 i więcej`) * 100,
                       bosak_share = bosak_share * 100,
                       duda_share = duda_share * 100,
                       sewage = sewage * 100) %>%
  filter(powiat != "wałbrzyski") %>%
    mutate(duda_more = duda_share > median(duda_share, na.rm = TRUE),
         bosak_more = bosak_share > median(bosak_share, na.rm = TRUE))

select(model_bayes_df, 'Konfederacja' = bosak_share, 
       covid, elder,nbrs_covid , docs, lotnisko , density , sewage , unemp, educ_census) %>%
  mutate(id = 1:nrow(.), density = log(density)) %>%
  drop_na() %>%
  pivot_longer(cols = -id) %>%
  group_by(name) %>%
  drop_na() %>%
  summarise(min = min(value),
            `1st Quantile` = quantile(value, 0.25),
            median = median(value),
            average = mean(value),
            `3rd Quantile` = quantile(value, 0.75),
            max = max(value),
            `standard dev.` = sd(value))

model_bayes_df$powiat

negbin_sim <- stan_glm(
  covid ~ bosak_share + elder + nbrs_covid + docs + lotnisko + density + sewage + unemp + educ_census, 
  data = drop_na(model_bayes_df), family = neg_binomial_2,
  prior_intercept = normal(0, 2, autoscale = TRUE),
  prior = normal(0, 0.2, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4,iter = 5000*2, seed = 0)


tidy(negbin_sim, conf.int = TRUE, conf.level = 0.95) 

model_vars <- select(model_bayes_df,covid , bosak_share, elder ,nbrs_covid , docs , lotnisko ,density ,sewage , unemp ,educ_census)

model_vars %>%
  drop_na() %>%
posterior_predict(negbin_sim, ., draws = 10000,seed = 1) %>%
  apply(2, mean) %>%
  as.numeric() %>%
  data.frame(pred = .,
             actual = select(drop_na(model_vars), covid)) %>%
  summarise(cor(pred, covid)^2,
            mae = mean(abs(pred - covid)),
            alt_mae = mean(abs(mean(covid) - covid)),
            sd_covid = sd(covid))

a <- plot(negbin_sim, plotfun = "areas", prob = 0.95,
       pars = c("bosak_share", "elder")) +
  theme_ipsum_es() +
  scale_y_discrete(labels = c( '% vote for Konfederacja', "% of 65 or above")) +
  theme(plot.margin = margin(c(10,10,10,10)))
  

b <- plot(negbin_sim, plotfun = "areas", prob = 0.95, pars = c("nbrs_covid", "sewage", "unemp")) +
          theme_ipsum_es() +
          scale_y_discrete(labels = c("Mean of neighbour's \nexcess deaths per 10k",
                                      "% of sewage access",
                                      "% unemployed")) +
      theme(plot.margin = margin(c(10,10,10,10)))

coef_grid <- gridExtra::grid.arrange(a,b)
ggsave("coef_grid.png", coef_grid, width = 7, height = 8)
