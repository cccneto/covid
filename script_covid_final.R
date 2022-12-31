# installing packages and load it if necessary
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(stargazer)) install.packages('stargazer')
library(stargazer)

if (!require(sampleSelection)) install.packages('sampleSelection')
library(sampleSelection)


# load data - It is data imputed as mentioned in the article methodology
database <- read.csv("https://raw.githubusercontent.com/cccneto/covid/master/database.csv", header = T)

# Converting to factors
as.factor(database$sexo) # gender
as.factor(database$equip_defensiv)  # pp.equip 
as.factor(database$morte_cov) # Respondents who know someone who died from COVID-19 
as.factor(database$estcivil) # marriage or not

##########################################################  Inequality analysis 

# selecting variables 

database <- database %>%  
  select(equip_defensiv, gasto_equip_defensivo, sexo, morte_cov, estcivil, ppe_inc,idade, d_raca, lndeaths, filho_dep, plano, lnrenda, renda) %>% 
  filter(idade >= 16, renda > 0,  renda > gasto_equip_defensivo) # constrains of age, income and income vs spend with pp.equip


# rename variables using english

database <- database %>% rename(
  pp.equip = equip_defensiv, 
  spend.equip = gasto_equip_defensivo, 
  gender = sexo, 
  d.contact = morte_cov, 
  married = estcivil, 
  ppe_inc = ppe_inc, # spend with protection / income 
  age = idade, 
  d_race = d_raca, 
  lndeaths = lndeaths, 
  care = filho_dep, 
  h.insurance = plano, 
  lnincome = lnrenda, 
  income = renda
)
  

# Statistical summary

  database %>% 
  select(pp.equip, ppe_inc, gender, d.contact, married, 
         ppe_inc, age, d_race, lndeaths, care, h.insurance, 
         lnincome, income) %>%
  skimr::skim()

# modelo pedido pelo revisor
heck_rev = heckit(
  selection = pp.equip ~ gender + d.contact + married,
  outcome = ppe_inc ~ gender + age + d_race + lndeaths + care + h.insurance + lnincome,
  data = database,
  method = "2step"
)


# Summary Model 
summary(heck_rev, singular.ok = T)
stargazer::stargazer(heck_rev, type = 'text')



library(ggplot2)

ggplot(database, aes(x=income, y=ppe_inc)) + 
  labs(title="ppe_inc Vs Income", subtitle= "Titulo", y="ppe_inc", x="Income", caption="dados...") +
  geom_point() +  # se quiser colocar cor (aes(col=escolaridade), size=3) 
  geom_smooth() + 
  theme_bw() +
  scale_x_continuous(breaks= seq(0, 5000000, 4000), 
                     labels = function(x){paste0(x/1000, 'k')})


