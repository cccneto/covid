# carregando pacotes
library(readstata13)
library(dplyr)
library(stargazer)
library(sampleSelection)



# chamando base com imputacoes
covid_imp_rob <- read.csv("https://raw.githubusercontent.com/cccneto/covid/master/database.csv", header = T)

as.factor(covid_imp_rob$sexo)
as.factor(covid_imp_rob$equip_defensiv)
as.factor(covid_imp_rob$morte_cov)
as.factor(covid_imp_rob$estcivil)

##########################################################  Inequality analysis 

# constrains

base_dados <- 
  covid_imp_rob %>% 
  select(equip_defensiv, gasto_equip_defensivo, sexo, morte_cov, estcivil, ppe_inc,idade, d_raca, lndeaths, filho_dep, plano, lnrenda, renda) %>% 
  filter(idade >= 16, renda > 0,  renda > gasto_equip_defensivo) 

# Statistical summary

base_dados %>% 
  select(equip_defensiv, sexo, morte_cov, estcivil,
         ppe_inc, idade, d_raca, lndeaths, 
         filho_dep, plano, lnrenda) %>%
  skimr::skim()

# modelo pedido pelo revisor
heck_rev = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = ppe_inc ~ sexo + idade + d_raca + lndeaths + filho_dep + plano + lnrenda,
  data = base_dados,
  method = "2step"
)


summary(heck_rev, singular.ok = T)
stargazer::stargazer(heck_rev, type = 'text')



library(ggplot2)

ggplot(teste, aes(x=renda, y=ppe_inc)) + 
  labs(title="ppe_inc Vs Renda", subtitle= "Titulo", y="ppe_inc", x="Renda", caption="dados...") +
  geom_point() +  # se quiser colocar cor (aes(col=escolaridade), size=3) 
  geom_smooth() + 
  theme_bw() +
  scale_x_continuous(breaks= seq(0, 5000000, 4000), 
                     labels = function(x){paste0(x/1000, 'k')})

ggplot(teste, aes(x=renda, y=ppe_inc)) + 
  labs(title="ppe_inc Vs Renda", subtitle= "Titulo", y="ppe_inc", x="Renda", caption="dados...") +
  geom_boxplot() +  # se quiser colocar cor (aes(col=escolaridade), size=3) 
  geom_smooth() + 
  theme_bw() +
  scale_x_continuous(breaks= seq(0, 5000000, 4000), 
                     labels = function(x){paste0(x/1000, 'k')})



