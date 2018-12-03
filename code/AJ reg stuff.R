####Regressions
library(ggplot2)
library(tidyverse)
library(tseries)
library(DataCombine) #Slide function
library(vars)
library(plm)


###Data needed
##JM file
##gdp growth

JM <- JM %>% rename(ctry = country)
gdp_growth_2 <- gdp_growth_2 %>% rename(ctry = ifs)

df <- JM %>% 
  left_join(gdp_growth_2)
df <- df[-c(1,2,333:338),]
df <- subset(df, year > 1944)

df2 <- slide(df, Var = "gdp_growth", GroupVar = "ctry", slideBy = 1)

DataSlid2 <- slide(Data2, Var = "B", GroupVar = "ID",
                   slideBy = 2)

table(df$year)
#consecutive years only
ggplot(data = JM) + geom_boxplot(aes(x = ctry, y = sotu_secu))
ggplot(data = JM) + geom_boxplot(aes(x = ctry, y = ungd_secu))
ggplot(data = JM) + geom_boxplot(aes(x = ctry, y = sotu_econ))
ggplot(data = JM) + geom_boxplot(aes(x = ctry, y = ungd_econ))

#Experimenting with panel
fixed <- plm(gdp_growth ~ ungd_sent_pn, data= df, index=c("ctry", "year"), model="within")
random <- plm(gdp_growth ~ ungd_sent_pn, data= df, index=c("ctry", "year"), model="random")
phtest(fixed, random)
summary(fixed)

fixed <- plm(gdp_growth ~ ungd_sent_ab, data= df, index=c("ctry", "year"), model="within")
random <- plm(gdp_growth ~ ungd_sent_ab, data= df, index=c("ctry", "year"), model="random")
phtest(fixed, random)
summary(random)

fixed <- plm(gdp_growth ~ sotu_sent_pn, data= df, index=c("ctry", "year"), model="within")
random <- plm(gdp_growth ~ sotu_sent_pn, data= df, index=c("ctry", "year"), model="random")
phtest(fixed, random)
summary(fixed)

fixed <- plm(gdp_growth ~ sotu_sent_ab, data= df, index=c("ctry", "year"), model="within")
random <- plm(gdp_growth ~ sotu_sent_ab, data= df, index=c("ctry", "year"), model="random")
phtest(fixed, random)
summary(random)


fixed <- plm(gdp_growth ~ ungd_sent_ab + ungd_sent_pn + ungd_wl + ungd_econ + ungd_secu, data= df, index=c("ctry", "year"), model="within")
random <- plm(gdp_growth ~ ungd_sent_ab + ungd_sent_pn + ungd_wl + ungd_econ + ungd_secu, data= df, index=c("ctry", "year"), model="random")
phtest(fixed, random)
summary(fixed)

#sotu_sent_ab + sotu_sent_pn + sotu_wl + sotu_econ + sotu_secu +
fixed  <- plm(gdp_growth1 ~  sotu_econ, data= df2, index=c("ctry", "year"), model="within")
fixed  <- plm(gdp_growth1 ~  sotu_sent_pn + sotu_wl + sotu_econ + sotu_secu + gdp_growth, data= df2, index=c("ctry", "year"), model="within")
random <- plm(gdp_growth1 ~  sotu_sent_pn + sotu_wl + sotu_econ + sotu_secu + gdp_growth, data= df2, index=c("ctry", "year"), model="random")
phtest(fixed, random)
summary(fixed)
