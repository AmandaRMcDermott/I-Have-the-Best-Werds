####Regressions
library(ggplot2)
library(tidyverse)
library(modelr)
library(tseries)
library(DataCombine) #Slide function
library(vars)
library(plm)
library(stargazer)
###Data needed
##JM file
##gdp growth

JM2 <- JM2 %>% rename(ctry = country)
df <- JM2


boxplot(df$sotu_secu, df$ungd_secu, names = c("SOTU", "UNGD"), main = "Security Sentiment")
t.test(df$sotu_secu, df$ungd_secu)
boxplot(df$sotu_econ, df$ungd_econ, names = c("SOTU", "UNGD"), main = "Economic Sentiment")
t.test(df$sotu_econ, df$ungd_econ)
boxplot(df$sotu_enviro, df$ungd_enviro, names = c("SOTU", "UNGD"), main = "Environmental Sentiment")
t.test(df$sotu_enviro, df$ungd_enviro)
boxplot(df$sotu_moral, df$ungd_moral, names = c("SOTU", "UNGD"), main = "Moral Sentiment")
t.test(df$sotu_moral, df$ungd_moral)
boxplot(df$sotu_sent_pn, df$ungd_sent_pn, names = c("SOTU", "UNGD"), main = "Positivity Sentiment")
t.test(df$sotu_sent_pn, df$ungd_sent_pn)
boxplot(df$sotu_sent_ab, df$ungd_sent_ab, names = c("SOTU", "UNGD"), main = "Sentimentality")
t.test(df$sotu_sent_ab, df$ungd_sent_ab)

hist(df$gdp_growth)
table(df$gdp_growth < 2, df$ctry)
df$badgrowth <- (df$gdp_growth < 2)


ggplot(data = JM2) + geom_boxplot(aes(x = ctry, y = sotu_secu)) + xlab("Country") + ylab("Security Sentiment (SOTU)")
ggplot(data = JM2) + geom_boxplot(aes(x = ctry, y = ungd_secu)) + xlab("Country") + ylab("Security Sentiment (UNGD)")
ggplot(data = JM2) + geom_boxplot(aes(x = ctry, y = sotu_econ)) + xlab("Country") + ylab("Economic Sentiment (SOTU)")
ggplot(data = JM2) + geom_boxplot(aes(x = ctry, y = ungd_econ)) + xlab("Country") + ylab("Economic Sentiment (UNGD)")

ggplot(data = JM2) + geom_boxplot(aes(x = ctry, y = sotu_enviro)) + xlab("Country") + ylab("Environment Sentiment (SOTU)")
ggplot(data = JM2) + geom_boxplot(aes(x = ctry, y = ungd_enviro)) + xlab("Country") + ylab("Environment Sentiment (UNGD)")
ggplot(data = JM2) + geom_boxplot(aes(x = ctry, y = sotu_moral)) + xlab("Country") + ylab("Moral Sentiment (SOTU)")
ggplot(data = JM2) + geom_boxplot(aes(x = ctry, y = ungd_moral)) + xlab("Country") + ylab("Moral Sentiment (UNGD)")


ggplot(data = JM) + geom_point(aes(x = year, y = sotu_econ, color = ctry)) +
  geom_smooth(aes(x = year, y = sotu_econ, color = ctry))

ggplot(data = JM) + geom_point(aes(x = year, y = sotu_secu, color = ctry)) +
  geom_smooth(aes(x = year, y = sotu_secu, color = ctry))

#Experimenting with panel
lm1<-lm(gdp_growth~sotu_secu, data = df2)
summary(lm1)

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

####Economic Shock

