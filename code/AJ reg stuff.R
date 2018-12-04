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

boxplot(df$sotu_secu, df$ungd_secu, names = c("SOTU", "UNGD"), main = "Security Sentiment")
t.test(df$sotu_secu, df$ungd_secu)
boxplot(df$sotu_econ, df$ungd_econ, names = c("SOTU", "UNGD"), main = "Economic Sentiment")
t.test(df$sotu_econ, df$ungd_econ)
boxplot(df$sotu_wl, df$ungd_wl, names = c("SOTU", "UNGD"), main = "Word Length")
t.test(df$sotu_wl, df$ungd_wl)
boxplot(df$sotu_sent_pn, df$ungd_sent_pn, names = c("SOTU", "UNGD"), main = "Positivity Sentiment")
t.test(df$sotu_sent_pn, df$ungd_sent_pn)
boxplot(df$sotu_sent_ab, df$ungd_sent_ab, names = c("SOTU", "UNGD"), main = "Sentimentality")
t.test(df$sotu_sent_ab, df$ungd_sent_ab)
boxplot(0, 0, names = c("SOTU", "UNGD"), main = "References to Pizza Bagels")
t.test(df$sotu_sent_ab, df$ungd_sent_ab)

table(df$gdp_growth < 1)

ggplot(data = JM) + geom_boxplot(aes(x = ctry, y = sotu_secu)) + xlab("Country") + ylab("Security Sentiment (SOTU)")
ggplot(data = JM) + geom_boxplot(aes(x = ctry, y = ungd_secu)) + xlab("Country") + ylab("Security Sentiment (UNGD)")
ggplot(data = JM) + geom_boxplot(aes(x = ctry, y = sotu_econ)) + xlab("Country") + ylab("Economic Sentiment (SOTU)")
ggplot(data = JM) + geom_boxplot(aes(x = ctry, y = ungd_econ)) + xlab("Country") + ylab("Economic Sentiment (UNGD)")
ggplot(data = JM) + geom_boxplot(aes(x = ctry, y = ungd_sent_pn))
ggplot(data = JM) + geom_boxplot(aes(x = ctry, y = ungd_wl))
ggplot(data = JM) + geom_boxplot(aes(x = ctry, y = sotu_wl))

ggplot(data = JM) + geom_point(aes(x = year, y = sotu_sent_pn, color = ctry)) +
  geom_smooth(aes(x = year, y = sotu_sent_pn, color = ctry))

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

?add_residuals

####Economic Shock
df_Russia <-subset(df, ctry == "RUS")
median(df_Russia$gdp_growth, na.rm = TRUE)
summary(df_Russia$gdp_growth)

lm1 <- lm(ungd_wl ~ gdp_growth, data = df_Russia)
summary(lm1)





df <- tibble::data_frame(
  x = sort(runif(100)),
  y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
)
plot(df)

m1 <- lm(y ~ x, data = df)
df %>% add_residuals(m1)

m2 <- lm(y ~ poly(x, 2), data = df)
df %>% spread_residuals(m1, m2)
df %>% gather_residuals(m1, m2)
