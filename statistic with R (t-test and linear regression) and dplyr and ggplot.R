install.packages("gapminder")
library("gapminder")
data("gapminder")
view(gapminder)
summary(gapminder)
mean(gapminder$gdpPercap)
attach(gapminder)
median(pop)
hist(lifeExp)
hist(log(pop))
boxplot(lifeExp~continent)
plot(lifeExp~log(gdpPercap))
#-----------------------------------------------------

#install.packages("dplyr")
library("dplyr")

gapminder %>%
  select(country,lifeExp)%>%
  filter(country=="South Africa" | country=="Ireland")%>% 
  group_by(country)%>%
summarise(Average_life = mean(lifeExp))

#----------------------------------------------------
df1<-gapminder %>%
  select(country,lifeExp)%>%
  filter(country=="South Africa" | country=="Ireland")
  
t.test(data=df1, lifeExp~country)

library(ggplot2)

gapminder %>%
  filter(gdpPercap<5000) %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp,col=continent, size= pop))+
  geom_point(alpha=0.3)+geom_smooth(method = lm)+facet_wrap(~continent)

gapminder %>%
  filter(gdpPercap<5000) %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp,col=year, size= pop))+
  geom_point(alpha=0.3)+geom_smooth(method = lm)+facet_wrap(~continent)

lm(lifeExp~gdpPercap)
summary(lm(lifeExp~gdpPercap))
summary(lm(lifeExp~gdpPercap+pop)) #multivariate analysis







