#' Author: Ted Kwartler
#' Date: Sept 5 2022
#' Purpose: R popular visuals
#' Good resource: https://r-graphics.org/

library(ggplot2)

# https://twitter.com/MaxCRoser/status/1566182467010273280
# Our world in Data example
df<-data.frame(country = rep(as.factor(c('Ireland','Portugal','Spain','Jamaica','Denmark','United Kingdom',
                               'Romania','Croatia','Finland','Netherlands',
                               'France','Germany','Estonia','Sweden','Cyprus','Singapore',
                               'Hungary','Japan','United States','Mexico','Czechia',
                               'New Zealand','Latvia','Slovena','Elsalvador')),2),
               gdp = c(0.81,0.1,0.16,0.06,0.19,0.22,0.62,0.16,0.14,0.22,0.18,0.24,0.37,0.32,0.31,0.96,0.29,0.09,0.28,0.33,0.41,0.38,0.30,0.31,0.36),
               co2 =  c(-0.42,-0.38,-0.35,-0.33,-0.29,-0.28,-0.26,-0.25,-0.23,-0.23,-0.22,-0.21,-0.21,-0.19,-0.19,-0.19,-0.16,-0.16,-0.15,-0.10,-0.05,-0.05,-0.04,-0.04,-0.03))
df$gdpPct <- paste0('+',substr(df$gdp, 3, nchar(df$gdp)),'%')
df$co2Pct <- paste0('-',substr(df$co2, 4, nchar(df$co2)),'%')
ggplot(df, aes(x= gdp, y = 0)) + 
  facet_wrap(~country) + 
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = gdp), arrow = arrow(length = unit(0.1, "cm")), color = 'blue') + 
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = co2), arrow = arrow(length = unit(0.1, "cm")), color = 'red') +
  geom_text(data = df, mapping = aes(x = 1, y = gdp, label = gdpPct), hjust   = 1.75, vjust   = -0.15, size = 2.25, color='black') + 
  geom_text(data = df, mapping = aes(x = 1, y = co2, label = co2Pct), hjust   = 1.75, vjust   = -1, size = 2.25, color='black') + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        strip.text.x = element_text(hjust = -0.01),
        strip.background=element_rect(fill="#F5F5F5"),
        plot.subtitle=element_text(size=10, color="darkgrey"),
        panel.background = element_rect(fill= '#F5F5F5')) + 
  labs(title = bquote('Decoupling: Countries that achieved economic growth while reducing' ~CO[2]~ 'emissions, 2005-19'),
       subtitle = bquote('Emissions are adjusted for trade.  This means that ' ~CO[2]~ 'emissions caused inthe production of imported goods are added to its domestic emissions - and for goods that are exported the emissions are subtracted.'),
       caption = "Data source: twitter@MaxCRoser, https://ourworldindata.org/carbon-price") 

# End
