###################################
#Correlation between IBOV's stocks#
###################################

############
#Important: This study just proposes to show how to determine the correlation
#between multiple stocks. In no way it can be considered a recommendation of 
#operation or investment.
############

#This study proposes to analyze the correlation between IBOV's stocks.
#IBOV index is the main index in the brazilian stock market. It's composed of 
#82 stocks.
#First of all, let's briefly clarify what is correlation: 
#Source: investopedia.com
#Correlation shows the strength of a relationship between two variables and is 
#expressed numerically by the correlation coefficient. The correlation 
#coefficient's values range between -1.0 and 1.0. A perfect positive correlation
#means that the correlation coefficient is exactly 1. This implies that as one
#security moves, either up or down, the other security moves in lockstep, in the
#same direction. A perfect negative correlation means that two assets move in 
#opposite directions, while a zero correlation implies no linear relationship at all.

###########
#Important: Correlation does not imply causation!
###########
#Correlation measures association, but doesn't show if x causes y or vice versa, 
#or if the association is caused by a third–perhaps unseen–factor.
###########

#Correlation in stock market
#The correlation coefficient is measured on a scale from -1 to 1. A correlation
#coefficient of 1 indicates a perfect positive correlation between the prices of
#two stocks, meaning the stocks always move the same direction by the same amount. 
#A coefficient of -1 indicates a perfect negative correlation, meaning that the
#stocks have historically always moved in the opposite direction. If two stocks 
#have a correlation coefficient of 0, it means there is no correlation and, 
#therefore, no relationship between the stocks. It is unusual to have either a
#perfect positive or negative correlation.

#Investors can use the correlation coefficient to select assets with negative
#correlations for inclusion in their portfolios, long/short operations and 
#predictions, etc. The calculation of the correlation coefficient takes the covariance 
#of the two variables in question and each variable's standard deviation.

#While standard deviation is a measure of the dispersion of data from its average,
#covariance is a measure of how two variables change together. By dividing 
#covariance by the product of the two standard deviations, one can calculate the
#correlation coefficient and determine to what extent assets in a portfolio are
#likely to move in tandem.


#Install packages:
packages_1 <- c("plotly","tidyverse","knitr","kableExtra","car","rgl","gridExtra",
             "PerformanceAnalytics","reshape2","rayshader","psych","ggrepel",
             "factoextra","sp","tmap","magick","gridExtra",'BatchGetSymbols','quantmod', 'ggthemes', 'plyr' )

if(sum(as.numeric(!packages_1 %in% installed.packages())) != 0){
  instalador <- packages_1[!packages_1 %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(packages_1, require, character = T) 
} else {
  sapply(packages_1, require, character = T) 
}


#Initial Date
di = "2016-01-01"
#Final Date
df = Sys.Date()
#IBOV index as benchmark
benchmark = '^BVSP'

#Load IBOV's stocks datas

ibov = GetIbovStocks()
View(ibov)

#Creating a new collumn including ".SA" in all stock's names to use 
#yahoo.com as source.

ibov$tickersSA = paste(ibov$tickers, ".SA", sep="")

#Load all stocks that compounds IBOV index:

data_ibov = BatchGetSymbols(
  tickers = ibov$tickersSA,
  first.date = di,
  last.date = df,
  bench.ticker = benchmark,
)

View(data_ibov)

#The data came in format of List. Let's change to DataFrame format and use just 
#the  second part of the list that contains the data we gonna use.

data_ibov = data_ibov$df.tickers
View(data_ibov)


#Changing the format of the DF to select one stock at time.

data_ibov2 = dlply(data_ibov, .(ticker), function(x) {rownames(x)=x$row; x$row = NULL;x})

#.(ticker) indicates that will separate this DF (data_ibov) into several other
#DFs according to the column "ticker"
#function(x) : function to separate the lines according to the referred stock, 
#will create a list of DFs.

View(data_ibov2)

#data_ibov had 82 observations, but just 75 returned in data_ibov2 because some
#stocks have missing information in the period selected (01-01-2016 until today).


#Select just columns 6 (price.adjusted) and 7 (ref.date). The column ref.date 
#will be renamed to Date.
#The best way to manipulate these data is to put them in a format where the 
#stock adjusted prices are separated by column, each column corresponds to the 
#stock adjusted prices, and each row corresponds to a trading session.

#Create a looping (for) to make the commands to all stocks in data_ibov2:

#acao = data_ibov3

data_ibov3 = data_ibov2[[1]][c(7,6)]
colnames(data_ibov3) = c("Date", paste(data_ibov2[[1]][1,8]))

for(i in 2:75){
  data_ibov4 = data_ibov2[[i]][c(7,6)]
  colnames(data_ibov4) = c("Date", paste(data_ibov2[[i]][1,8]))
  data_ibov3 = merge(data_ibov3, data_ibov4, by = "Date")
}
View(data_ibov3)

#Load IBOV and S&P500 data to use as benchmark:

#IBOV
IBOV = BatchGetSymbols(
  tickers = '^BVSP',
  first.date = di,
  last.date = df,
  bench.ticker = benchmark,
)

IBOV = IBOV$df.tickers

#SP500
SP500 = BatchGetSymbols(
  tickers = '^GSPC',
  first.date = di,
  last.date = df,
  bench.ticker = '^GSPC',
)

SP500 = SP500$df.tickers


#Changing the column's names to reach the same format of data_ibov3:
colnames(IBOV)[6] = 'IBOV'
colnames(IBOV)[7] = 'Date'

colnames(SP500)[6] = 'SP500'
colnames(SP500)[7] = 'Date'

#Selecting just columns 6 (price.adjusted) and 7 (Date):

IBOV = IBOV[,c(7,6)]
SP500 = SP500[,c(7,6)]


#Joint of SP500 and IBOV:
ibov_sp500=merge(IBOV, SP500, by = 'Date')
View(ibov_sp500)

#Joint de ibov_sp500 and data_ibov3:
data_ibov5 = merge(ibov_sp500, data_ibov3, by = 'Date')
View(data_ibov5)
# total = data_ibov5

#Standardizing adjusted prices:

#Deleting collumn Date for a moment:
std_ibov = data_ibov5[,-c(1)]
View(std_ibov)

#std_ibov = normalizado

#The idea of standarding is that all variables "start" from the same place, 
#in this case the value 1. For this we will take the first value from the column of
#each stock and divide by all other values of the column.
#Let's use the function lapply that will apply the STD function in all columns.
std_ibov2 = data.frame(lapply(std_ibov, function(x) x/x[1]))
View(std_ibov2)

#novo_total= std_ibov2

#Returning column Date

std_ibov2$Date = data_ibov5$Date
View(std_ibov2)

# Plot all stocks of Ibovespa + IBOV + SP500

# Transforming data  to a dataframe of 3 columns: Date, Series e Value. 
#Series contains the name and value contains the adjusted price.

df_all = melt(std_ibov2, id.vars = "Date", variable.name = 'Series')
View(df_all)

#df_todas = df_all

ggplot(df_all, aes(Date, value)) + geom_line(aes(color=Series))

#The graph is anything but visual, just to check is there is nothing out of whack.

#Calculating correlation:

#using the object std_ibov because it has no column Date.
library(corrplot)
correlation = cor(std_ibov, use='complete.obs', method = 'spearman')
View(correlation)

#Ploting Correlation:
corrplot(correlation, number.cex = 0.001, number.font = 5, tl.srt = 90, tl.cex=0.5)

#Plot correlation as a heat map and including correlation value:
correlation %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 1)),
            size = 2) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlation") +
  scale_fill_gradient2(low = "brown4", 
                       mid = "white", 
                       high = "dodgerblue4",
                       midpoint = 0) +
  theme(axis.text.x = element_text(size = 5, angle = 90),
        axis.text.y = element_text(size = 4),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

# First step: save the correlation heat map and delete the correlation values:

plot3d_cor <- correlation %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value, color = value),
            color = "black") +
  labs(x = NULL,
       y = NULL,
       fill = "Correlation") +
  scale_fill_gradient2(low = "brown4", 
                       mid = "white", 
                       high = "dodgerblue4",
                       midpoint = 0) +
  theme(axis.text.x = element_text(size = 5, angle = 90),
        axis.text.y = element_text(size = 5),
        title = element_text(size = 18,face = "bold"),
        panel.border= element_rect(size = 1, color = "black", fill = NA))

plot3d_cor

#Second step: visualize the 3D graph. Not usual, but beautiful.

plot_gg(ggobj = plot3d_cor, 
        multicore = TRUE, 
        width = 6, 
        height = 6, 
        scale = 100, 
        background = "white",
        shadowcolor = "dodgerblue4")




