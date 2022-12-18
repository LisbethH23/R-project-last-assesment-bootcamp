#Load data
df<- read.csv("/cloud/project/HollywoodsMostProfitableStories.csv")
View(df)
install.packages("tidyverse")
str(df)
colSums(is.na(df))
#na.omit(df)
#colSums(is.na(df))
#na.omit(df)
#colSums(is.na(df))

#drop missing values , the last command it worked
#print(na.omit(df))
#colSums(is.na(df))
#df_clean <- na.omit(df)
colSums(is.na(df))
df <- df[complete.cases(df),]
colSums(is.na(df_clean))              
#df <- df[rowSums(is.na(df)) ]
#colSums(is.na(df)) 
#Check for duplicates
dim(df[duplicated(df$Film),])[1]
#round off values to 2 places
df$Profitability <-round(df$Profitability ,digit=2)
df$Worldwide.Gross <-round(df$Worldwide.Gross ,digit=2)
#View(df)
dim(df)
#Check for outliers using a boxplot
library(ggplot2)
ggplot(df,aes(x=Profitability, y=Worldwide.Gross)) +geom_boxplot(outlier.colour= "blue",outlier.shape= 4)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,750))
#Remove outliers in 'Profitability'
Q1 <- quantile(df$Profitability, .25)
Q3 <- quantile(df$Profitability, .75)
IQR <- IQR(df$Profitability)

no_outliers <- subset(df, df$Profitability> (Q1 - 1.5*IQR) & df$Profitability< (Q3 + 1.5*IQR))
dim(no_outliers) 
# Remove outliers in 'Worldwide.Gross'
Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)
df1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))
dim(df1)
#Summary Statistics/Univariate Analysis:
summary(df1)

#bivariate analysis

#scatterplot
ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point(colour="green")+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

#bar chart
ggplot(df1, aes(x=Year)) + geom_bar(fill="blue",colour="green")

ggplot(data=df1, mapping=aes(x=Lead.Studio, y=2005&2011)) + geom_point(size=1)+
geom_line(colour="red")

ggplot(df1,aes(Year,Profitability,colour=Genre))+
  geom_point(size=1,aplha=0.5)+
  geom_smooth(method=lm,se=F)+
facet_wrap(~Genre)
