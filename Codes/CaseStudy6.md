# CaseStudy6
Vinh Le  
June 21, 2016  

#INTRODUCTION
### We examine two dataset GDP.csv and Edstat.csv from the World Bank website. Each datasets contain an enormous amount of data where we have to clean up the data in order to perform the necessary analysis. Once the data is Tidy we then merge the two together to answer the following questions below. 


##REQUIRED PACKAGES

```r
require("repmis")
```

```
## Loading required package: repmis
```

```r
require("dplyr")
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
require("ggplot2")
```

```
## Loading required package: ggplot2
```
##LOADING DATA

```r
site1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"

site2<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

download.file(site1, destfile="./GDP.csv")
download.file(site2, destfile ="./Edstat.csv")

rawGDP <- read.csv("/Users/vinhle/Desktop/SMU DATA SCIENCE/MSDS 6306 Intro to Data Science/Unit 6/Case Study/GDP.csv", header=TRUE)

rawEdstat <-read.csv("/Users/vinhle/Desktop/SMU DATA SCIENCE/MSDS 6306 Intro to Data Science/Unit 6/Case Study/Edstat.csv", header=TRUE)
```
## CLEAN UP RAW GDP DATA

```r
gdp<-rawGDP[5:235,c(1,2,4,5)]
names(gdp)<-c("CountryCode","Ranking","CountryName","GDP")
#Assign Ranking as numeric value
gdp$Ranking<-as.numeric(gdp$Ranking)
#Convert CountryCode and CountryName to Character
gdp$CountryCode<-as.character(gdp$CountryCode)
gdp$CountryName<-as.character(gdp$CountryName)
#Remove commas in GDP column
gdp$GDP<-as.numeric(gsub(",","",gdp$GDP))
```

```
## Warning: NAs introduced by coercion
```

```r
#Sum of all NAs in gdp data
missingValues<-sum(is.na(gdp))
##Remove all rows with NAs
gdp<-gdp[!is.na(gdp$GDP),]
```


```r
str(gdp)
```

```
## 'data.frame':	204 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : num  3 104 115 126 137 148 159 170 181 4 ...
##  $ CountryName: chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : num  16244600 8227103 5959718 3428131 2612878 ...
```

## CLEAN UP RAW EDSTAT DATA

```r
edstat<-rawEdstat
edstat$CountryCode<-as.character(edstat$CountryCode)
edstat$Income.Group<-as.character(edstat$Income.Group)
```
##MERGE GDP AND EDSTAT DATA

```r
mergeData<-merge(x=gdp, y=edstat, by="CountryCode", all=TRUE)
```

###Question 1: Match the data based on the country shortcode. How many of the IDs match? 

```r
sum(!is.na(unique(mergeData$Ranking)))
```

```
## [1] 190
```
#####There are 190 IDs that match. 


###Question 2: 	Sort the data frame in ascending order by GDP rank (so United States is last). What is the 13th country in the resulting data frame?

```r
sortMergeData <-mergeData[order(mergeData$GDP,decreasing = FALSE),]

###Look at the 13th row in the third column Ranking.
sortMergeData[13,3]
```

```
## [1] "St. Kitts and Nevis"
```
#####The 13th country from the data with ascending order by GDP ranking is St. Kitts and Nevis.


###Question 3: 	What are the average GDP rankings for the "High income: OECD" and "High income: nonOECD" groups? 

```r
OECD<-sortMergeData[sortMergeData$Income.Group == "High income: OECD",]
#Remove any NAs in OECD$Ranking
OECD<-OECD[!is.na(OECD$Ranking),]

nonOECD<-sortMergeData[sortMergeData$Income.Group == "High income: nonOECD",]
#Remove any NAs in nonOECD$Ranking
nonOECD<-nonOECD[!is.na(nonOECD$Ranking),]
```


```r
mean(OECD$Ranking)
```

```
## [1] 110.0667
```

```r
mean(nonOECD$Ranking)
```

```
## [1] 93.73913
```
#####We filter out the two groups OECD and nonOECD and then remove any NAs from the the dataset. Then we take the mean of each category.The average ranking of OECD is 110.066. The average ranking of nonOECD is 93.739.


###Question 4:  Plot the GDP for all of the countries. Use ggplot2 to color your plot by Income Group.

```r
ggplot(mergeData,
    aes(x=GDP/1000000,y=Income.Group,color=factor(Income.Group))) +
    geom_point() +
    stat_smooth(se=F) +
    coord_flip() +
    theme(legend.position="none") +
      xlab("GDP in Million US$") +
      ylab("Income Groups") +
    labs(title="GDP by Income Group")
```

```
## Warning: Removed 32 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 32 rows containing missing values (geom_point).
```

![](CaseStudy6_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

###Question 5: Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. How many countries are Lower middle income but among the 38 nations with highest GDP?

```r
Q<-sortMergeData
Q<-Q[!is.na(Q$Ranking),]

quantiles <- quantile(Q$Ranking, probs= seq(0,1,0.2))

#Breaking into 5 Quantiles for Ranking
Q[Q$Ranking >= quantiles[1] & Q$Ranking <= quantiles[2], "Income.Level"] <- "Q1"
Q[Q$Ranking > quantiles[2] & Q$Ranking <= quantiles[3], "Income.Level"] <- "Q2"
Q[Q$Ranking > quantiles[3] & Q$Ranking <= quantiles[4], "Income.Level"] <- "Q3"
Q[Q$Ranking > quantiles[4] & Q$Ranking <= quantiles[5], "Income.Level"] <- "Q4"
Q[Q$Ranking > quantiles[5] & Q$Ranking <= quantiles[6], "Income.Level"] <- "Q5"

table(Q$Income.Group, Q$Income.Level)
```

```
##                       
##                        Q1 Q2 Q3 Q4 Q5
##                        14  0  0  0  0
##   High income: nonOECD  5  5  2  6  5
##   High income: OECD     4  2  5 15  4
##   Low income            7 18  7  1  4
##   Lower middle income   7  7 16  9 15
##   Upper middle income   4  8 10 10 13
```
##### There are 7 countries that in the Lower Middle Income but among the 38 nations with highest GDP.


#CONCLUSION
###Tidying the two datasets GDP and Edstat helped answer the questions above. There are 7 out of 38 countries that have the highest GDP with lower middle income. 


