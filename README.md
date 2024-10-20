---
output:
  pdf_document: default
  html_document: default
---


---
title: "Spatial Autocorrelation Tutorial"
author: "Geog 418"
date: "`r format(Sys.time(), '%B %d, %Y')`"
output:
  pdf_document: default
  word_document: default
---







## Introduction

Spatial Autocorrelation is the amount of similarity between spatial data points within a study area and can be used to assess clustering or dispersion or random patterns in a geographic location. High spatial autocorrelation indicates that nearby locations have similar characteristics. For example in many cities neighbourhoods with high property values tend to cluster together; the same is usually true for areas with lower property values. This is an example of positive spatial autocorrelation. The data used in this study originate from the 2016 Government of Canada Census. The data contains information about census divisions (government term for municipal areas), land area, electoral districts, census division codes, and other boundaries. This data is useful for assessing spatial autocorrelation because it has defined geographic coordinates and regional identifiers, a large sample size, and sufficient variability. The excel data contains various information for each dissemination area (named GEO UID in the file) which is a unique code that identifies a geographic area along with other attributes such as census division. Information available for each dissemination area includes median income, sample size of incomes, land area, population, knowledge of French language, sample size of French language knowledge, population, and other geographic government codes. 

Libraries in R Studio are packages of pre-written code that allow R users to produce information faster. Typically, libraries are deployed so users can concentrate on data presentation instead of building complex mapping or graphing code from scratch. An analogy for using libraries could be using an existing theme in MS Power Point; instead of building each slide from scratch, users can save time by applying a theme for all slides. This way they can focus on writing text instead of selecting colors and formatting background artwork. 

## Setup
Define the working directory. This is where R looks for files and saves outputs. 
```
knitr::opts_knit$set(root.dir = "C:/Users/Albany/OneDrive - University of Victoria/GEOG418/lab3 spatial auto/r_studio")
```

Define the file paths. For this assignment, the data was kept in a separate folder for organization. 
```{r Setup, echo=TRUE, eval=TRUE, warning=FALSE}
# Define the directory path here
data_directory <- "C:/Users/Albany/OneDrive - University of Victoria/GEOG418/lab3 spatial auto/Assignment3_Data/"
```

Load required libraries and packages:
```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Install packages if not already installed:
setwd("C:/Users/Albany/OneDrive - University of Victoria/GEOG418/lab3 spatial auto/r_studio")
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# install.packages("knitr")
# #install.packages("rgdal_1.6-7.tar.gz",repos=NULL, type='source')
# install.packages("tmap")
# install.packages("spdep")
# install.packages("raster")
# install.packages("shinyjs")
# install.packages("e1071")

#Load in libraries:
library("knitr")
# library("rgdal")
library(tmap)
library("spdep")
library("raster")
library("shinyjs")
library("e1071")
library(sf)
library(ggplot2)
library(raster)
library(spatstat)
library(sp)
library(tmap)
library(dplyr)
library(st)
library(plyr)
library(ggplot2)
```



First the data is read by the R Studio script. The excel data is read by the built-in functions and the .shp is read using the st library. For an explanation of the data, see the introduction. Before executing this step, make sure all data is in the same directory as the .rmd file. In this example, the .shp and .csv is in a different directory from the .rmd.

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#From the working dir read in the csv
csv <- read.csv(paste0(data_directory,"ucgsJQnBVLvP_data.csv"))

#Data source is the working dir (where the layer is), layer is the name of the file
shp <- st_read(paste0(data_directory, "lda_000a16a_e.shp")) 
shp <- st_transform(shp, crs=3005)
#print(shp)
```

Next, the data will be "cleaned" and adjusted specifically to this tutorial. First, a vector of the column names will be created to clarify what each column represents. Then, any unnecessary rows will be removed and the cleaned data will be merged with the spatial polygon data frame. After that, the focus will be on the specific city of interest for this analysis: 'Prince George.' Finally, any absolute count data will be converted into rates for mapping and analysis purposes.

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#New column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols

#Add column to count number of ID characters
csv$len <- nchar(csv$`GEO UID`)

write.csv(csv, "C:/Users/Albany/OneDrive - University of Victoria/GEOG418/lab3 spatial auto/Assignment3_Data/exported_data.csv", row.names = FALSE)

#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)

#Merge spatial and aspatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

#Subset for Prince George 
Municp <- subset(census_DAs, census_DAs$CMANAME == "Prince George")

#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```

Before analyzing the data, it is essential to ensure its relevance. Missing data, represented as NA or 0 values, can significantly affect analysis outcomes. To confirm that the polygons contain values for the variables of interest, any polygon with an NA value for either median total income or knowledge of French should be removed.

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#print(colnames(Municp))

#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$`PercFrench`)),]
```

A closer examination of the two variables of interest will be conducted: median total income and the percentage of respondents with French language knowledge. Descriptive statistics will be analyzed, and a final check for NA values in the data will be performed.
```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- skewness(Income_noNA$`Median total income`)

#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$`PercFrench`)
stdevFrench <- sd(French_noNA$`PercFrench`)
skewFrench <- skewness(French_noNA$`PercFrench`)

#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))

#Produce table
kable(data, caption = paste0("Descriptive statistics for selected ", 2016, " census variables"))
```
![Alt text](https://github.com/CSC-370-project/418a3/blob/main/descStats.PNG)


This R code creates two thematic maps using the tmap package, displaying different data for the census dissemination areas in Prince George. The code below compares median total income and the percentage of people with French knowledge in Prince George. 

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Prince George census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
#Choose a pallete
# tmaptools::palette_explorer() #Tool for selecting pallettes

#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "BuGn", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "BuGn", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```
![Alt text](https://github.com/CSC-370-project/418a3/blob/main/censusDisem.png)
## Neighbourhood matrix

A weighted neighborhood matrix describes the relationship between geographic coordinates based on their proximity. Rows and columns in the matrix usually correspond to neighborhoods or census boundaries and the cells contain weight scales corresponding to the strength of the distance metric. The weights matrix also gives programmers control over the influence among neighbors. In queen weighting, all units sharing a boundary or a point have equal influence. In rook weighting only those sharing a boundary count in the calculation. In this tutorial the matrix is not explicitly shown, but is used to produce the map in figure 2. 

Creating a list of neighbors in R is straightforward using the poly2nb() function from the ‘spdep’ package. To switch from the default queen weighting to rook weighting, change ‘queen = TRUE’ to ‘queen = FALSE’.

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
# Use st_coordinates to get the coordinates
Income.net <- nb2lines(Income.nb, coords=st_coordinates(Income_noNA))


#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(Income_noNA))
crs(Income.net2) <- crs(Income_noNA)

#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(French_noNA))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(French_noNA))
crs(French.net2) <- crs(French_noNA)

```

The code below produces a three-paneled map using total median income for the census disseminations in Prince George. Initially the figure title and associated information is setup, next the colors and overlay line weights are setup in the following lines. 

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Prince George census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}

#Make queens map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col="blue") + 
              tm_shape(Income.net) + tm_lines(col="lightgrey")

#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col="blue") + 
              tm_shape(Income.net2) + tm_lines(col="lightgrey", lwd = 2)

#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col="blue") + 
               tm_shape(Income.net) + tm_lines(col="lightgrey", lwd = 2) +
               tm_shape(Income.net2) + tm_lines(col="lightblue", lwd = 2)

#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)

```
![Description of Image](https://github.com/CSC-370-project/418a3/blob/main/neighborMap.png)

The poly2nb() function will identify neighbouring polygons based on the queen weighting criteria. Next the neighbours list is converted into line segments representing connections between neighboring areas. This is done for both French knowledge and median income. 

Weights are categorized by type, specifically "B," "W," and "C." The B weights matrix is the simplest, and used a binary weighting scheme where each neighbor is assigned a weight of 1, and all other polygons are assigned a weight of 0. The W weights matrix uses a row-standardized weighting scheme where equal weights to neighbors that sum to 1. Lastly, the C weights matrix uses a globally standardized method which gives equal weight to all neighbors across the entire study area.


```{r Final_weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

#head(Income.lw[["weights"]])
#head(Income.lw[["weights"]])[c(1:3)]
#Income.lw[["weights"]][1:3]
#library(dplyr)
#slice_head(as.data.frame(Income.lw[["weights"]]), n = 3)
#sapply(Income.lw[["weights"]], `[`, 1:3)

```


## Global Moran’s I

Having described the process for selecting and weighting neighbors, the next step is to calculate the Global Moran's I statistic. This statistic evaluates spatial autocorrelation by examining all locations across the entire study area simultaneously. The formula for this statistic is as follows:

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$
I $I$ is the value of the Global Moran's I statistic, indicating the level of spatial autocorrelation. $n$ is the number of polygons, or regions, and $W_{i,j}$ is the weight assigned to the pair $i, j$ based on their relationship in the weights matrix. In the numerator, the weighted covariance for values at different locations is calculated. If similair values cluster together, the value here will be higher. In the denominator, the weights are summed and multiplied by the total variance of the variable - this standardizes the measure. 


```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)
#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]

# Print the results for Income
print(paste("Global Moran's I for Income: ", mIIncome))
print(paste("Expected Moran's I for Income: ", eIIncome))
print(paste("Variance for Income: ", varIncome))

# Print the results for French
print(paste("Global Moran's I for French: ", mIFrench))
print(paste("Expected Moran's I for French: ", eIFrench))
print(paste("Variance for French: ", varFrench))
```


### Global Moran's $I$ for Income
The Global Moran's $I$ is 0.5898; this value indicates a moderate to strong positive spatial autocorrelation. A possible explanation for this values is because regions with higher median incomes tend to cluster together; that neighboring areas are likely to have similarly incomes. The expected Moran's I is -0.0067; this is the expected value of Moran's I using the null hypothesis that no spatial autocorrelation exists. This value is close to zero and is typical when there is no spatial correlation. The variance is 0.0024; this value indicates the variability of the Global Moran's I statistic. The smaller the variance, the higher the stability of Moran's I for different samples. This is good because it indicates reliable results.

### Global Moran's $I$ for French
The Global Moran's $I$ is 0.1348; this value indicates a weak positive spatial autocorrelation. A possible explanation is due to the fact that areas with a higher percentage of French speakers are clustered, although the clustering is not as pronounced as with median income. The expected Moran's I is -0.0065 which is similar to the income expected $I$ in that it is close to zero. It supports the null hypothesis: that there is no spatial autocorrelation. The variance of 0.0021 is small, which indicates that while the results are consistent for most samples, the clustering is weaker than that of median income.


```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]

rminRange <- round(range[1],4)
rmaxRange <- round(range[2],4)

print(paste("Minimum Range:", rminRange))
print(paste("Maximum Range:", rmaxRange))


```

The minimum range of -0.5703 reveals strong negative spatial autocorrelation exists in some districts; that income is inversely related for select regions because high incomes in one area are surrounded by low incomes. The maximum range of 1.0439 indicates strong positive spatial autocorrelation for some districts where high incomes are clustered together. Because this value is exceptionally high it supports the theory that the data exhibits homogeneity. The wider the range, the more variability exists within the data. In this study the global Moran's $I$ of 0.5898 is closer to the maximum value of the range and supports the theory of strong positive spatial autocorrelation. 


I assess if these patterns are statistically significant a Z-test is used. The null hypothesis is that there is no spatial autocorrelation; that the income and French variables are randomly distributed. The alternate hypothesis is that there is positive spatial autocorrelation. Using an $\alpha$ value of 0.05, if the Z-score falls above 1.96 then there is strong positive spatial autocorrelation, while a value less than -1.96 would imply strong negative spatial autocorrelation. 

To calculate a Z-test, use the following code:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))

rzIncome <- round(zIncome,4)
rzFrench <- round(zFrench,4)

print(paste("Z score Income is: ",rzIncome))
print(paste("Z score French is: ",rzFrench))
  
```

The zscores for both variable confirm that there is strong positive spatial autocorrelation.

## Local spatial autocorrelation

Local spatial autocorrelation examines specific regions within the study area, as opposed to considering the entire study area.


$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} 
$$



```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```


Now going back to our basic mapping template we can visualize some of these results to understand what this test is doing.


```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Prince George census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondents with knowledge of French (right)."}
# Dynamic breaks for Income
income_breaks <- quantile(Income_noNA$Z.Ii, probs = seq(0, 1, 0.25), na.rm = TRUE)

# Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores for Income",
              style = "fixed",
              border.alpha = 0.1,
              breaks = income_breaks,
              palette = "-RdBu", n = 3) +
  tm_compass(position = c("left", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_legend(position = c("right", "top"))

# Dynamic breaks French
french_breaks <- quantile(French_noNA$Z.Ii, probs = seq(0, 1, 0.25), na.rm = TRUE)

# Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores for French",
              style = "fixed",
              border.alpha = 0.1,
              breaks = french_breaks,
              palette = "-RdBu", n = 3) +
  tm_compass(position = c("left", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_legend(position = c("right", "top"))

# Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```
![Description of Image](https://github.com/CSC-370-project/418a3/blob/main/localMoran.png)


### Income
In the map, z-scores for income are clustered together which confirms the global Moran's $I$ results: that areas of high income are surrounded by other areas of high income. Similarly, areas of low income are surrounded by other areas of low income. 

### French
In this map, both the low and high z-scores for French knowledge are clustered relatively close together which indicates variability of French knowledge within the community. This could be the result of boundaries where there are sharp demographic shifts, or simply because of high diversity within the neighbourhood. 


## Scatterplots
Moran's $I$ scatter plots were produced using the following code:

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```
![Description of Image](https://github.com/CSC-370-project/418a3/blob/main/scatterIncome.png)


```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```
![Description of Image](https://github.com/CSC-370-project/418a3/blob/main/scatter.png)


In these plots the points with diamonds are statistically significant. For the income scatterplot, most points are clustered around the regression line and near the top left quadrant which means there is positive spatial correlation. While there are some points in the lower left, this still suggests positive spatial autocorrelation. For the French knowledge scatterplot, most points are moderately clustered around the lower left quandrant, which suggests positive spatial autocorrelation, with a specific trend. This supports the local Moran's $I$ results where high and low z-scores were clustered together and implies sharp demographic shifts and high diversity within the neighbourhood. 


## Summary

The global Moran's $I$ for incomes was 0.5898 which is indicative of moderate positive spatial autocorrelation. The local Moran's $I$ also revealed clusters of high income earners within the census districts, while the scatterplot displayed a strong trend of clustering in the upper right corner along the regression line, which indicates positive spatial autocorrelation. From this is it was concluded that high income earners tend to live near other high income earners in Prince George. The global Moran's $I$ for French knowledge was 0.1348 which is indicative of  spatial autocorrelation, although it is weaker than the result for incomes. The local Moran's $I$ revealed a pattern where both high and low knowledge of French was clustered together within nearby census districts, while the scatterplot displayed a moderate trend of clustering in the lower left corner along the regression line, which indicates positive spatial autocorrelation. From this it was concluded that there are sharp shifts in French knowledge within the community, although the exact reason for this is unknown. 

## References

(1) Statistics Canada. (2021). The changing geography of Canada’s population: 2021 Census (Catalogue No. 92-160-G2021002). Government of Canada. https://www150.statcan.gc.ca/n1/en/pub/92-160-g/92-160-g2021002-eng.pdf?st=uUM4ntq8

(2) Statistics Canada. (2021). 2021 census of population: Income in Canada: Key results from the 2021 Census. https://www150.statcan.gc.ca/n1/pub/92-160-g/92-160-g2021002-eng.pdf

(3) Bone, Chris. (2024) Assignment 3. Brightspace. https://bright.uvic.ca/d2l/lms/dropbox/user/folder_submit_files.d2l?ou=357893&db=172723
