## This file creates a bivariate choropleth map of positive cases of COVID 19 against the number of tests administered per 
#county in Texas. 

library(rstudioapi)
library(tidyverse) 
library(magrittr)
library(lintr) 
library(sf) 
library(raster) 
library(viridis) 
library(cowplot)
library(latticeExtra)
library(maps)
require(classInt) 
require(grid) 
library(pals)
library(usmap)

## References: https://cran.r-project.org/web/packages/pals/vignettes/bivariate_choropleths.html
#             https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/ 


## Positive cases data 
cases_count_url <-'https://www.dshs.texas.gov/coronavirus/TexasCOVID19DailyCountyCaseCountData.xlsx'
p1f <- tempfile()
download.file(cases_count_url, p1f, mode="wb")
p1<-read_excel(path = p1f)

weekcasecount <- as.vector(as.numeric(p1$...102[3:256])) +  
                as.vector(as.numeric(p1$...103[3:256])) + 
                as.vector(as.numeric(p1$...104[3:256])) + 
               as.vector(as.numeric(p1$...105[3:256])) +
               as.vector(as.numeric(p1$...106[3:256])) +  
               as.vector(as.numeric(p1$...107[3:256])) + 
               as.vector(as.numeric(p1$...108[3:256])) 
#this provides the total number of cases per county in the week of 6/14 - 6/20

## Testing data 
tests_url <-'https://www.dshs.texas.gov/coronavirus/TexasCOVID-19CumulativeTestsOverTimebyCounty.xlsx'
p4f <- tempfile()
download.file(tests_url, p4f, mode="wb")
p4 <-read_excel(path = p4f)
weektestcount <- as.vector(as.numeric(p4$...62[2:255]))  - 
                  as.vector(as.numeric(p4$...55[2:255])) 
# this provides the total number of tests per county in the week of 6/14 - 6/20


## Counties 
mapz <- map("county",'texas', plot = FALSE, fill = TRUE,
            projection = "tetra")
counties <- mapz$names
counties <- counties[-85] # to remove repeated Galveston county


cols <- stevens.greenblue
# for other colors, https://www.rdocumentation.org/packages/pals/versions/1.6/topics/bivariate

nbins <- 3   # number of bins for each variable 
brks_cases <- classIntervals(weekcasecount, n=nbins, style='quantile')
brks_tests <- classIntervals(weektestcount, n=nbins, style='quantile')
class_cases <- findCols(brks_cases)
class_tests <- findCols(brks_tests)
class2 <- class_cases + nbins*(class_tests-1)
class2 <- as.numeric(class2)
# assigns each county a "bin", bins 1-9, based on their cases, test numbers 

tests_cases_data <- data.frame(counties, weekcasecount, weektestcount, class2) 

## Creating map
m3 <- mapplot(tests_cases_data$counties ~ as.numeric(tests_cases_data$class2), data = tests_cases_data,
              colramp=cols, breaks=seq(from=0.5, by=1, length=nbins*nbins+1),
              xlab="",
              colorkey=FALSE,
              map = map("county","texas", plot = FALSE, fill = TRUE,
                        projection = "tetra"),
              scales = list(draw = FALSE))
print( m3 )

## Creating legend and title 
m3leg <- levelplot(matrix(1:(nbins*nbins), nrow=nbins), axes=FALSE, col.regions=cols(),
                   xlab="cases  -->", ylab="tests  -->", cuts=8, colorkey=FALSE,
                   scales=list(draw=0))
vp <- viewport(x=.3, y=.25, width=.2, height=.2)
pushViewport(vp)
print(m3leg, newpage=FALSE)
popViewport()

vp <- viewport(x=.5, y=.93, width=.2, height=.2)
pushViewport(vp)
grid.text("Bivariate Plot of Texas Counties' Testing and Positive Cases for 6/14 - 6/20")
popViewport()


