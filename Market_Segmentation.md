``` r
library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: lattice

    ## Loading required package: ggformula

    ## 
    ## New to ggformula?  Try the tutorials: 
    ##  learnr::run_tutorial("introduction", package = "ggformula")
    ##  learnr::run_tutorial("refining", package = "ggformula")

    ## Loading required package: mosaicData

    ## 
    ## The 'mosaic' package masks several functions from core packages in order to add 
    ## additional features.  The original behavior of these functions should not be affected by this.
    ## 
    ## Note: If you use the Matrix package, be sure to load it BEFORE loading mosaic.

    ## 
    ## Attaching package: 'mosaic'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     count, do, tally

    ## The following objects are masked from 'package:stats':
    ## 
    ##     binom.test, cor, cor.test, cov, fivenum, IQR, median,
    ##     prop.test, quantile, sd, t.test, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     max, mean, min, prod, range, sample, sum

``` r
social = read.csv('http://raw.githubusercontent.com/jgscott/STA380/master/data/social_marketing.csv', header=TRUE)
head(social[,-1],5)
```

    ##   chatter current_events travel photo_sharing uncategorized tv_film
    ## 1       2              0      2             2             2       1
    ## 2       3              3      2             1             1       1
    ## 3       6              3      4             3             1       5
    ## 4       1              5      2             2             0       1
    ## 5       5              2      0             6             1       0
    ##   sports_fandom politics food family home_and_garden music news
    ## 1             1        0    4      1               2     0    0
    ## 2             4        1    2      2               1     0    0
    ## 3             0        2    1      1               1     1    1
    ## 4             0        1    0      1               0     0    0
    ## 5             0        2    0      1               0     0    0
    ##   online_gaming shopping health_nutrition college_uni sports_playing
    ## 1             0        1               17           0              2
    ## 2             0        0                0           0              1
    ## 3             0        2                0           0              0
    ## 4             0        0                0           1              0
    ## 5             3        2                0           4              0
    ##   cooking eco computers business outdoors crafts automotive art religion
    ## 1       5   1         1        0        2      1          0   0        1
    ## 2       0   0         0        1        0      2          0   0        0
    ## 3       2   1         0        0        0      2          0   8        0
    ## 4       0   0         0        1        0      3          0   2        0
    ## 5       1   0         1        0        1      0          0   0        0
    ##   beauty parenting dating school personal_fitness fashion small_business
    ## 1      0         1      1      0               11       0              0
    ## 2      0         0      1      4                0       0              0
    ## 3      1         0      1      0                0       1              0
    ## 4      1         0      0      0                0       0              0
    ## 5      0         0      0      0                0       0              1
    ##   spam adult
    ## 1    0     0
    ## 2    0     0
    ## 3    0     0
    ## 4    0     0
    ## 5    0     0

``` r
#scale the data
X = social[,-1]
X = X/rowSums(X)
```

We decided to scale the data according to their row sums because we want all users' interests to be equally weighted regardless of their overall level of twitter usage.

Kmeans method:
==============

``` r
#test various k values
CH_list = {}
kval = c(2,4,6,10,15)
for (i in kval){
  # Using kmeans++ initialization
  cluster = kmeanspp(X, k=i, nstart=25)
  W = cluster$tot.withinss
  B = cluster$betweenss
  nk = (nrow(social)-i)/(i-1)
  val = (B/W)*nk
  CH_list = c(CH_list,val)
  cat(c(i," "))
}
```

    ## 2  4  6  10  15

``` r
#plot CH index againt k value to find best k
plot(kval,CH_list)
```

![](Market_Segmentation_files/figure-markdown_github/kmeans%20method-1.png)

The plot of CH index against K value, we decided that the best k value is 6. The max CH value occurs when k = 2, but since our dataset is so large, it makes sense to split into more than 2 clusters. At a k value of 6, the CH index is high, but we are clustering the data into smaller subsets, so we chose to work with k = 6 clusters.

``` r
#run kmeans to cluster the data
set.seed(11)
finalcluster = kmeanspp(X, k=6, nstart=25)

finalcluster$size
```

    ## [1]  789 2069  610 1946 1349 1119

``` r
finalcluster$centers[2,]
```

    ##          chatter   current_events           travel    photo_sharing 
    ##     2.339924e-01     5.633070e-02     3.593937e-02     1.175645e-01 
    ##    uncategorized          tv_film    sports_fandom         politics 
    ##     2.567276e-02     2.362182e-02     2.519554e-02     2.949261e-02 
    ##             food           family  home_and_garden            music 
    ##     1.902771e-02     2.045199e-02     1.568317e-02     1.843638e-02 
    ##             news    online_gaming         shopping health_nutrition 
    ##     1.406586e-02     1.523592e-02     6.777381e-02     2.145805e-02 
    ##      college_uni   sports_playing          cooking              eco 
    ##     2.266085e-02     1.401526e-02     1.938834e-02     1.489493e-02 
    ##        computers         business         outdoors           crafts 
    ##     1.402210e-02     1.265952e-02     9.474187e-03     1.268036e-02 
    ##       automotive              art         religion           beauty 
    ##     1.886850e-02     1.049689e-02     1.102066e-02     9.606155e-03 
    ##        parenting           dating           school personal_fitness 
    ##     1.236559e-02     1.691043e-02     1.500503e-02     1.719061e-02 
    ##          fashion   small_business             spam            adult 
    ##     1.460953e-02     9.441356e-03     7.020247e-05     4.676975e-03

``` r
finalcluster$centers[4,]
```

    ##          chatter   current_events           travel    photo_sharing 
    ##     0.0816242872     0.0492912467     0.0349430699     0.0434273869 
    ##    uncategorized          tv_film    sports_fandom         politics 
    ##     0.0266437055     0.0447842182     0.0729954519     0.0204538113 
    ##             food           family  home_and_garden            music 
    ##     0.0579891985     0.0335093834     0.0156797854     0.0208438668 
    ##             news    online_gaming         shopping health_nutrition 
    ##     0.0183975036     0.0160780634     0.0237825555     0.0214936595 
    ##      college_uni   sports_playing          cooking              eco 
    ##     0.0288050578     0.0148722498     0.0190312471     0.0130273344 
    ##        computers         business         outdoors           crafts 
    ##     0.0128717928     0.0121358616     0.0133053981     0.0172850646 
    ##       automotive              art         religion           beauty 
    ##     0.0176483138     0.0313111823     0.0579849730     0.0160396928 
    ##        parenting           dating           school personal_fitness 
    ##     0.0437419395     0.0190613783     0.0319351484     0.0166516796 
    ##          fashion   small_business             spam            adult 
    ##     0.0166442516     0.0107877632     0.0003340708     0.0245884068

The largest clusters are clusters 2 and 4, so we can look at the top topics of interest in the clusters to learn about many of the followers. Cluster 2 displays an interest in photo sharing, shopping, current events, and travel. Cluster 4 shows interest in sports fandom, food, and religion. We can use this information to tailor our message in our marketing campaign.

``` r
#plot to show our target influencers
qplot(health_nutrition, food, data=X, color=factor(finalcluster$cluster))
```

![](Market_Segmentation_files/figure-markdown_github/unnamed-chunk-2-1.png)

After looking at the 6 clusters, we plotted the clusters against health\_nutrition and food because we determined that these are the topics that most relate to nutrientH2O. We found that groups 4 and 5 show a high level of interest in these topics, so we can find the influencers among these groups to help us campaign about NutrientH2O.

PCA method:
===========

``` r
# PCA
set.seed(12)
PC = prcomp(X, scale=TRUE)
loadings = PC$rotation
scores = PC$x


# Show the top topics associated with each component
o1 = order(loadings[,1], decreasing = TRUE)
colnames(X)[head(o1,6)]
```

    ## [1] "religion"      "sports_fandom" "parenting"     "food"         
    ## [5] "school"        "family"

``` r
colnames(X)[tail(o1,6)]
```

    ## [1] "college_uni"   "fashion"       "cooking"       "shopping"     
    ## [5] "chatter"       "photo_sharing"

``` r
o2 = order(loadings[,2], decreasing = TRUE)
colnames(X)[head(o2,6)]
```

    ## [1] "chatter"        "politics"       "travel"         "shopping"      
    ## [5] "automotive"     "current_events"

``` r
colnames(X)[tail(o2,6)]
```

    ## [1] "beauty"           "fashion"          "cooking"         
    ## [4] "outdoors"         "personal_fitness" "health_nutrition"

``` r
#plot the data on PC1 and PC2
qplot(scores[,1], scores[,2], xlab='Component 1', ylab='Component 2')
```

![](Market_Segmentation_files/figure-markdown_github/PCA%20method-1.png)

``` r
#plot all vectors
biplot(PC)
```

![](Market_Segmentation_files/figure-markdown_github/PCA%20method-2.png)

Because health\_nutrition is the lowest loading in PC2 and food is high in PC1, we want to target the users whose PC2 is low and PC1 is high. These users are plotted in the bottom region slightly to the right in the biplot shown.
