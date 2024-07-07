
1.  Done below. No missing values. However, we need to convert East
    column into a factor variable.

``` r
restaurants_df <- read.csv("nyc-italian-cheap.csv", sep="\t")
```

``` r
nrow(restaurants_df)
```

    ## [1] 168

``` r
ncol(restaurants_df)
```

    ## [1] 6

``` r
sapply(restaurants_df, class)
```

    ##  Restaurant        Food       Decor     Service        East       Cheap 
    ## "character"   "integer"   "integer"   "integer"   "integer"   "integer"

``` r
sum(is.na(restaurants_df))
```

    ## [1] 0

``` r
restaurants_df[restaurants_df == ""] <- NA 
sum(is.na(restaurants_df))
```

    ## [1] 0

``` r
summary(restaurants_df)
```

    ##   Restaurant             Food          Decor          Service    
    ##  Length:168         Min.   :16.0   Min.   : 6.00   Min.   :14.0  
    ##  Class :character   1st Qu.:19.0   1st Qu.:16.00   1st Qu.:18.0  
    ##  Mode  :character   Median :20.5   Median :18.00   Median :20.0  
    ##                     Mean   :20.6   Mean   :17.69   Mean   :19.4  
    ##                     3rd Qu.:22.0   3rd Qu.:19.00   3rd Qu.:21.0  
    ##                     Max.   :25.0   Max.   :25.00   Max.   :24.0  
    ##       East           Cheap       
    ##  Min.   :0.000   Min.   :0.0000  
    ##  1st Qu.:0.000   1st Qu.:0.0000  
    ##  Median :1.000   Median :0.0000  
    ##  Mean   :0.631   Mean   :0.3929  
    ##  3rd Qu.:1.000   3rd Qu.:1.0000  
    ##  Max.   :1.000   Max.   :1.0000

2.  Because the outcome variable is a binary variable and can have two
    and only two distinct values 0 and 1, we will model the probability
    using logistic regression. In other words, since we are trying to
    predict whether a restaurant is cheap or not, there are only two
    associated values for the outcome variable. Cheap is 1 and not cheap
    or expensive is 0. Logistic is best suited for the same reason.

We convert East into a factor variable. Numeric operations do not make
sense on ‘East’ as it refers to a location based category.

``` r
restaurants_df$East <- as.factor(restaurants_df$East)
class(restaurants_df$East)
```

    ## [1] "factor"

3.  Done below. Explanation follows the code.

``` r
log_m <- glm(Cheap ~ Food + Decor + Service + East, data = restaurants_df, family=binomial())
summary(log_m)
```

    ## 
    ## Call:
    ## glm(formula = Cheap ~ Food + Decor + Service + East, family = binomial(), 
    ##     data = restaurants_df)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) 19.44989    3.19810   6.082 1.19e-09 ***
    ## Food        -0.47434    0.20283  -2.339   0.0194 *  
    ## Decor       -0.63285    0.14141  -4.475 7.63e-06 ***
    ## Service      0.04739    0.19262   0.246   0.8057    
    ## East1       -0.33058    0.43586  -0.758   0.4482    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 225.12  on 167  degrees of freedom
    ## Residual deviance: 139.70  on 163  degrees of freedom
    ## AIC: 149.7
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
library(margins)
me <- margins(log_m)
summary(me)
```

    ##   factor     AME     SE       z      p   lower   upper
    ##    Decor -0.0850 0.0145 -5.8411 0.0000 -0.1135 -0.0564
    ##    East1 -0.0449 0.0595 -0.7536 0.4511 -0.1616  0.0718
    ##     Food -0.0637 0.0256 -2.4859 0.0129 -0.1139 -0.0135
    ##  Service  0.0064 0.0258  0.2461 0.8056 -0.0443  0.0570

We have assumed 4 explanatory variables - Food, Service, Decor and
East/location. Since we performed logistic regression, we will use the
marginal effect to explain the results. Also, as AME is the Average
Marginal Effect, it’s necessary to keep in mind that these estimates are
all ‘averages’ because the probability will vary unequally (logistic
function is not linear) for an increase of 1 unit in the explanatory
variable.

Firstly, we can say that increasing the decor review by 1 unit decreases
the probability of the restaurant being cheap by 8.5% on an average.
Similarly, if we increase the food review by 1 unit, the probability of
cheap restaurant goes down by 6.37% on an average. Both the above
results are statistically significant as the p value is low enough,
hence we can reject the corresponding null hypothesis.

Moreover, we have two statistically insignificant results. Their
insignificance is confirmed by the high p value (0.45 and 0.8). One is
that for a restaurant located east of the 5th Ave, probability of it
being cheap goes down by 4.49%. Second is if we increase service review
by 1 unit, probability of that restaurant being cheap increases by
0.64%. The latter is non intuitive and statistically insignificant. We
can discard both from our results.

I believe the overall results align with common sense. Given that we
increase decor in any restaurant, it is likely an expensive restaurant
and hence the Probability of finding a cheap restaurant diminishes.
Expensive restaurants like to work on ambiance to provide an overall
customer experience and do not limit themselves to prioritizing just
delicious food. Also, it is probable that cheaper restaurants compromise
on key aspects such as food quality to cut costs, prompting customers to
give poor ‘food’ reviews. Hence, our model states that improving food
reviews decreases the probability that the restaurant is cheap.

4.  Done below. Analysis follows the code.

``` r
#creating a data frame for our different restaurant data
food <- c(23, 18)
decor <- c(17, 15)
service <- c(22,24)
east <- c(0,1)
```

``` r
newdata <- data.frame(Food=food, Decor=decor, Service=service, East=east)
newdata$East <- as.factor(newdata$East) #converting East into a factor variable
predict(log_m, newdata, type="response")
```

    ##         1         2 
    ## 0.2358072 0.9025400

``` r
predict(log_m, newdata, type="response") > 0.5
```

    ##     1     2 
    ## FALSE  TRUE

First we create a dataframe ‘newdata’ to store review data for the two
new restaurants. Then, we convert East into a factor variable and use
predict() to ask our model to work on this new dataset and generate
probability whether the restaurant is cheap or not.

The probability that the first restaurant (Assagio Ristorante) is cheap
is ~0.236, so we can classify it as an expensive restaurant. For Altura,
this probability is 0.902 and we classify it as a cheap restaurant.

Conclusion - Our model predicts that Altura is a cheap restaurant while
Assagio Ristorante is not.
