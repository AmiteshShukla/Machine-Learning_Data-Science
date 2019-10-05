BoxCox\_Music\_Data\_Amitesh\_Shukla
================
Amitesh Shukla
3/17/2018

## Predict longitude and latitude from the music dataset at <https://archive.ics.uci.edu/ml/datasets/Geographical+Original+of+Music>

## Simple Linear Regression Model

``` r
#latitude r squared
summary(modellat)$r.squared
```

    ## [1] 0.2928092

``` r
#longitude r squared
summary(modellong)$r.squared
```

    ## [1] 0.3645767

``` r
plot(modellat$fitted.values,modellat$residuals, type = "p",pch=1, col="darkblue",
     main = "residuals vs fitted values - latitude")
abline(a=0,b=0, lty="dotted")
```

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
plot(modellong$fitted.values,modellong$residuals, type = "p",pch=1, col="red",
     main = "residuals vs fitted values - longitude")
abline(a=0,b=0, lty="dotted")
```

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Withh BoxCox transformation

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

## identified lambda for latitude based on log-likelihood:

    ## [1] 3.575758

## identified lambda for longitude based on log-likelihood:

    ## [1] 1.090909

``` r
#latitude r squared
summary(modeltlat)$r.squared
```

    ## [1] 0.3269483

``` r
#longitude r squared
summary(modeltlong)$r.squared
```

    ## [1] 0.3652043

## The box cox transformation looks to improve the regression. As per the below table r-squred values for latitude and longitude seem to improve after the transformation. The box cox transformation on latitude shows major improvement as compared to the longitude. The plot of residulas against fitted values too show the transformation of latitude results in slight decrease in the residuals.

## Original BoxCox Transformed

## Latitude: 0.2928092 0.3269483

## Longitude: 0.3645767 0.3652043

## L2 regularized regression of music dataset

    ## Loading required package: Matrix

    ## Loading required package: foreach

    ## Loaded glmnet 2.0-18

    ## [1] 20.56023

    ## [1] 0.5124197

``` r
library(glmnet)
#par(mfrow=c(1, 2))
modellatcvridge <- cv.glmnet(x=as.matrix(data.tlat[,-117]), y=data.tlat[,117], nfold=10, alpha=0)
modellongcvridge <- cv.glmnet(x=as.matrix(data.tlong[,-117]), y=data.tlong[,117], nfold=10, alpha=0)

plot(modellatcvridge, xlab="lambda - Latitude Model")
```

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
plot(modellongcvridge, xlab="lambda - Longitude Model")
```

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

## Minimum lambda and lambda under 1 standard deviation for latitude with

## ridge regression

``` r
modellatcvridge$lambda.min
```

    ## [1] 1.784949

``` r
modellatcvridge$lambda.1se
```

    ## [1] 2.589655

\#\#minimum mean square
error

``` r
mse.min
```

    ## [1] 24.0242

## Minimum lambda and lambda under 1 standard deviation for longitude with

## ridge regression(L2 regularization)

``` r
modellongcvridge$lambda.min
```

    ## [1] 5.039034

``` r
modellongcvridge$lambda.1se
```

    ## [1] 5.039034

## minimum mean square error

``` r
mse.min
```

    ## [1] 31.59896

## L1 regularized regression of music dataset

``` r
modellatcvlasso <- cv.glmnet(x=as.matrix(data.tlat[,-117]), y=data.tlat[,117], nfold= 10, alpha=1)
modellongcvlasso <- cv.glmnet(x=as.matrix(data.tlong[,-117]), y=data.tlong[,117], nfold= 10, alpha=1)

plot(modellatcvlasso, xlab="lambda - Latitude Model")
```

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
plot(modellongcvlasso, xlab="lambda - Longitude Model")
```

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-23-2.png)<!-- -->

## Minimum lambda and lambda under 1 standard deviation for latitude with

## lasso regression(L1 regularization) - Latitude

``` r
modellatcvlasso$lambda.min
```

    ## [1] 0.08094526

``` r
modellatcvlasso$lambda.1se
```

    ## [1] 0.4319805

\#\#minimum mean square error

``` r
mse.min
```

    ## [1] 19.27683

## Minimum and lambda under 1 standard deviation for longitude with

## lasso regression(L1 regularization) - Longitude

``` r
modellongcvlasso$lambda.min
```

    ## [1] 1.338412

``` r
modellongcvlasso$lambda.1se
```

    ## [1] 1.338412

\#\#minimum mean square error

``` r
mse.min
```

    ## [1] 2.390837

``` r
coef(modellatcvlasso)
```

    ## 118 x 1 sparse Matrix of class "dgCMatrix"
    ##                         1
    ## (Intercept)  7.825800e+01
    ## V1           .           
    ## V2           .           
    ## V3           .           
    ## V4           .           
    ## V5           .           
    ## V6           .           
    ## V7           .           
    ## V8           .           
    ## V9           .           
    ## V10          .           
    ## V11          .           
    ## V12          .           
    ## V13          .           
    ## V14          .           
    ## V15          .           
    ## V16          .           
    ## V17         -2.036571e-01
    ## V18         -4.146414e-01
    ## V19         -2.839867e-07
    ## V20         -2.503463e-16
    ## V21          .           
    ## V22          .           
    ## V23          .           
    ## V24          .           
    ## V25          .           
    ## V26          .           
    ## V27          .           
    ## V28          .           
    ## V29          .           
    ## V30          .           
    ## V31          .           
    ## V32          .           
    ## V33          .           
    ## V34          .           
    ## V35          .           
    ## V36          .           
    ## V37          .           
    ## V38          .           
    ## V39          .           
    ## V40          .           
    ## V41          .           
    ## V42          .           
    ## V43          .           
    ## V44          .           
    ## V45          .           
    ## V46          .           
    ## V47          .           
    ## V48          .           
    ## V49          .           
    ## V50          .           
    ## V51          .           
    ## V52          .           
    ## V53          .           
    ## V54          .           
    ## V55          .           
    ## V56          .           
    ## V57          .           
    ## V58          .           
    ## V59          .           
    ## V60          .           
    ## V61          .           
    ## V62          .           
    ## V63          .           
    ## V64          .           
    ## V65          .           
    ## V66          .           
    ## V67          .           
    ## V68          .           
    ## V69          .           
    ## V70          .           
    ## V71          .           
    ## V72          .           
    ## V73          .           
    ## V74          .           
    ## V75          .           
    ## V76          .           
    ## V77          .           
    ## V78          .           
    ## V79          .           
    ## V80          .           
    ## V81          .           
    ## V82          .           
    ## V83          .           
    ## V84          .           
    ## V85          .           
    ## V86          .           
    ## V87          .           
    ## V88          .           
    ## V89          .           
    ## V90          .           
    ## V91         -9.621707e-01
    ## V92          .           
    ## V93          .           
    ## V94          .           
    ## V95          .           
    ## V96          .           
    ## V97          .           
    ## V98          .           
    ## V99          .           
    ## V100         .           
    ## V101         .           
    ## V102         .           
    ## V103         .           
    ## V104         .           
    ## V105         .           
    ## V106         .           
    ## V107         .           
    ## V108         .           
    ## V109         .           
    ## V110         .           
    ## V111         .           
    ## V112         .           
    ## V113         .           
    ## V114         .           
    ## V115         .           
    ## V116         .           
    ## lt           5.048883e-06

``` r
coef(modellongcvlasso)
```

    ## 118 x 1 sparse Matrix of class "dgCMatrix"
    ##                      1
    ## (Intercept) 23.1094251
    ## V1           .        
    ## V2           .        
    ## V3           .        
    ## V4           .        
    ## V5           .        
    ## V6           .        
    ## V7           .        
    ## V8           .        
    ## V9           .        
    ## V10          .        
    ## V11          .        
    ## V12          .        
    ## V13          .        
    ## V14          .        
    ## V15          .        
    ## V16          .        
    ## V17          .        
    ## V18          .        
    ## V19          .        
    ## V20          .        
    ## V21          .        
    ## V22          .        
    ## V23          .        
    ## V24          .        
    ## V25          .        
    ## V26          .        
    ## V27          .        
    ## V28          .        
    ## V29          .        
    ## V30          .        
    ## V31          .        
    ## V32          .        
    ## V33          .        
    ## V34          .        
    ## V35          .        
    ## V36          .        
    ## V37          .        
    ## V38          .        
    ## V39          .        
    ## V40          .        
    ## V41          .        
    ## V42          .        
    ## V43          .        
    ## V44          .        
    ## V45          .        
    ## V46          .        
    ## V47          .        
    ## V48          .        
    ## V49          .        
    ## V50          .        
    ## V51          .        
    ## V52          .        
    ## V53          .        
    ## V54          .        
    ## V55          .        
    ## V56          .        
    ## V57          .        
    ## V58          .        
    ## V59          .        
    ## V60          .        
    ## V61          .        
    ## V62          .        
    ## V63          .        
    ## V64          .        
    ## V65          .        
    ## V66          .        
    ## V67          .        
    ## V68          .        
    ## V69          .        
    ## V70          .        
    ## V71          .        
    ## V72          .        
    ## V73          .        
    ## V74          .        
    ## V75          .        
    ## V76          .        
    ## V77          .        
    ## V78          .        
    ## V79          .        
    ## V80          .        
    ## V81          .        
    ## V82          .        
    ## V83          .        
    ## V84          .        
    ## V85          .        
    ## V86          .        
    ## V87          .        
    ## V88          .        
    ## V89          .        
    ## V90          .        
    ## V91          .        
    ## V92          .        
    ## V93          .        
    ## V94          .        
    ## V95          .        
    ## V96          .        
    ## V97          .        
    ## V98          .        
    ## V99          .        
    ## V100         .        
    ## V101         .        
    ## V102         .        
    ## V103         .        
    ## V104         .        
    ## V105         .        
    ## V106         .        
    ## V107         .        
    ## V108         .        
    ## V109         .        
    ## V110         .        
    ## V111         .        
    ## V112         .        
    ## V113         .        
    ## V114         .        
    ## V115         .        
    ## V116         .        
    ## ln           0.5978622

## Elastic Net regularized regression of music dataset

``` r
elastic_net<-function(x) {
      modelenlat <- cv.glmnet(x=as.matrix(data.tlat[,-117]),
                              y=data.tlat[,117], nfold=10, alpha=x)
      i <- which(modelenlat$lambda == modelenlat$lambda.min)
      
      modelenlong <- cv.glmnet(x=as.matrix(data.tlong[,-117]),
                               y=data.tlong[,117], alpha=x)
      j <- which(modelenlong$lambda == modelenlong$lambda.min)
      #minimum mean square error
      mse.min.long <- modelenlong$cvm[j]
      #minimum mean square error
      mse.min <- modelenlat$cvm[i]
      cat("Latitude: MSE for alpha",x,"is",mse.min,"\n")
      cat("Latitude: lambda.min for alpha",x,"is",
          modelenlat$lambda.min,"\n")
      cat("Latitude: lambda.1se for alpha",x,"is",
          modelenlat$lambda.1se,"\n")
      plot(modelenlat, xlab="lambda - Latitude Model")
      coef(modelenlat)
      cat("Longitude:MSE for alpha",x,"is",mse.min.long,"\n")
      cat("Longitude: lambda.min for alpha",x,"is",
          modelenlong$lambda.min,"\n")
      cat("Longitude: lambda.1se for alpha",x,"is",
          modelenlong$lambda.1se,"\n")
      plot(modelenlong, xlab="lambda - Longitude Model")
      coef(modelenlong)
}
#par(mfrow=c(1, 2))
elastic_net(0.2)
```

    ## Latitude: MSE for alpha 0.2 is 19.4097 
    ## Latitude: lambda.min for alpha 0.2 is 0.2110246 
    ## Latitude: lambda.1se for alpha 0.2 is 1.126175

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

    ## Longitude:MSE for alpha 0.2 is 2.399646 
    ## Longitude: lambda.min for alpha 0.2 is 1.37623 
    ## Longitude: lambda.1se for alpha 0.2 is 1.37623

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->

    ## 118 x 1 sparse Matrix of class "dgCMatrix"
    ##                         1
    ## (Intercept)  2.343511e+01
    ## V1           .           
    ## V2           .           
    ## V3           .           
    ## V4           .           
    ## V5           .           
    ## V6           .           
    ## V7           .           
    ## V8           .           
    ## V9           .           
    ## V10          .           
    ## V11          .           
    ## V12          .           
    ## V13          .           
    ## V14          .           
    ## V15          .           
    ## V16          .           
    ## V17          .           
    ## V18          .           
    ## V19          .           
    ## V20          .           
    ## V21          .           
    ## V22          .           
    ## V23          .           
    ## V24          .           
    ## V25          .           
    ## V26          .           
    ## V27          .           
    ## V28          .           
    ## V29          .           
    ## V30          .           
    ## V31          .           
    ## V32         -2.166505e-01
    ## V33          .           
    ## V34          .           
    ## V35          .           
    ## V36          .           
    ## V37          .           
    ## V38          .           
    ## V39          .           
    ## V40          .           
    ## V41          .           
    ## V42          .           
    ## V43          .           
    ## V44          .           
    ## V45          .           
    ## V46          .           
    ## V47         -1.861568e-03
    ## V48         -6.336247e-05
    ## V49         -1.858429e-06
    ## V50         -5.050275e-08
    ## V51         -1.309992e-09
    ## V52         -3.293013e-11
    ## V53         -8.092333e-13
    ## V54         -7.025316e-03
    ## V55         -2.631613e-02
    ## V56         -2.849027e-02
    ## V57         -2.566777e-02
    ## V58         -2.131217e-02
    ## V59          .           
    ## V60          .           
    ## V61          .           
    ## V62          .           
    ## V63          .           
    ## V64          .           
    ## V65          .           
    ## V66          .           
    ## V67          .           
    ## V68          .           
    ## V69          .           
    ## V70          .           
    ## V71          .           
    ## V72          .           
    ## V73          .           
    ## V74          .           
    ## V75          .           
    ## V76          .           
    ## V77          .           
    ## V78          .           
    ## V79          .           
    ## V80          .           
    ## V81          .           
    ## V82          .           
    ## V83          .           
    ## V84          .           
    ## V85          .           
    ## V86          .           
    ## V87          .           
    ## V88          .           
    ## V89          .           
    ## V90          .           
    ## V91          .           
    ## V92          .           
    ## V93          .           
    ## V94          .           
    ## V95          .           
    ## V96          .           
    ## V97          .           
    ## V98          .           
    ## V99          .           
    ## V100         .           
    ## V101         .           
    ## V102         .           
    ## V103         .           
    ## V104         .           
    ## V105         .           
    ## V106         .           
    ## V107         .           
    ## V108         .           
    ## V109         .           
    ## V110         .           
    ## V111         .           
    ## V112         .           
    ## V113         .           
    ## V114         .           
    ## V115         .           
    ## V116         .           
    ## ln           5.968452e-01

``` r
elastic_net(0.6)
```

    ## Latitude: MSE for alpha 0.6 is 18.91106 
    ## Latitude: lambda.min for alpha 0.6 is 0.1120036 
    ## Latitude: lambda.1se for alpha 0.6 is 0.9517547

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-32-3.png)<!-- -->

    ## Longitude:MSE for alpha 0.6 is 2.527737 
    ## Longitude: lambda.min for alpha 0.6 is 1.400936 
    ## Longitude: lambda.1se for alpha 0.6 is 1.400936

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-32-4.png)<!-- -->

    ## 118 x 1 sparse Matrix of class "dgCMatrix"
    ##                      1
    ## (Intercept) 23.2967744
    ## V1           .        
    ## V2           .        
    ## V3           .        
    ## V4           .        
    ## V5           .        
    ## V6           .        
    ## V7           .        
    ## V8           .        
    ## V9           .        
    ## V10          .        
    ## V11          .        
    ## V12          .        
    ## V13          .        
    ## V14          .        
    ## V15          .        
    ## V16          .        
    ## V17          .        
    ## V18          .        
    ## V19          .        
    ## V20          .        
    ## V21          .        
    ## V22          .        
    ## V23          .        
    ## V24          .        
    ## V25          .        
    ## V26          .        
    ## V27          .        
    ## V28          .        
    ## V29          .        
    ## V30          .        
    ## V31          .        
    ## V32          .        
    ## V33          .        
    ## V34          .        
    ## V35          .        
    ## V36          .        
    ## V37          .        
    ## V38          .        
    ## V39          .        
    ## V40          .        
    ## V41          .        
    ## V42          .        
    ## V43          .        
    ## V44          .        
    ## V45          .        
    ## V46          .        
    ## V47          .        
    ## V48          .        
    ## V49          .        
    ## V50          .        
    ## V51          .        
    ## V52          .        
    ## V53          .        
    ## V54          .        
    ## V55          .        
    ## V56          .        
    ## V57          .        
    ## V58          .        
    ## V59          .        
    ## V60          .        
    ## V61          .        
    ## V62          .        
    ## V63          .        
    ## V64          .        
    ## V65          .        
    ## V66          .        
    ## V67          .        
    ## V68          .        
    ## V69          .        
    ## V70          .        
    ## V71          .        
    ## V72          .        
    ## V73          .        
    ## V74          .        
    ## V75          .        
    ## V76          .        
    ## V77          .        
    ## V78          .        
    ## V79          .        
    ## V80          .        
    ## V81          .        
    ## V82          .        
    ## V83          .        
    ## V84          .        
    ## V85          .        
    ## V86          .        
    ## V87          .        
    ## V88          .        
    ## V89          .        
    ## V90          .        
    ## V91          .        
    ## V92          .        
    ## V93          .        
    ## V94          .        
    ## V95          .        
    ## V96          .        
    ## V97          .        
    ## V98          .        
    ## V99          .        
    ## V100         .        
    ## V101         .        
    ## V102         .        
    ## V103         .        
    ## V104         .        
    ## V105         .        
    ## V106         .        
    ## V107         .        
    ## V108         .        
    ## V109         .        
    ## V110         .        
    ## V111         .        
    ## V112         .        
    ## V113         .        
    ## V114         .        
    ## V115         .        
    ## V116         .        
    ## ln           0.5972886

``` r
elastic_net(0.8)
```

    ## Latitude: MSE for alpha 0.8 is 19.25836 
    ## Latitude: lambda.min for alpha 0.8 is 0.09219288 
    ## Latitude: lambda.1se for alpha 0.8 is 0.6504025

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-32-5.png)<!-- -->

    ## Longitude:MSE for alpha 0.8 is 2.502468 
    ## Longitude: lambda.min for alpha 0.8 is 1.388966 
    ## Longitude: lambda.1se for alpha 0.8 is 1.388966

![](BoxCox_Music_Data_files/figure-gfm/unnamed-chunk-32-6.png)<!-- -->

    ## 118 x 1 sparse Matrix of class "dgCMatrix"
    ##                      1
    ## (Intercept) 23.2802646
    ## V1           .        
    ## V2           .        
    ## V3           .        
    ## V4           .        
    ## V5           .        
    ## V6           .        
    ## V7           .        
    ## V8           .        
    ## V9           .        
    ## V10          .        
    ## V11          .        
    ## V12          .        
    ## V13          .        
    ## V14          .        
    ## V15          .        
    ## V16          .        
    ## V17          .        
    ## V18          .        
    ## V19          .        
    ## V20          .        
    ## V21          .        
    ## V22          .        
    ## V23          .        
    ## V24          .        
    ## V25          .        
    ## V26          .        
    ## V27          .        
    ## V28          .        
    ## V29          .        
    ## V30          .        
    ## V31          .        
    ## V32          .        
    ## V33          .        
    ## V34          .        
    ## V35          .        
    ## V36          .        
    ## V37          .        
    ## V38          .        
    ## V39          .        
    ## V40          .        
    ## V41          .        
    ## V42          .        
    ## V43          .        
    ## V44          .        
    ## V45          .        
    ## V46          .        
    ## V47          .        
    ## V48          .        
    ## V49          .        
    ## V50          .        
    ## V51          .        
    ## V52          .        
    ## V53          .        
    ## V54          .        
    ## V55          .        
    ## V56          .        
    ## V57          .        
    ## V58          .        
    ## V59          .        
    ## V60          .        
    ## V61          .        
    ## V62          .        
    ## V63          .        
    ## V64          .        
    ## V65          .        
    ## V66          .        
    ## V67          .        
    ## V68          .        
    ## V69          .        
    ## V70          .        
    ## V71          .        
    ## V72          .        
    ## V73          .        
    ## V74          .        
    ## V75          .        
    ## V76          .        
    ## V77          .        
    ## V78          .        
    ## V79          .        
    ## V80          .        
    ## V81          .        
    ## V82          .        
    ## V83          .        
    ## V84          .        
    ## V85          .        
    ## V86          .        
    ## V87          .        
    ## V88          .        
    ## V89          .        
    ## V90          .        
    ## V91          .        
    ## V92          .        
    ## V93          .        
    ## V94          .        
    ## V95          .        
    ## V96          .        
    ## V97          .        
    ## V98          .        
    ## V99          .        
    ## V100         .        
    ## V101         .        
    ## V102         .        
    ## V103         .        
    ## V104         .        
    ## V105         .        
    ## V106         .        
    ## V107         .        
    ## V108         .        
    ## V109         .        
    ## V110         .        
    ## V111         .        
    ## V112         .        
    ## V113         .        
    ## V114         .        
    ## V115         .        
    ## V116         .        
    ## ln           0.5973392
