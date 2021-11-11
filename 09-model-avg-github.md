Exercise: Model Averaging and GitHub
================
Colin McDaniel
11/04/2021

-   [Import and Scale the Data](#import-and-scale-the-data)
-   [Obtain a Subset of 50 Schools (\~ 2,000
    students)](#obtain-a-subset-of-50-schools--2000-students)
-   [Fit Four Separate Models](#fit-four-separate-models)
-   [Averaging M1, M2, M3, and M4](#averaging-m1-m2-m3-and-m4)
    -   [Test error](#test-error)
-   [Model Averaging of More Models](#model-averaging-of-more-models)

Instructions:

1.  Update the `author` field to your name(s) in line 3.

2.  Run the following code and fill in the blanks. You may need to
    install the `mlmRev` package, but other than that you shouldn’t need
    to change any R code below.

``` r
library(tidyverse)
library(mlmRev)  # if error, install the mlmRev package first:
# install.packages("mlmRev")
library(here)
library(haven)
library(lme4)
library(MuMIn)  # for model averaging
library(modelsummary)
theme_set(theme_bw())  # Theme; just my personal preference
```

## Import and Scale the Data

``` r
# Import HSB data from the mlmRev package
data(Hsb82, package = "mlmRev")
# Rename to hsball
hsball <- Hsb82
```

## Obtain a Subset of 50 Schools (\~ 2,000 students)

``` r
# Create a subset of 50 schools
set.seed(2)  # make the results reproducible
random_schools <- sample(unique(hsball$school), size = 50)
hsbsub <- hsball %>% 
  filter(school %in% random_schools) %>% 
  # cluster-mean centeringn
  group_by(school) %>% 
  mutate(ses_cm = mean(ses),  # mean SES
         sx_cm = mean(sx == "Female"),  # proportion female
         minrty_cm = mean(minrty == "Yes"),  # proportion minority
         ses_cmc = ses - ses_cm) %>% 
  ungroup()
```

## Fit Four Separate Models

``` r
m1 <- lmer(mAch ~ sector * ses_cmc +
             (1 + ses_cmc | school), data = hsbsub, 
           na.action = "na.fail",  # needed for the `MuMIn` package
           REML = FALSE)  # use ML to get AIC/BIC
m2 <- lmer(mAch ~ ses_cmc + ses_cm + (1 + ses_cmc | school), data = hsbsub,
           na.action = "na.fail",  # needed for the `MuMIn` package
           REML = FALSE)  # use ML to get AIC/BIC
m3 <- lmer(mAch ~ minrty_cm + minrty + ses_cm + ses_cmc +
             (1 + minrty + ses_cmc | school), data = hsbsub, 
           na.action = "na.fail",  # needed for the `MuMIn` package
           REML = FALSE)  # use ML to get AIC/BIC
m4 <- lmer(mAch ~ (minrty_cm + minrty) * (ses_cm + ses_cmc) +
             (1 + minrty + ses_cmc | school), data = hsbsub, 
           na.action = "na.fail",  # needed for the `MuMIn` package
           REML = FALSE)  # use ML to get AIC/BIC
AIC(m1, m2, m3, m4)  # marginal AIC
```

    ##    df      AIC
    ## m1  8 14056.97
    ## m2  7 14030.77
    ## m3 12 13980.02
    ## m4 16 13977.87

Fill in the blank: Model 4 appears to have the best out-of-sample
prediction accuracy.

## Averaging M1, M2, M3, and M4

In model averaging, the Akaike weights are usually used, defined as
$$\\frac{\\exp(-0.5 \\times \\Delta\_m)}{\\sum\_i \\exp(-0.5 \\times \\Delta\_i)}, $$
where *Δ*<sub>*m*</sub> = AIC<sub>*m*</sub> − AIC<sub>min</sub> is the
difference between the AIC of model *m* from the minimum AIC among all
the candidate models. You can get the model weights with

``` r
model.sel(m1, m2, m3, m4, rank = "AIC")  # see the last column
```

    ## Model selection table 
    ##    (Int) sct ses_cmc sct:ses_cmc ses_cm mnr  mnr_cm mnr_cm:ses_cm
    ## m4 13.78       2.263              4.990   + -0.9401        0.6053
    ## m3 14.00       1.685              5.254   + -1.9810              
    ## m2 12.79       1.884              6.392                          
    ## m1 11.26   +   2.375           +                                 
    ##    mnr_cm:ses_cmc mnr:ses_cm mnr:ses_cmc             family      random df
    ## m4         -1.329          +           + gaussian(identity) 1+m+ss_c|sc 16
    ## m3                                       gaussian(identity) 1+m+ss_c|sc 12
    ## m2                                       gaussian(identity)   1+ss_c|sc  7
    ## m1                                       gaussian(identity)   1+ss_c|sc  8
    ##       logLik     AIC delta weight
    ## m4 -6972.936 13977.9  0.00  0.745
    ## m3 -6978.009 13980.0  2.14  0.255
    ## m2 -7008.385 14030.8 52.90  0.000
    ## m1 -7020.483 14057.0 79.09  0.000
    ## Models ranked by AIC(x) 
    ## Random terms: 
    ## 1+m+ss_c|sc = '1 + minrty + ses_cmc | school'
    ## 1+ss_c|sc = '1 + ses_cmc | school'

Fill in the blank: Model 4 has the highest weight

To average the models, we use

``` r
# Averaging
m_avg1 <- model.avg(m1, m2, m3, m4, rank = "AIC")
summary(m_avg1)
```

    ## 
    ## Call:
    ## model.avg(object = m1, m2, m3, m4, rank = "AIC")
    ## 
    ## Component model call: 
    ## lmer(formula = mAch ~ <4 unique rhs>, data = hsbsub, REML = FALSE, 
    ##      na.action = na.fail)
    ## 
    ## Component models: 
    ##                 df   logLik      AIC delta weight
    ## 1/2/4/5/6/7/8/9 16 -6972.94 13977.87  0.00   0.75
    ## 1/2/4/5         12 -6978.01 13980.02  2.14   0.25
    ## 4/5              7 -7008.38 14030.77 52.90   0.00
    ## 3/5/10           8 -7020.48 14056.97 79.09   0.00
    ## 
    ## Term codes: 
    ##            minrty         minrty_cm            sector            ses_cm 
    ##                 1                 2                 3                 4 
    ##           ses_cmc  minrty_cm:ses_cm minrty_cm:ses_cmc     minrty:ses_cm 
    ##                 5                 6                 7                 8 
    ##    minrty:ses_cmc    sector:ses_cmc 
    ##                 9                10 
    ## 
    ## Model-averaged coefficients:  
    ## (full average) 
    ##                          Estimate Std. Error Adjusted SE z value Pr(>|z|)    
    ## (Intercept)             1.383e+01  3.924e-01   3.926e-01  35.241   <2e-16 ***
    ## minrty_cm              -1.205e+00  1.239e+00   1.240e+00   0.972    0.331    
    ## minrtyYes              -2.706e+00  4.786e-01   4.789e-01   5.651   <2e-16 ***
    ## ses_cm                  5.057e+00  9.693e-01   9.699e-01   5.214    2e-07 ***
    ## ses_cmc                 2.116e+00  3.562e-01   3.563e-01   5.938   <2e-16 ***
    ## minrty_cm:ses_cm        4.510e-01  1.885e+00   1.886e+00   0.239    0.811    
    ## minrty_cm:ses_cmc      -9.904e-01  9.062e-01   9.065e-01   1.093    0.275    
    ## minrtyYes:ses_cm        3.690e-02  9.425e-01   9.430e-01   0.039    0.969    
    ## minrtyYes:ses_cmc      -3.863e-01  5.286e-01   5.289e-01   0.730    0.465    
    ## sectorCatholic          1.544e-17  7.194e-09   7.195e-09   0.000    1.000    
    ## sectorCatholic:ses_cmc -4.623e-18  2.258e-09   2.258e-09   0.000    1.000    
    ##  
    ## (conditional average) 
    ##                        Estimate Std. Error Adjusted SE z value Pr(>|z|)    
    ## (Intercept)            13.83410    0.39235     0.39256  35.241  < 2e-16 ***
    ## minrty_cm              -1.20535    1.23919     1.23979   0.972 0.330937    
    ## minrtyYes              -2.70584    0.47859     0.47886   5.651  < 2e-16 ***
    ## ses_cm                  5.05732    0.96933     0.96986   5.214    2e-07 ***
    ## ses_cmc                 2.11570    0.35620     0.35630   5.938  < 2e-16 ***
    ## minrty_cm:ses_cm        0.60533    2.16200     2.16321   0.280 0.779610    
    ## minrty_cm:ses_cmc      -1.32930    0.80728     0.80774   1.646 0.099822 .  
    ## minrtyYes:ses_cm        0.04952    1.09162     1.09223   0.045 0.963835    
    ## minrtyYes:ses_cmc      -0.51841    0.55366     0.55397   0.936 0.349372    
    ## sectorCatholic          3.10028    0.88265     0.88314   3.511 0.000447 ***
    ## sectorCatholic:ses_cmc -0.92811    0.40215     0.40238   2.307 0.021078 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Now let’s look at the prediction accuracy on the hold-out sample (i.e.,
the remaining 110 schools).

### Test error

``` r
# Validation sample
hsbtest <- hsball %>% 
  filter(!school %in% random_schools) %>% 
  # cluster-mean centeringn
  group_by(school) %>% 
  mutate(ses_cm = mean(ses),  # mean SES
         sx_cm = mean(sx == "Female"),  # proportion female
         minrty_cm = mean(minrty == "Yes"),  # proportion minority
         ses_cmc = ses - ses_cm) %>% 
  ungroup()
mse <- lapply(list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, `m_avg1` = m_avg1), 
              function(m) {
                mean(
                  (predict(m, newdata = hsbtest, re.form = NA) -
                     hsbtest$mAch)^2
                )
              })
# Mean squared errors
unlist(mse)
```

    ##       m1       m2       m3       m4   m_avg1 
    ## 43.12587 40.29472 40.46373 39.80878 39.93174

Fill in the blank: From the output above, model 1 shows the highest
out-of-sample prediction accuracy.

## Model Averaging of More Models

The following performs model averaging of 23 best possible submodels
that contain `ses_cmc` and `ses_cm` out of the 7 predictors, using
Akaike weights.

``` r
m_full <- lmer(mAch ~ ses_cmc + ses_cm +
                 minrty + minrty_cm + sx + sx_cm +
                 (1 + ses_cmc + minrty + sx | school), data = hsbsub, 
               na.action = "na.fail",
               REML = FALSE)
dd <- dredge(m_full, fixed = ~ ses_cmc + ses_cm, rank = "AIC")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00213797 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00307154 (tol = 0.002, component 1)

``` r
# Average models with 95% of the Akaike weights
dd_models <- get.models(dd, subset = cumsum(weight) <= .95)
m_avg2 <- model.avg(dd_models)
summary(m_avg2)
```

    ## 
    ## Call:
    ## model.avg(object = dd_models)
    ## 
    ## Component model call: 
    ## lmer(formula = mAch ~ <3 unique rhs>, data = hsbsub, REML = FALSE, 
    ##      na.action = na.fail)
    ## 
    ## Component models: 
    ##       df   logLik      AIC delta weight
    ## 1345  16 -6968.02 13968.04  0.00   0.55
    ## 13456 17 -6967.82 13969.65  1.61   0.24
    ## 12345 17 -6967.98 13969.96  1.92   0.21
    ## 
    ## Term codes: 
    ##    minrty minrty_cm    ses_cm   ses_cmc        sx     sx_cm 
    ##         1         2         3         4         5         6 
    ## 
    ## Model-averaged coefficients:  
    ## (full average) 
    ##             Estimate Std. Error Adjusted SE z value Pr(>|z|)    
    ## (Intercept)  14.1807     0.5574      0.5576  25.430  < 2e-16 ***
    ## minrtyYes    -2.9000     0.4643      0.4646   6.242  < 2e-16 ***
    ## sxFemale     -1.2373     0.3509      0.3511   3.524 0.000425 ***
    ## ses_cm        5.0795     0.6792      0.6796   7.474  < 2e-16 ***
    ## ses_cmc       1.6279     0.1985      0.1986   8.198  < 2e-16 ***
    ## sx_cm         0.1868     0.6213      0.6216   0.301 0.763774    
    ## minrty_cm    -0.3570     0.8370      0.8372   0.426 0.669813    
    ##  
    ## (conditional average) 
    ##             Estimate Std. Error Adjusted SE z value Pr(>|z|)    
    ## (Intercept)  14.1807     0.5574      0.5576  25.430  < 2e-16 ***
    ## minrtyYes    -2.9000     0.4643      0.4646   6.242  < 2e-16 ***
    ## sxFemale     -1.2373     0.3509      0.3511   3.524 0.000425 ***
    ## ses_cm        5.0795     0.6792      0.6796   7.474  < 2e-16 ***
    ## ses_cmc       1.6279     0.1985      0.1986   8.198  < 2e-16 ***
    ## sx_cm         0.7649     1.0670      1.0676   0.716 0.473723    
    ## minrty_cm    -1.7065     1.0228      1.0234   1.668 0.095403 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Mean squared error 
mean(
  (predict(m_avg2, newdata = hsbtest, re.form = NA) - hsbtest$mAch)^2
)
```

    ## [1] 39.1885

Note the above performs better than previous models.

The following shows the variance importance of the predictors. It adds
up the weights of the models that contain a given predictor, for each
predictor.

``` r
importance(m_avg2)
```

    ##                      minrty ses_cm ses_cmc sx   sx_cm minrty_cm
    ## Sum of weights:      1.00   1.00   1.00    1.00 0.24  0.21     
    ## N containing models:    3      3      3       3    1     1

Fill in the blanks: Aside from `ses_cm` and `ses_cmc`, `minrty` and `sx`
are the most important for predicting `mAch`, while `minrty_cm` is the
least important?

To learn more about averaging, check out this paper:
<https://www.sciencedirect.com/science/article/pii/S0022249699912786>
