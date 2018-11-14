drak\_tc
================
Tatini Mal-Sarkar
11/14/2018

TC Newman Model 3
=================

Model 3, applied to TC Newman

``` r
drak_tc = read_csv("./data/KerryDataRequestV2_visit1_c.csv") %>% 
  select(-ASSIST_Tobacco_Score:-opioids_preg_any,
         -AQ1a:-AQ14b,
         -pdiq1_afr:-pdiq13_xho,
         -leq1_1_afr:-leq51_2_xho,
         -ipvq1_afr:-ipvq17_xho,
         -epdsq1_afr:-epdsq10_xho,
         -bdiq1_afr:-bdiq25_2_xho,
         -srqq1_afr:-srqq20_xho) %>% 
  filter(Clinic == 2)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   shortpid = col_integer(),
    ##   mother_age_at_enrolment = col_double(),
    ##   SESaQ1 = col_integer(),
    ##   SESaQ2 = col_integer(),
    ##   SESaQ3_1 = col_integer(),
    ##   SESaQ3_2 = col_integer(),
    ##   SESaQ4_1 = col_integer(),
    ##   SESaQ4_2 = col_integer(),
    ##   SESaQ5 = col_integer(),
    ##   SESaQ6_1 = col_integer(),
    ##   SESaQ6_2 = col_integer(),
    ##   SESaQ7 = col_integer(),
    ##   SESaQ8_1 = col_integer(),
    ##   SESaQ8_2 = col_integer(),
    ##   SESaQ9_1 = col_integer(),
    ##   SESaQ9_2 = col_integer(),
    ##   SESaQ9_3 = col_integer(),
    ##   SESaQ9_4 = col_integer(),
    ##   SESaQ9_5 = col_integer(),
    ##   SESaQ9_6 = col_integer()
    ##   # ... with 421 more columns
    ## )

    ## See spec(...) for full column specifications.

No mediator
-----------

``` r
nomed_mod <- 'pov =~ sumses9 + ethnicity + SESaQ31_1 + sumses33 + SESaQ34_1
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21
depf2 =~ bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1

depf1 ~ pov
depf2 ~ pov '

nomed_mod_fit_3 <- sem(nomed_mod, data = drak_tc, missing = 'fiml.x')
```

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
    ## variances are negative

``` r
summary(nomed_mod_fit_3)
```

    ## lavaan 0.6-3 ended normally after 156 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         75
    ## 
    ##   Number of observations                           470
    ##   Number of missing patterns                         7
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                     557.029
    ##   Degrees of freedom                               249
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Observed
    ##   Observed information based on                Hessian
    ##   Standard Errors                             Standard
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   pov =~                                              
    ##     sumses9           1.000                           
    ##     ethnicity         0.109    0.103    1.061    0.289
    ##     SESaQ31_1        15.659   16.247    0.964    0.335
    ##     sumses33          6.468    2.845    2.274    0.023
    ##     SESaQ34_1         1.218    0.554    2.199    0.028
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.071    0.100   10.665    0.000
    ##     bdiq4             1.086    0.101   10.761    0.000
    ##     bdiq5             0.888    0.085   10.472    0.000
    ##     bdiq6             1.306    0.129   10.100    0.000
    ##     bdiq7             0.983    0.093   10.550    0.000
    ##     bdiq8             1.383    0.126   10.937    0.000
    ##     bdiq10            1.297    0.148    8.774    0.000
    ##     bdiq12            1.067    0.106   10.063    0.000
    ##     bdiq13            1.171    0.116   10.128    0.000
    ##     bdiq14            1.033    0.097   10.696    0.000
    ##     bdiq15            0.779    0.096    8.146    0.000
    ##     bdiq17            0.791    0.098    8.044    0.000
    ##     bdiq20            0.623    0.096    6.508    0.000
    ##     bdiq21            0.672    0.116    5.781    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.701    0.078    9.044    0.000
    ##     bdiq24_1          0.810    0.075   10.858    0.000
    ##     bdiq25_1          0.481    0.067    7.175    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     pov               0.068    0.151    0.452    0.651
    ##   depf2 ~                                             
    ##     pov              -0.075    0.101   -0.740    0.459
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.047    0.008    6.143    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           2.484    0.043   58.200    0.000
    ##    .ethnicity         2.970    0.011  265.806    0.000
    ##    .SESaQ31_1         3.029    0.044   68.807    0.000
    ##    .sumses33          5.331    0.071   74.664    0.000
    ##    .SESaQ34_1         1.573    0.027   57.410    0.000
    ##    .bdiq2             0.351    0.032   10.902    0.000
    ##    .bdiq3             0.381    0.034   11.083    0.000
    ##    .bdiq4             0.562    0.034   16.509    0.000
    ##    .bdiq5             0.434    0.028   15.314    0.000
    ##    .bdiq6             0.521    0.044   11.908    0.000
    ##    .bdiq7             0.328    0.031   10.441    0.000
    ##    .bdiq8             0.470    0.042   11.291    0.000
    ##    .bdiq10            0.697    0.051   13.681    0.000
    ##    .bdiq12            0.523    0.035   14.882    0.000
    ##    .bdiq13            0.572    0.039   14.696    0.000
    ##    .bdiq14            0.332    0.032   10.281    0.000
    ##    .bdiq15            0.728    0.034   21.495    0.000
    ##    .bdiq17            0.830    0.035   23.895    0.000
    ##    .bdiq20            0.881    0.035   25.227    0.000
    ##    .bdiq21            0.932    0.043   21.479    0.000
    ##    .bdiq22_1          0.149    0.016    9.076    0.000
    ##    .bdiq23_1          0.107    0.014    7.485    0.000
    ##    .bdiq24_1          0.094    0.013    6.973    0.000
    ##    .bdiq25_1          0.100    0.014    7.230    0.000
    ##     pov               0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           0.845    0.056   15.122    0.000
    ##    .ethnicity         0.059    0.004   15.344    0.000
    ##    .SESaQ31_1        -1.048    1.342   -0.781    0.435
    ##    .sumses33          2.048    0.267    7.677    0.000
    ##    .SESaQ34_1         0.340    0.023   14.536    0.000
    ##    .bdiq2             0.315    0.022   14.023    0.000
    ##    .bdiq3             0.357    0.026   14.000    0.000
    ##    .bdiq4             0.341    0.024   13.930    0.000
    ##    .bdiq5             0.242    0.017   14.028    0.000
    ##    .bdiq6             0.607    0.043   14.203    0.000
    ##    .bdiq7             0.296    0.021   14.022    0.000
    ##    .bdiq8             0.485    0.035   13.754    0.000
    ##    .bdiq10            0.927    0.064   14.585    0.000
    ##    .bdiq12            0.385    0.027   14.128    0.000
    ##    .bdiq13            0.477    0.034   14.162    0.000
    ##    .bdiq14            0.306    0.022   13.907    0.000
    ##    .bdiq15            0.434    0.029   14.740    0.000
    ##    .bdiq17            0.459    0.031   14.758    0.000
    ##    .bdiq20            0.506    0.034   15.008    0.000
    ##    .bdiq21            0.807    0.053   15.106    0.000
    ##    .bdiq22_1          0.063    0.007    9.530    0.000
    ##    .bdiq23_1          0.064    0.005   12.376    0.000
    ##    .bdiq24_1          0.043    0.004    9.803    0.000
    ##    .bdiq25_1          0.075    0.005   14.189    0.000
    ##     pov               0.008    0.011    0.696    0.486
    ##    .depf1             0.172    0.026    6.654    0.000
    ##    .depf2             0.064    0.009    7.201    0.000

With mediator
-------------

``` r
med_mod <- 'pov =~ sumses9 + ethnicity + SESaQ31_1 + sumses33 + SESaQ34_1
trauf2 =~ ipvq1 + ipvq2 + ipvq3 + ipvq4 + ipvq5 + ipvq6 + ipvq7 + ipvq8 + ipvq9 + ipvq10 + ipvq11 + ipvq12 + ipvq13 + ipvq14 + ipvq15
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21
depf2 =~ bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1

depf1 ~ trauf2 + pov
depf2 ~ trauf2 + pov
trauf2 ~ pov '

med_mod_fit_3 <- sem(med_mod, data = drak_tc, missing = 'fiml.x')
```

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
    ## variances are negative

``` r
summary(med_mod_fit_3)
```

    ## lavaan 0.6-3 ended normally after 191 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        123
    ## 
    ##   Number of observations                           470
    ##   Number of missing patterns                        16
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    2251.030
    ##   Degrees of freedom                               696
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Observed
    ##   Observed information based on                Hessian
    ##   Standard Errors                             Standard
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   pov =~                                              
    ##     sumses9           1.000                           
    ##     ethnicity         0.105    0.102    1.031    0.302
    ##     SESaQ31_1        14.510   14.785    0.981    0.326
    ##     sumses33          6.318    2.763    2.287    0.022
    ##     SESaQ34_1         1.198    0.545    2.197    0.028
    ##   trauf2 =~                                           
    ##     ipvq1             1.000                           
    ##     ipvq2             0.882    0.056   15.741    0.000
    ##     ipvq3             0.932    0.056   16.741    0.000
    ##     ipvq4             0.918    0.055   16.702    0.000
    ##     ipvq5            -0.384    0.030  -12.775    0.000
    ##     ipvq6             0.932    0.056   16.780    0.000
    ##     ipvq7             0.942    0.057   16.534    0.000
    ##     ipvq8             0.743    0.047   15.884    0.000
    ##     ipvq9             0.774    0.048   16.265    0.000
    ##     ipvq10            0.350    0.029   11.982    0.000
    ##     ipvq11           -0.340    0.028  -12.081    0.000
    ##     ipvq12            0.426    0.035   12.119    0.000
    ##     ipvq13            0.570    0.043   13.377    0.000
    ##     ipvq14            0.249    0.024   10.230    0.000
    ##     ipvq15           -0.259    0.024  -10.842    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.070    0.099   10.855    0.000
    ##     bdiq4             1.073    0.099   10.867    0.000
    ##     bdiq5             0.878    0.083   10.579    0.000
    ##     bdiq6             1.305    0.127   10.278    0.000
    ##     bdiq7             0.967    0.091   10.621    0.000
    ##     bdiq8             1.364    0.124   11.038    0.000
    ##     bdiq10            1.272    0.145    8.789    0.000
    ##     bdiq12            1.053    0.104   10.153    0.000
    ##     bdiq13            1.157    0.113   10.224    0.000
    ##     bdiq14            1.014    0.094   10.751    0.000
    ##     bdiq15            0.765    0.094    8.157    0.000
    ##     bdiq17            0.778    0.096    8.076    0.000
    ##     bdiq20            0.604    0.094    6.436    0.000
    ##     bdiq21            0.669    0.115    5.838    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.724    0.079    9.194    0.000
    ##     bdiq24_1          0.818    0.074   11.085    0.000
    ##     bdiq25_1          0.493    0.068    7.237    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf2            0.244    0.032    7.664    0.000
    ##     pov               0.088    0.137    0.642    0.521
    ##   depf2 ~                                             
    ##     trauf2            0.123    0.019    6.499    0.000
    ##     pov              -0.064    0.092   -0.691    0.490
    ##   trauf2 ~                                            
    ##     pov              -0.086    0.271   -0.317    0.751
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.029    0.006    4.819    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           2.484    0.043   58.199    0.000
    ##    .ethnicity         2.970    0.011  265.806    0.000
    ##    .SESaQ31_1         3.029    0.044   68.806    0.000
    ##    .sumses33          5.331    0.071   74.669    0.000
    ##    .SESaQ34_1         1.573    0.027   57.410    0.000
    ##    .ipvq1             1.891    0.048   39.542    0.000
    ##    .ipvq2             1.640    0.045   36.746    0.000
    ##    .ipvq3             1.613    0.044   36.629    0.000
    ##    .ipvq4             1.532    0.043   35.258    0.000
    ##    .ipvq5             1.587    0.023   67.831    0.000
    ##    .ipvq6             1.751    0.043   40.357    0.000
    ##    .ipvq7             1.664    0.044   37.413    0.000
    ##    .ipvq8             1.345    0.036   37.102    0.000
    ##    .ipvq9             1.370    0.037   37.207    0.000
    ##    .ipvq10            1.134    0.022   51.157    0.000
    ##    .ipvq11            1.699    0.022   77.193    0.000
    ##    .ipvq12            1.177    0.027   43.947    0.000
    ##    .ipvq13            1.283    0.033   39.305    0.000
    ##    .ipvq14            1.081    0.018   58.687    0.000
    ##    .ipvq15            1.829    0.018   99.838    0.000
    ##    .bdiq2             0.351    0.032   10.902    0.000
    ##    .bdiq3             0.381    0.034   11.083    0.000
    ##    .bdiq4             0.562    0.034   16.509    0.000
    ##    .bdiq5             0.434    0.028   15.314    0.000
    ##    .bdiq6             0.521    0.044   11.908    0.000
    ##    .bdiq7             0.328    0.031   10.441    0.000
    ##    .bdiq8             0.470    0.042   11.291    0.000
    ##    .bdiq10            0.697    0.051   13.680    0.000
    ##    .bdiq12            0.523    0.035   14.882    0.000
    ##    .bdiq13            0.572    0.039   14.696    0.000
    ##    .bdiq14            0.332    0.032   10.281    0.000
    ##    .bdiq15            0.728    0.034   21.495    0.000
    ##    .bdiq17            0.830    0.035   23.895    0.000
    ##    .bdiq20            0.881    0.035   25.227    0.000
    ##    .bdiq21            0.932    0.043   21.479    0.000
    ##    .bdiq22_1          0.149    0.016    9.077    0.000
    ##    .bdiq23_1          0.107    0.014    7.485    0.000
    ##    .bdiq24_1          0.094    0.013    6.974    0.000
    ##    .bdiq25_1          0.100    0.014    7.230    0.000
    ##     pov               0.000                           
    ##    .trauf2            0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           0.844    0.056   15.081    0.000
    ##    .ethnicity         0.059    0.004   15.344    0.000
    ##    .SESaQ31_1        -0.945    1.237   -0.764    0.445
    ##    .sumses33          2.030    0.271    7.493    0.000
    ##    .SESaQ34_1         0.339    0.023   14.486    0.000
    ##    .ipvq1             0.476    0.035   13.742    0.000
    ##    .ipvq2             0.471    0.033   14.090    0.000
    ##    .ipvq3             0.390    0.029   13.629    0.000
    ##    .ipvq4             0.382    0.028   13.733    0.000
    ##    .ipvq5             0.157    0.011   14.103    0.000
    ##    .ipvq6             0.365    0.027   13.440    0.000
    ##    .ipvq7             0.398    0.029   13.617    0.000
    ##    .ipvq8             0.286    0.021   13.857    0.000
    ##    .ipvq9             0.278    0.020   13.680    0.000
    ##    .ipvq10            0.158    0.011   14.763    0.000
    ##    .ipvq11            0.144    0.010   14.111    0.000
    ##    .ipvq12            0.228    0.016   14.715    0.000
    ##    .ipvq13            0.306    0.021   14.510    0.000
    ##    .ipvq14            0.122    0.008   14.961    0.000
    ##    .ipvq15            0.101    0.007   13.798    0.000
    ##    .bdiq2             0.311    0.022   14.008    0.000
    ##    .bdiq3             0.353    0.025   13.989    0.000
    ##    .bdiq4             0.341    0.024   13.971    0.000
    ##    .bdiq5             0.242    0.017   14.064    0.000
    ##    .bdiq6             0.600    0.042   14.193    0.000
    ##    .bdiq7             0.298    0.021   14.075    0.000
    ##    .bdiq8             0.487    0.035   13.809    0.000
    ##    .bdiq10            0.932    0.064   14.618    0.000
    ##    .bdiq12            0.386    0.027   14.164    0.000
    ##    .bdiq13            0.477    0.034   14.193    0.000
    ##    .bdiq14            0.308    0.022   13.977    0.000
    ##    .bdiq15            0.436    0.029   14.765    0.000
    ##    .bdiq17            0.460    0.031   14.779    0.000
    ##    .bdiq20            0.509    0.034   15.029    0.000
    ##    .bdiq21            0.806    0.053   15.109    0.000
    ##    .bdiq22_1          0.065    0.007    9.942    0.000
    ##    .bdiq23_1          0.063    0.005   12.256    0.000
    ##    .bdiq24_1          0.043    0.004    9.994    0.000
    ##    .bdiq25_1          0.075    0.005   14.172    0.000
    ##     pov               0.009    0.012    0.706    0.480
    ##    .trauf2            0.599    0.065    9.254    0.000
    ##    .depf1             0.141    0.021    6.716    0.000
    ##    .depf2             0.053    0.008    6.859    0.000

TC Newman Model 1
=================

Model 1, applied to TC Newman

No mediator
-----------

``` r
nomed_mod <- 'pov =~ SESaQ13_1 + SESaQ14 + SESaQ15_1 + SESaQ20_1 + SESaQ21 + SESaQ31_1 + SESaQ32 + SESaQ34_1 + sumses9 + sumses33
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21
depf2 =~ bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1

depf1 ~ pov
depf2 ~ pov '

nomed_mod_fit_1 <- sem(nomed_mod, data = drak_tc, missing = 'fiml.x')
```

    ## Warning in lavaan::lavaan(model = nomed_mod, data = drak_tc, missing =
    ## "fiml.x", : lavaan WARNING: the optimizer warns that a solution has NOT
    ## been found!

``` r
summary(nomed_mod_fit_1)
```

    ## lavaan 0.6-3 did NOT end normally after 650 iterations
    ## ** WARNING ** Estimates below are most likely unreliable
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         90
    ## 
    ##   Number of observations                           470
    ##   Number of missing patterns                        13
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                          NA
    ##   Degrees of freedom                                NA
    ##   P-value                                           NA
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Observed
    ##   Observed information based on                Hessian
    ##   Standard Errors                             Standard
    ## 
    ## Latent Variables:
    ##                    Estimate   Std.Err  z-value  P(>|z|)
    ##   pov =~                                               
    ##     SESaQ13_1          1.000                           
    ##     SESaQ14           17.276       NA                  
    ##     SESaQ15_1         -7.916       NA                  
    ##     SESaQ20_1          5.520       NA                  
    ##     SESaQ21           -4.278       NA                  
    ##     SESaQ31_1         39.749       NA                  
    ##     SESaQ32           -9.443       NA                  
    ##     SESaQ34_1         -6.125       NA                  
    ##     sumses9            8.789       NA                  
    ##     sumses33       -2807.197       NA                  
    ##   depf1 =~                                             
    ##     bdiq2              1.000                           
    ##     bdiq3              1.071       NA                  
    ##     bdiq4              1.087       NA                  
    ##     bdiq5              0.888       NA                  
    ##     bdiq6              1.304       NA                  
    ##     bdiq7              0.982       NA                  
    ##     bdiq8              1.383       NA                  
    ##     bdiq10             1.297       NA                  
    ##     bdiq12             1.066       NA                  
    ##     bdiq13             1.170       NA                  
    ##     bdiq14             1.033       NA                  
    ##     bdiq15             0.779       NA                  
    ##     bdiq17             0.791       NA                  
    ##     bdiq20             0.623       NA                  
    ##     bdiq21             0.674       NA                  
    ##   depf2 =~                                             
    ##     bdiq22_1           1.000                           
    ##     bdiq23_1           0.704       NA                  
    ##     bdiq24_1           0.811       NA                  
    ##     bdiq25_1           0.481       NA                  
    ## 
    ## Regressions:
    ##                    Estimate   Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                              
    ##     pov               -0.907       NA                  
    ##   depf2 ~                                              
    ##     pov               -0.256       NA                  
    ## 
    ## Covariances:
    ##                    Estimate   Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                             
    ##    .depf2              0.047       NA                  
    ## 
    ## Intercepts:
    ##                    Estimate   Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1          1.117       NA                  
    ##    .SESaQ14            4.376       NA                  
    ##    .SESaQ15_1          2.689       NA                  
    ##    .SESaQ20_1          1.649       NA                  
    ##    .SESaQ21            0.481       NA                  
    ##    .SESaQ31_1          3.030       NA                  
    ##    .SESaQ32            2.879       NA                  
    ##    .SESaQ34_1          1.573       NA                  
    ##    .sumses9            2.484       NA                  
    ##    .sumses33           5.326       NA                  
    ##    .bdiq2              0.351       NA                  
    ##    .bdiq3              0.381       NA                  
    ##    .bdiq4              0.562       NA                  
    ##    .bdiq5              0.434       NA                  
    ##    .bdiq6              0.521       NA                  
    ##    .bdiq7              0.328       NA                  
    ##    .bdiq8              0.470       NA                  
    ##    .bdiq10             0.697       NA                  
    ##    .bdiq12             0.523       NA                  
    ##    .bdiq13             0.572       NA                  
    ##    .bdiq14             0.332       NA                  
    ##    .bdiq15             0.728       NA                  
    ##    .bdiq17             0.830       NA                  
    ##    .bdiq20             0.881       NA                  
    ##    .bdiq21             0.932       NA                  
    ##    .bdiq22_1           0.149       NA                  
    ##    .bdiq23_1           0.107       NA                  
    ##    .bdiq24_1           0.094       NA                  
    ##    .bdiq25_1           0.100       NA                  
    ##     pov                0.000                           
    ##    .depf1              0.000                           
    ##    .depf2              0.000                           
    ## 
    ## Variances:
    ##                    Estimate   Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1          0.159       NA                  
    ##    .SESaQ14            0.591       NA                  
    ##    .SESaQ15_1          1.979       NA                  
    ##    .SESaQ20_1          0.632       NA                  
    ##    .SESaQ21            0.476       NA                  
    ##    .SESaQ31_1          0.920       NA                  
    ##    .SESaQ32            2.313       NA                  
    ##    .SESaQ34_1          0.352       NA                  
    ##    .sumses9            0.853       NA                  
    ##    .sumses33          57.966       NA                  
    ##    .bdiq2              0.315       NA                  
    ##    .bdiq3              0.357       NA                  
    ##    .bdiq4              0.340       NA                  
    ##    .bdiq5              0.242       NA                  
    ##    .bdiq6              0.607       NA                  
    ##    .bdiq7              0.296       NA                  
    ##    .bdiq8              0.485       NA                  
    ##    .bdiq10             0.927       NA                  
    ##    .bdiq12             0.386       NA                  
    ##    .bdiq13             0.477       NA                  
    ##    .bdiq14             0.306       NA                  
    ##    .bdiq15             0.434       NA                  
    ##    .bdiq17             0.459       NA                  
    ##    .bdiq20             0.506       NA                  
    ##    .bdiq21             0.806       NA                  
    ##    .bdiq22_1           0.063       NA                  
    ##    .bdiq23_1           0.064       NA                  
    ##    .bdiq24_1           0.043       NA                  
    ##    .bdiq25_1           0.075       NA                  
    ##     pov               -0.000       NA                  
    ##    .depf1              0.172       NA                  
    ##    .depf2              0.064       NA

``` r
med_mod <- 'pov =~ SESaQ13_1 + SESaQ14 + SESaQ15_1 + SESaQ20_1 + SESaQ21 + SESaQ31_1 + SESaQ32 + SESaQ34_1 + sumses9 + sumses33
trauf1 =~ MPSSQ2 + MPSSQ3 + MPSSQ4 + MPSSQ5 + MPSSQ6 + MPSSQ7 + MPSSQ8 + MPSSQ9 + MPSSQ10 + MPSSQ11 + MPSSQ12 + MPSSQ13 + MPSSQ14 + MPSSQ15 + MPSSQ16 + MPSSQ17 + MPSSQ18
trauf2 =~ ipvq1 + ipvq2 + ipvq3 + ipvq4 + ipvq5 + ipvq6 + ipvq7 + ipvq8 + ipvq9 + ipvq10 + ipvq11 + ipvq12 + ipvq13 + ipvq14 + ipvq15
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21
depf2 =~ bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1

depf1 ~ trauf1 + trauf2 + pov
depf2 ~ trauf1 + trauf2 + pov
trauf1 ~ pov
trauf2 ~ pov '

med_mod_fit_1 <- sem(med_mod, data = drak_tc, missing = 'fiml.x')
```

    ## Warning in lavaan::lavaan(model = med_mod, data = drak_tc, missing =
    ## "fiml.x", : lavaan WARNING: the optimizer warns that a solution has NOT
    ## been found!

``` r
summary(med_mod_fit_1)
```

    ## lavaan 0.6-3 did NOT end normally after 772 iterations
    ## ** WARNING ** Estimates below are most likely unreliable
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        192
    ## 
    ##   Number of observations                           470
    ##   Number of missing patterns                        30
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                          NA
    ##   Degrees of freedom                                NA
    ##   P-value                                           NA
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Observed
    ##   Observed information based on                Hessian
    ##   Standard Errors                             Standard
    ## 
    ## Latent Variables:
    ##                    Estimate   Std.Err  z-value  P(>|z|)
    ##   pov =~                                               
    ##     SESaQ13_1          1.000                           
    ##     SESaQ14           13.876       NA                  
    ##     SESaQ15_1         -6.157       NA                  
    ##     SESaQ20_1          4.495       NA                  
    ##     SESaQ21           -3.400       NA                  
    ##     SESaQ31_1         32.146       NA                  
    ##     SESaQ32           -7.733       NA                  
    ##     SESaQ34_1         -4.771       NA                  
    ##     sumses9            7.330       NA                  
    ##     sumses33       -3092.977       NA                  
    ##   trauf1 =~                                            
    ##     MPSSQ2             1.000                           
    ##     MPSSQ3             1.205       NA                  
    ##     MPSSQ4             1.331       NA                  
    ##     MPSSQ5             1.447       NA                  
    ##     MPSSQ6             1.511       NA                  
    ##     MPSSQ7             0.838       NA                  
    ##     MPSSQ8             1.406       NA                  
    ##     MPSSQ9             1.565       NA                  
    ##     MPSSQ10            1.357       NA                  
    ##     MPSSQ11            1.453       NA                  
    ##     MPSSQ12            1.465       NA                  
    ##     MPSSQ13            1.596       NA                  
    ##     MPSSQ14            1.488       NA                  
    ##     MPSSQ15            1.387       NA                  
    ##     MPSSQ16            1.446       NA                  
    ##     MPSSQ17            1.321       NA                  
    ##     MPSSQ18            1.283       NA                  
    ##   trauf2 =~                                            
    ##     ipvq1              1.000                           
    ##     ipvq2              0.882       NA                  
    ##     ipvq3              0.932       NA                  
    ##     ipvq4              0.918       NA                  
    ##     ipvq5             -0.384       NA                  
    ##     ipvq6              0.934       NA                  
    ##     ipvq7              0.944       NA                  
    ##     ipvq8              0.745       NA                  
    ##     ipvq9              0.775       NA                  
    ##     ipvq10             0.350       NA                  
    ##     ipvq11            -0.340       NA                  
    ##     ipvq12             0.425       NA                  
    ##     ipvq13             0.569       NA                  
    ##     ipvq14             0.249       NA                  
    ##     ipvq15            -0.259       NA                  
    ##   depf1 =~                                             
    ##     bdiq2              1.000                           
    ##     bdiq3              1.067       NA                  
    ##     bdiq4              1.064       NA                  
    ##     bdiq5              0.875       NA                  
    ##     bdiq6              1.304       NA                  
    ##     bdiq7              0.964       NA                  
    ##     bdiq8              1.357       NA                  
    ##     bdiq10             1.265       NA                  
    ##     bdiq12             1.041       NA                  
    ##     bdiq13             1.146       NA                  
    ##     bdiq14             1.006       NA                  
    ##     bdiq15             0.762       NA                  
    ##     bdiq17             0.776       NA                  
    ##     bdiq20             0.606       NA                  
    ##     bdiq21             0.670       NA                  
    ##   depf2 =~                                             
    ##     bdiq22_1           1.000                           
    ##     bdiq23_1           0.728       NA                  
    ##     bdiq24_1           0.822       NA                  
    ##     bdiq25_1           0.495       NA                  
    ## 
    ## Regressions:
    ##                    Estimate   Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                              
    ##     trauf1             0.304       NA                  
    ##     trauf2             0.209       NA                  
    ##     pov                0.340       NA                  
    ##   depf2 ~                                              
    ##     trauf1             0.073       NA                  
    ##     trauf2             0.114       NA                  
    ##     pov                0.187       NA                  
    ##   trauf1 ~                                             
    ##     pov               -2.345       NA                  
    ##   trauf2 ~                                             
    ##     pov               -1.431       NA                  
    ## 
    ## Covariances:
    ##                    Estimate   Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                             
    ##    .depf2              0.024       NA                  
    ## 
    ## Intercepts:
    ##                    Estimate   Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1          1.117       NA                  
    ##    .SESaQ14            4.376       NA                  
    ##    .SESaQ15_1          2.689       NA                  
    ##    .SESaQ20_1          1.649       NA                  
    ##    .SESaQ21            0.481       NA                  
    ##    .SESaQ31_1          3.030       NA                  
    ##    .SESaQ32            2.879       NA                  
    ##    .SESaQ34_1          1.573       NA                  
    ##    .sumses9            2.484       NA                  
    ##    .sumses33           5.325       NA                  
    ##    .MPSSQ2             0.667       NA                  
    ##    .MPSSQ3             0.870       NA                  
    ##    .MPSSQ4             1.315       NA                  
    ##    .MPSSQ5             1.194       NA                  
    ##    .MPSSQ6             1.055       NA                  
    ##    .MPSSQ7             0.477       NA                  
    ##    .MPSSQ8             0.729       NA                  
    ##    .MPSSQ9             0.860       NA                  
    ##    .MPSSQ10            0.793       NA                  
    ##    .MPSSQ11            0.899       NA                  
    ##    .MPSSQ12            0.980       NA                  
    ##    .MPSSQ13            1.048       NA                  
    ##    .MPSSQ14            0.804       NA                  
    ##    .MPSSQ15            0.818       NA                  
    ##    .MPSSQ16            0.804       NA                  
    ##    .MPSSQ17            0.736       NA                  
    ##    .MPSSQ18            1.947       NA                  
    ##    .ipvq1              1.892       NA                  
    ##    .ipvq2              1.640       NA                  
    ##    .ipvq3              1.613       NA                  
    ##    .ipvq4              1.532       NA                  
    ##    .ipvq5              1.587       NA                  
    ##    .ipvq6              1.751       NA                  
    ##    .ipvq7              1.664       NA                  
    ##    .ipvq8              1.345       NA                  
    ##    .ipvq9              1.370       NA                  
    ##    .ipvq10             1.134       NA                  
    ##    .ipvq11             1.699       NA                  
    ##    .ipvq12             1.177       NA                  
    ##    .ipvq13             1.283       NA                  
    ##    .ipvq14             1.081       NA                  
    ##    .ipvq15             1.829       NA                  
    ##    .bdiq2              0.351       NA                  
    ##    .bdiq3              0.381       NA                  
    ##    .bdiq4              0.562       NA                  
    ##    .bdiq5              0.434       NA                  
    ##    .bdiq6              0.521       NA                  
    ##    .bdiq7              0.328       NA                  
    ##    .bdiq8              0.470       NA                  
    ##    .bdiq10             0.697       NA                  
    ##    .bdiq12             0.523       NA                  
    ##    .bdiq13             0.572       NA                  
    ##    .bdiq14             0.332       NA                  
    ##    .bdiq15             0.728       NA                  
    ##    .bdiq17             0.830       NA                  
    ##    .bdiq20             0.881       NA                  
    ##    .bdiq21             0.932       NA                  
    ##    .bdiq22_1           0.149       NA                  
    ##    .bdiq23_1           0.107       NA                  
    ##    .bdiq24_1           0.094       NA                  
    ##    .bdiq25_1           0.100       NA                  
    ##     pov                0.000                           
    ##    .trauf1             0.000                           
    ##    .trauf2             0.000                           
    ##    .depf1              0.000                           
    ##    .depf2              0.000                           
    ## 
    ## Variances:
    ##                    Estimate   Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1          0.159       NA                  
    ##    .SESaQ14            0.590       NA                  
    ##    .SESaQ15_1          1.979       NA                  
    ##    .SESaQ20_1          0.632       NA                  
    ##    .SESaQ21            0.476       NA                  
    ##    .SESaQ31_1          0.917       NA                  
    ##    .SESaQ32            2.313       NA                  
    ##    .SESaQ34_1          0.352       NA                  
    ##    .sumses9            0.853       NA                  
    ##    .sumses33          77.456       NA                  
    ##    .MPSSQ2             0.643       NA                  
    ##    .MPSSQ3             0.649       NA                  
    ##    .MPSSQ4             0.779       NA                  
    ##    .MPSSQ5             0.650       NA                  
    ##    .MPSSQ6             0.677       NA                  
    ##    .MPSSQ7             0.478       NA                  
    ##    .MPSSQ8             0.444       NA                  
    ##    .MPSSQ9             0.439       NA                  
    ##    .MPSSQ10            0.534       NA                  
    ##    .MPSSQ11            0.612       NA                  
    ##    .MPSSQ12            0.567       NA                  
    ##    .MPSSQ13            0.499       NA                  
    ##    .MPSSQ14            0.376       NA                  
    ##    .MPSSQ15            0.597       NA                  
    ##    .MPSSQ16            0.409       NA                  
    ##    .MPSSQ17            0.490       NA                  
    ##    .MPSSQ18            0.968       NA                  
    ##    .ipvq1              0.477       NA                  
    ##    .ipvq2              0.471       NA                  
    ##    .ipvq3              0.391       NA                  
    ##    .ipvq4              0.383       NA                  
    ##    .ipvq5              0.157       NA                  
    ##    .ipvq6              0.363       NA                  
    ##    .ipvq7              0.397       NA                  
    ##    .ipvq8              0.285       NA                  
    ##    .ipvq9              0.278       NA                  
    ##    .ipvq10             0.158       NA                  
    ##    .ipvq11             0.144       NA                  
    ##    .ipvq12             0.229       NA                  
    ##    .ipvq13             0.307       NA                  
    ##    .ipvq14             0.122       NA                  
    ##    .ipvq15             0.102       NA                  
    ##    .bdiq2              0.309       NA                  
    ##    .bdiq3              0.352       NA                  
    ##    .bdiq4              0.342       NA                  
    ##    .bdiq5              0.241       NA                  
    ##    .bdiq6              0.598       NA                  
    ##    .bdiq7              0.297       NA                  
    ##    .bdiq8              0.487       NA                  
    ##    .bdiq10             0.932       NA                  
    ##    .bdiq12             0.389       NA                  
    ##    .bdiq13             0.479       NA                  
    ##    .bdiq14             0.310       NA                  
    ##    .bdiq15             0.435       NA                  
    ##    .bdiq17             0.460       NA                  
    ##    .bdiq20             0.508       NA                  
    ##    .bdiq21             0.805       NA                  
    ##    .bdiq22_1           0.065       NA                  
    ##    .bdiq23_1           0.063       NA                  
    ##    .bdiq24_1           0.043       NA                  
    ##    .bdiq25_1           0.075       NA                  
    ##     pov               -0.000       NA                  
    ##    .trauf1             0.263       NA                  
    ##    .trauf2             0.598       NA                  
    ##    .depf1              0.119       NA                  
    ##    .depf2              0.051       NA
