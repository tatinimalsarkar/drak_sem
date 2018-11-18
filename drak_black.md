drak\_black
================
Tatini Mal-Sarkar
11/14/2018

Model 3, applied to Black mothers
=================================

``` r
drak_black = read_csv("/Users/tatinimal-sarkar/Documents/mph2/practicum/thesis/data/KerryDataRequestV2_visit1_c.csv") %>%
  select(-ASSIST_Tobacco_Score:-opioids_preg_any,
         -AQ1a:-AQ14b,
         -pdiq1_afr:-pdiq13_xho,
         -leq1_1_afr:-leq51_2_xho,
         -ipvq1_afr:-ipvq17_xho,
         -epdsq1_afr:-epdsq10_xho,
         -bdiq1_afr:-bdiq25_2_xho,
         -srqq1_afr:-srqq20_xho) %>% 
  filter(ethnicity == 1)
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
nomed_mod <- 'pov =~ sumses9 + SESaQ31_1 + sumses33 + SESaQ34_1
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21
depf2 =~ bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1

depf1 ~ pov
depf2 ~ pov '

nomed_mod_fit_3 <- sem(nomed_mod, data = drak_black, missing = 'fiml.x')
summary(nomed_mod_fit_3)
```

    ## lavaan 0.6-3 ended normally after 99 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         72
    ## 
    ##   Number of observations                           566
    ##   Number of missing patterns                        15
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                     643.008
    ##   Degrees of freedom                               227
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
    ##     SESaQ31_1         2.315    0.252    9.192    0.000
    ##     sumses33          4.045    0.468    8.637    0.000
    ##     SESaQ34_1        -0.732    0.107   -6.809    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.112    0.088   12.636    0.000
    ##     bdiq4             1.462    0.106   13.842    0.000
    ##     bdiq5             0.940    0.080   11.802    0.000
    ##     bdiq6             1.348    0.098   13.737    0.000
    ##     bdiq7             1.257    0.094   13.328    0.000
    ##     bdiq8             1.568    0.117   13.353    0.000
    ##     bdiq10            1.235    0.104   11.854    0.000
    ##     bdiq12            1.208    0.091   13.289    0.000
    ##     bdiq13            1.296    0.092   14.089    0.000
    ##     bdiq14            1.206    0.086   13.991    0.000
    ##     bdiq15            1.263    0.092   13.775    0.000
    ##     bdiq17            1.005    0.086   11.648    0.000
    ##     bdiq20            1.291    0.098   13.120    0.000
    ##     bdiq21            1.262    0.121   10.401    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.646    0.088    7.355    0.000
    ##     bdiq24_1          0.341    0.087    3.923    0.000
    ##     bdiq25_1          0.371    0.088    4.222    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     pov              -0.143    0.062   -2.295    0.022
    ##   depf2 ~                                             
    ##     pov               0.011    0.018    0.644    0.520
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.008    0.003    2.915    0.004
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           2.039    0.035   58.674    0.000
    ##    .SESaQ31_1         2.182    0.042   52.201    0.000
    ##    .sumses33          4.315    0.076   56.817    0.000
    ##    .SESaQ34_1         1.899    0.026   72.932    0.000
    ##    .bdiq2             0.365    0.029   12.454    0.000
    ##    .bdiq3             0.427    0.032   13.331    0.000
    ##    .bdiq4             0.624    0.037   17.088    0.000
    ##    .bdiq5             0.387    0.029   13.372    0.000
    ##    .bdiq6             0.410    0.034   12.002    0.000
    ##    .bdiq7             0.426    0.033   12.830    0.000
    ##    .bdiq8             0.575    0.041   13.954    0.000
    ##    .bdiq10            0.501    0.038   13.227    0.000
    ##    .bdiq12            0.485    0.032   15.112    0.000
    ##    .bdiq13            0.431    0.032   13.615    0.000
    ##    .bdiq14            0.339    0.030   11.321    0.000
    ##    .bdiq15            0.475    0.032   15.001    0.000
    ##    .bdiq17            0.448    0.031   14.242    0.000
    ##    .bdiq20            0.536    0.035   15.523    0.000
    ##    .bdiq21            0.657    0.045   14.578    0.000
    ##    .bdiq22_1          0.039    0.008    4.782    0.000
    ##    .bdiq23_1          0.018    0.006    3.186    0.001
    ##    .bdiq24_1          0.005    0.003    1.730    0.084
    ##    .bdiq25_1          0.004    0.003    1.410    0.159
    ##     pov               0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           0.556    0.035   15.827    0.000
    ##    .SESaQ31_1         0.321    0.052    6.191    0.000
    ##    .sumses33          1.224    0.165    7.424    0.000
    ##    .SESaQ34_1         0.317    0.020   15.899    0.000
    ##    .bdiq2             0.304    0.019   15.892    0.000
    ##    .bdiq3             0.357    0.023   15.849    0.000
    ##    .bdiq4             0.368    0.024   15.269    0.000
    ##    .bdiq5             0.312    0.019   16.024    0.000
    ##    .bdiq6             0.330    0.022   15.273    0.000
    ##    .bdiq7             0.336    0.022   15.483    0.000
    ##    .bdiq8             0.516    0.033   15.525    0.000
    ##    .bdiq10            0.533    0.033   16.027    0.000
    ##    .bdiq12            0.318    0.020   15.555    0.000
    ##    .bdiq13            0.261    0.017   15.051    0.000
    ##    .bdiq14            0.243    0.016   15.177    0.000
    ##    .bdiq15            0.276    0.018   15.187    0.000
    ##    .bdiq17            0.374    0.023   16.012    0.000
    ##    .bdiq20            0.371    0.024   15.530    0.000
    ##    .bdiq21            0.854    0.053   16.249    0.000
    ##    .bdiq22_1          0.028    0.003    9.637    0.000
    ##    .bdiq23_1          0.013    0.001    9.988    0.000
    ##    .bdiq24_1          0.004    0.000   11.177    0.000
    ##    .bdiq25_1          0.002    0.000    6.716    0.000
    ##     pov               0.124    0.026    4.759    0.000
    ##    .depf1             0.177    0.023    7.715    0.000
    ##    .depf2             0.010    0.003    3.497    0.000

With mediator
-------------

``` r
med_mod <- 'pov =~ sumses9 + SESaQ31_1 + sumses33 + SESaQ34_1
trauf2 =~ ipvq1 + ipvq2 + ipvq3 + ipvq4 + ipvq5 + ipvq6 + ipvq7 + ipvq8 + ipvq9 + ipvq10 + ipvq11 + ipvq12 + ipvq13 + ipvq14 + ipvq15
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21
depf2 =~ bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1

depf1 ~ trauf2 + pov
depf2 ~ trauf2 + pov
trauf2 ~ pov '

med_mod_fit_3 <- sem(med_mod, data = drak_black, missing = 'fiml.x')
summary(med_mod_fit_3)
```

    ## lavaan 0.6-3 ended normally after 143 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        120
    ## 
    ##   Number of observations                           566
    ##   Number of missing patterns                        25
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    2364.323
    ##   Degrees of freedom                               659
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
    ##     SESaQ31_1         2.313    0.252    9.198    0.000
    ##     sumses33          4.046    0.468    8.649    0.000
    ##     SESaQ34_1        -0.735    0.108   -6.821    0.000
    ##   trauf2 =~                                           
    ##     ipvq1             1.000                           
    ##     ipvq2             0.816    0.055   14.754    0.000
    ##     ipvq3             0.797    0.054   14.649    0.000
    ##     ipvq4             0.947    0.058   16.391    0.000
    ##     ipvq5            -0.418    0.034  -12.264    0.000
    ##     ipvq6             0.990    0.068   14.521    0.000
    ##     ipvq7             0.938    0.062   15.202    0.000
    ##     ipvq8             0.765    0.051   15.033    0.000
    ##     ipvq9             0.593    0.042   14.015    0.000
    ##     ipvq10            0.528    0.038   13.797    0.000
    ##     ipvq11           -0.368    0.035  -10.531    0.000
    ##     ipvq12            0.387    0.035   11.045    0.000
    ##     ipvq13            0.199    0.025    8.137    0.000
    ##     ipvq14            0.170    0.019    8.973    0.000
    ##     ipvq15           -0.235    0.022  -10.625    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.111    0.088   12.679    0.000
    ##     bdiq4             1.456    0.105   13.866    0.000
    ##     bdiq5             0.936    0.079   11.808    0.000
    ##     bdiq6             1.345    0.098   13.772    0.000
    ##     bdiq7             1.253    0.094   13.346    0.000
    ##     bdiq8             1.563    0.117   13.377    0.000
    ##     bdiq10            1.231    0.104   11.871    0.000
    ##     bdiq12            1.206    0.090   13.326    0.000
    ##     bdiq13            1.292    0.092   14.121    0.000
    ##     bdiq14            1.204    0.086   14.025    0.000
    ##     bdiq15            1.262    0.091   13.818    0.000
    ##     bdiq17            1.004    0.086   11.681    0.000
    ##     bdiq20            1.288    0.098   13.147    0.000
    ##     bdiq21            1.259    0.121   10.420    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.643    0.088    7.332    0.000
    ##     bdiq24_1          0.343    0.086    4.002    0.000
    ##     bdiq25_1          0.376    0.088    4.288    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf2            0.120    0.032    3.721    0.000
    ##     pov              -0.155    0.062   -2.491    0.013
    ##   depf2 ~                                             
    ##     trauf2            0.018    0.009    1.889    0.059
    ##     pov               0.010    0.017    0.551    0.582
    ##   trauf2 ~                                            
    ##     pov               0.095    0.087    1.096    0.273
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.008    0.003    2.727    0.006
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           2.039    0.035   58.674    0.000
    ##    .SESaQ31_1         2.182    0.042   52.200    0.000
    ##    .sumses33          4.315    0.076   56.818    0.000
    ##    .SESaQ34_1         1.899    0.026   72.932    0.000
    ##    .ipvq1             1.573    0.040   39.357    0.000
    ##    .ipvq2             1.279    0.031   41.896    0.000
    ##    .ipvq3             1.245    0.029   42.333    0.000
    ##    .ipvq4             1.238    0.030   40.742    0.000
    ##    .ipvq5             1.706    0.020   83.986    0.000
    ##    .ipvq6             1.593    0.038   42.349    0.000
    ##    .ipvq7             1.380    0.033   41.591    0.000
    ##    .ipvq8             1.213    0.027   44.238    0.000
    ##    .ipvq9             1.140    0.023   49.885    0.000
    ##    .ipvq10            1.113    0.021   53.309    0.000
    ##    .ipvq11            1.683    0.021   81.030    0.000
    ##    .ipvq12            1.109    0.020   56.418    0.000
    ##    .ipvq13            1.061    0.014   75.862    0.000
    ##    .ipvq14            1.034    0.011   96.524    0.000
    ##    .ipvq15            1.920    0.012  153.767    0.000
    ##    .bdiq2             0.365    0.029   12.452    0.000
    ##    .bdiq3             0.427    0.032   13.330    0.000
    ##    .bdiq4             0.624    0.037   17.086    0.000
    ##    .bdiq5             0.386    0.029   13.371    0.000
    ##    .bdiq6             0.409    0.034   12.000    0.000
    ##    .bdiq7             0.425    0.033   12.828    0.000
    ##    .bdiq8             0.575    0.041   13.952    0.000
    ##    .bdiq10            0.501    0.038   13.225    0.000
    ##    .bdiq12            0.485    0.032   15.110    0.000
    ##    .bdiq13            0.431    0.032   13.613    0.000
    ##    .bdiq14            0.339    0.030   11.320    0.000
    ##    .bdiq15            0.475    0.032   14.999    0.000
    ##    .bdiq17            0.448    0.031   14.241    0.000
    ##    .bdiq20            0.536    0.035   15.521    0.000
    ##    .bdiq21            0.657    0.045   14.577    0.000
    ##    .bdiq22_1          0.039    0.008    4.779    0.000
    ##    .bdiq23_1          0.018    0.006    3.184    0.001
    ##    .bdiq24_1          0.005    0.003    1.727    0.084
    ##    .bdiq25_1          0.004    0.003    1.407    0.159
    ##     pov               0.000                           
    ##    .trauf2            0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           0.556    0.035   15.828    0.000
    ##    .SESaQ31_1         0.322    0.051    6.276    0.000
    ##    .sumses33          1.223    0.163    7.498    0.000
    ##    .SESaQ34_1         0.317    0.020   15.889    0.000
    ##    .ipvq1             0.516    0.033   15.533    0.000
    ##    .ipvq2             0.269    0.018   15.324    0.000
    ##    .ipvq3             0.243    0.016   15.147    0.000
    ##    .ipvq4             0.175    0.013   13.634    0.000
    ##    .ipvq5             0.140    0.009   14.843    0.000
    ##    .ipvq6             0.420    0.027   15.276    0.000
    ##    .ipvq7             0.282    0.019   14.842    0.000
    ##    .ipvq8             0.198    0.013   15.023    0.000
    ##    .ipvq9             0.159    0.010   15.460    0.000
    ##    .ipvq10            0.138    0.009   15.562    0.000
    ##    .ipvq11            0.165    0.011   15.148    0.000
    ##    .ipvq12            0.160    0.010   16.174    0.000
    ##    .ipvq13            0.095    0.006   16.489    0.000
    ##    .ipvq14            0.054    0.003   16.420    0.000
    ##    .ipvq15            0.052    0.004   14.281    0.000
    ##    .bdiq2             0.303    0.019   15.886    0.000
    ##    .bdiq3             0.356    0.022   15.846    0.000
    ##    .bdiq4             0.369    0.024   15.279    0.000
    ##    .bdiq5             0.313    0.020   16.029    0.000
    ##    .bdiq6             0.330    0.022   15.274    0.000
    ##    .bdiq7             0.337    0.022   15.491    0.000
    ##    .bdiq8             0.516    0.033   15.532    0.000
    ##    .bdiq10            0.534    0.033   16.031    0.000
    ##    .bdiq12            0.317    0.020   15.555    0.000
    ##    .bdiq13            0.261    0.017   15.057    0.000
    ##    .bdiq14            0.243    0.016   15.181    0.000
    ##    .bdiq15            0.276    0.018   15.183    0.000
    ##    .bdiq17            0.373    0.023   16.011    0.000
    ##    .bdiq20            0.371    0.024   15.534    0.000
    ##    .bdiq21            0.854    0.053   16.249    0.000
    ##    .bdiq22_1          0.028    0.003    9.780    0.000
    ##    .bdiq23_1          0.013    0.001   10.180    0.000
    ##    .bdiq24_1          0.004    0.000   11.350    0.000
    ##    .bdiq25_1          0.002    0.000    6.710    0.000
    ##     pov               0.124    0.026    4.758    0.000
    ##    .trauf2            0.384    0.046    8.409    0.000
    ##    .depf1             0.172    0.022    7.738    0.000
    ##    .depf2             0.010    0.003    3.522    0.000

Mbekweni Model 1
================

Model 1, applied to Black mothers

No mediator
-----------

``` r
nomed_mod <- 'pov =~ SESaQ13_1 + SESaQ14 + SESaQ15_1 + SESaQ20_1 + SESaQ21 + SESaQ31_1 + SESaQ32 + SESaQ34_1 + sumses9 + sumses33
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21
depf2 =~ bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1

depf1 ~ pov
depf2 ~ pov '

nomed_mod_fit_1 <- sem(nomed_mod, data = drak_black, missing = 'fiml.x')
```

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, :
    ## lavaan WARNING: some observed variances are (at least) a factor 1000 times
    ## larger than others; use varTable(fit) to investigate

``` r
summary(nomed_mod_fit_1)
```

    ## lavaan 0.6-3 ended normally after 113 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         90
    ## 
    ##   Number of observations                           566
    ##   Number of missing patterns                        20
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    1489.161
    ##   Degrees of freedom                               374
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
    ##     SESaQ13_1         1.000                           
    ##     SESaQ14          -0.485    0.099   -4.888    0.000
    ##     SESaQ15_1        -0.359    0.218   -1.649    0.099
    ##     SESaQ20_1         0.595    0.110    5.419    0.000
    ##     SESaQ21           0.415    0.074    5.578    0.000
    ##     SESaQ31_1        -2.025    0.222   -9.105    0.000
    ##     SESaQ32           0.030    0.100    0.299    0.765
    ##     SESaQ34_1         0.596    0.088    6.757    0.000
    ##     sumses9          -0.905    0.121   -7.494    0.000
    ##     sumses33         -3.220    0.364   -8.844    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.111    0.088   12.634    0.000
    ##     bdiq4             1.461    0.106   13.839    0.000
    ##     bdiq5             0.941    0.080   11.805    0.000
    ##     bdiq6             1.349    0.098   13.739    0.000
    ##     bdiq7             1.258    0.094   13.335    0.000
    ##     bdiq8             1.567    0.117   13.348    0.000
    ##     bdiq10            1.235    0.104   11.856    0.000
    ##     bdiq12            1.208    0.091   13.288    0.000
    ##     bdiq13            1.296    0.092   14.090    0.000
    ##     bdiq14            1.206    0.086   13.989    0.000
    ##     bdiq15            1.263    0.092   13.772    0.000
    ##     bdiq17            1.005    0.086   11.644    0.000
    ##     bdiq20            1.290    0.098   13.114    0.000
    ##     bdiq21            1.261    0.121   10.399    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.645    0.088    7.335    0.000
    ##     bdiq24_1          0.343    0.087    3.950    0.000
    ##     bdiq25_1          0.374    0.088    4.240    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     pov               0.077    0.051    1.497    0.134
    ##   depf2 ~                                             
    ##     pov              -0.011    0.015   -0.752    0.452
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.008    0.003    2.900    0.004
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         2.235    0.040   55.301    0.000
    ##    .SESaQ14           4.347    0.033  130.549    0.000
    ##    .SESaQ15_1         3.782    0.080   47.114    0.000
    ##    .SESaQ20_1         1.609    0.036   44.548    0.000
    ##    .SESaQ21           0.411    0.024   16.990    0.000
    ##    .SESaQ31_1         2.181    0.042   52.200    0.000
    ##    .SESaQ32           2.071    0.037   55.862    0.000
    ##    .SESaQ34_1         1.899    0.026   72.932    0.000
    ##    .sumses9           2.040    0.035   58.686    0.000
    ##    .sumses33          4.316    0.076   56.846    0.000
    ##    .bdiq2             0.365    0.029   12.454    0.000
    ##    .bdiq3             0.427    0.032   13.332    0.000
    ##    .bdiq4             0.624    0.037   17.088    0.000
    ##    .bdiq5             0.387    0.029   13.373    0.000
    ##    .bdiq6             0.410    0.034   12.002    0.000
    ##    .bdiq7             0.426    0.033   12.831    0.000
    ##    .bdiq8             0.575    0.041   13.954    0.000
    ##    .bdiq10            0.501    0.038   13.227    0.000
    ##    .bdiq12            0.485    0.032   15.112    0.000
    ##    .bdiq13            0.431    0.032   13.615    0.000
    ##    .bdiq14            0.339    0.030   11.321    0.000
    ##    .bdiq15            0.475    0.032   15.001    0.000
    ##    .bdiq17            0.448    0.031   14.243    0.000
    ##    .bdiq20            0.536    0.035   15.523    0.000
    ##    .bdiq21            0.657    0.045   14.578    0.000
    ##    .bdiq22_1          0.039    0.008    4.782    0.000
    ##    .bdiq23_1          0.018    0.006    3.186    0.001
    ##    .bdiq24_1          0.005    0.003    1.730    0.084
    ##    .bdiq25_1          0.004    0.003    1.410    0.159
    ##     pov               0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         0.753    0.048   15.815    0.000
    ##    .SESaQ14           0.586    0.035   16.523    0.000
    ##    .SESaQ15_1         3.605    0.215   16.753    0.000
    ##    .SESaQ20_1         0.677    0.042   16.308    0.000
    ##    .SESaQ21           0.301    0.019   16.252    0.000
    ##    .SESaQ31_1         0.288    0.039    7.473    0.000
    ##    .SESaQ32           0.775    0.046   16.792    0.000
    ##    .SESaQ34_1         0.323    0.020   16.068    0.000
    ##    .sumses9           0.541    0.034   15.722    0.000
    ##    .sumses33          1.487    0.126   11.840    0.000
    ##    .bdiq2             0.304    0.019   15.892    0.000
    ##    .bdiq3             0.357    0.023   15.849    0.000
    ##    .bdiq4             0.369    0.024   15.270    0.000
    ##    .bdiq5             0.312    0.019   16.022    0.000
    ##    .bdiq6             0.330    0.022   15.270    0.000
    ##    .bdiq7             0.336    0.022   15.478    0.000
    ##    .bdiq8             0.516    0.033   15.527    0.000
    ##    .bdiq10            0.533    0.033   16.026    0.000
    ##    .bdiq12            0.318    0.020   15.554    0.000
    ##    .bdiq13            0.260    0.017   15.048    0.000
    ##    .bdiq14            0.243    0.016   15.177    0.000
    ##    .bdiq15            0.276    0.018   15.187    0.000
    ##    .bdiq17            0.374    0.023   16.013    0.000
    ##    .bdiq20            0.371    0.024   15.531    0.000
    ##    .bdiq21            0.854    0.053   16.249    0.000
    ##    .bdiq22_1          0.028    0.003    9.701    0.000
    ##    .bdiq23_1          0.013    0.001   10.050    0.000
    ##    .bdiq24_1          0.004    0.000   11.226    0.000
    ##    .bdiq25_1          0.002    0.000    6.694    0.000
    ##     pov               0.171    0.036    4.786    0.000
    ##    .depf1             0.179    0.023    7.721    0.000
    ##    .depf2             0.010    0.003    3.497    0.000

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

med_mod_fit_1 <- sem(med_mod, data = drak_black, missing = 'fiml.x')
```

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, :
    ## lavaan WARNING: some observed variances are (at least) a factor 1000 times
    ## larger than others; use varTable(fit) to investigate

``` r
summary(med_mod_fit_1)
```

    ## lavaan 0.6-3 ended normally after 160 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        192
    ## 
    ##   Number of observations                           566
    ##   Number of missing patterns                        42
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    4643.551
    ##   Degrees of freedom                              1760
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
    ##     SESaQ13_1         1.000                           
    ##     SESaQ14          -0.481    0.099   -4.859    0.000
    ##     SESaQ15_1        -0.355    0.218   -1.630    0.103
    ##     SESaQ20_1         0.588    0.109    5.369    0.000
    ##     SESaQ21           0.411    0.074    5.540    0.000
    ##     SESaQ31_1        -2.029    0.223   -9.111    0.000
    ##     SESaQ32           0.027    0.100    0.270    0.787
    ##     SESaQ34_1         0.596    0.088    6.775    0.000
    ##     sumses9          -0.905    0.121   -7.504    0.000
    ##     sumses33         -3.215    0.363   -8.864    0.000
    ##   trauf1 =~                                           
    ##     MPSSQ2            1.000                           
    ##     MPSSQ3            1.039    0.091   11.365    0.000
    ##     MPSSQ4            0.983    0.087   11.301    0.000
    ##     MPSSQ5            0.951    0.090   10.601    0.000
    ##     MPSSQ6            0.948    0.088   10.722    0.000
    ##     MPSSQ7            0.905    0.085   10.609    0.000
    ##     MPSSQ8            1.007    0.084   11.954    0.000
    ##     MPSSQ9            1.059    0.081   13.078    0.000
    ##     MPSSQ10           1.082    0.086   12.551    0.000
    ##     MPSSQ11           1.135    0.087   12.988    0.000
    ##     MPSSQ12           1.052    0.087   12.124    0.000
    ##     MPSSQ13           0.938    0.085   11.087    0.000
    ##     MPSSQ14           0.943    0.079   12.001    0.000
    ##     MPSSQ15           0.926    0.084   10.983    0.000
    ##     MPSSQ16           1.040    0.082   12.624    0.000
    ##     MPSSQ17           0.943    0.083   11.312    0.000
    ##     MPSSQ18           0.567    0.090    6.310    0.000
    ##   trauf2 =~                                           
    ##     ipvq1             1.000                           
    ##     ipvq2             0.816    0.055   14.750    0.000
    ##     ipvq3             0.797    0.054   14.645    0.000
    ##     ipvq4             0.947    0.058   16.384    0.000
    ##     ipvq5            -0.418    0.034  -12.259    0.000
    ##     ipvq6             0.991    0.068   14.520    0.000
    ##     ipvq7             0.938    0.062   15.200    0.000
    ##     ipvq8             0.765    0.051   15.030    0.000
    ##     ipvq9             0.593    0.042   14.013    0.000
    ##     ipvq10            0.528    0.038   13.795    0.000
    ##     ipvq11           -0.368    0.035  -10.530    0.000
    ##     ipvq12            0.387    0.035   11.046    0.000
    ##     ipvq13            0.199    0.025    8.138    0.000
    ##     ipvq14            0.170    0.019    8.976    0.000
    ##     ipvq15           -0.236    0.022  -10.627    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.110    0.087   12.686    0.000
    ##     bdiq4             1.455    0.105   13.875    0.000
    ##     bdiq5             0.935    0.079   11.811    0.000
    ##     bdiq6             1.344    0.098   13.782    0.000
    ##     bdiq7             1.252    0.094   13.355    0.000
    ##     bdiq8             1.562    0.117   13.384    0.000
    ##     bdiq10            1.229    0.104   11.869    0.000
    ##     bdiq12            1.204    0.090   13.328    0.000
    ##     bdiq13            1.292    0.091   14.136    0.000
    ##     bdiq14            1.202    0.086   14.032    0.000
    ##     bdiq15            1.262    0.091   13.833    0.000
    ##     bdiq17            1.004    0.086   11.686    0.000
    ##     bdiq20            1.286    0.098   13.152    0.000
    ##     bdiq21            1.260    0.121   10.434    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.643    0.088    7.305    0.000
    ##     bdiq24_1          0.345    0.086    4.034    0.000
    ##     bdiq25_1          0.377    0.087    4.318    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf1           -0.096    0.038   -2.522    0.012
    ##     trauf2            0.126    0.033    3.878    0.000
    ##     pov               0.069    0.052    1.337    0.181
    ##   depf2 ~                                             
    ##     trauf1           -0.005    0.008   -0.681    0.496
    ##     trauf2            0.018    0.009    1.933    0.053
    ##     pov              -0.011    0.015   -0.739    0.460
    ##   trauf1 ~                                            
    ##     pov              -0.159    0.149   -1.067    0.286
    ##   trauf2 ~                                            
    ##     pov              -0.059    0.073   -0.802    0.423
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.007    0.003    2.662    0.008
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         2.235    0.040   55.302    0.000
    ##    .SESaQ14           4.347    0.033  130.549    0.000
    ##    .SESaQ15_1         3.782    0.080   47.114    0.000
    ##    .SESaQ20_1         1.609    0.036   44.548    0.000
    ##    .SESaQ21           0.411    0.024   16.990    0.000
    ##    .SESaQ31_1         2.181    0.042   52.199    0.000
    ##    .SESaQ32           2.071    0.037   55.862    0.000
    ##    .SESaQ34_1         1.899    0.026   72.932    0.000
    ##    .sumses9           2.040    0.035   58.685    0.000
    ##    .sumses33          4.316    0.076   56.846    0.000
    ##    .MPSSQ2            1.688    0.072   23.547    0.000
    ##    .MPSSQ3            1.654    0.078   21.329    0.000
    ##    .MPSSQ4            1.886    0.074   25.604    0.000
    ##    .MPSSQ5            1.755    0.075   23.390    0.000
    ##    .MPSSQ6            1.818    0.074   24.610    0.000
    ##    .MPSSQ7            1.542    0.071   21.578    0.000
    ##    .MPSSQ8            1.629    0.072   22.663    0.000
    ##    .MPSSQ9            1.581    0.070   22.438    0.000
    ##    .MPSSQ10           1.646    0.074   22.099    0.000
    ##    .MPSSQ11           1.618    0.076   21.377    0.000
    ##    .MPSSQ12           1.556    0.074   20.968    0.000
    ##    .MPSSQ13           1.592    0.071   22.426    0.000
    ##    .MPSSQ14           1.462    0.067   21.744    0.000
    ##    .MPSSQ15           1.503    0.071   21.237    0.000
    ##    .MPSSQ16           1.470    0.071   20.648    0.000
    ##    .MPSSQ17           1.430    0.070   20.284    0.000
    ##    .MPSSQ18           1.948    0.069   28.053    0.000
    ##    .ipvq1             1.573    0.040   39.357    0.000
    ##    .ipvq2             1.279    0.031   41.897    0.000
    ##    .ipvq3             1.245    0.029   42.333    0.000
    ##    .ipvq4             1.238    0.030   40.743    0.000
    ##    .ipvq5             1.706    0.020   83.986    0.000
    ##    .ipvq6             1.593    0.038   42.349    0.000
    ##    .ipvq7             1.380    0.033   41.592    0.000
    ##    .ipvq8             1.213    0.027   44.238    0.000
    ##    .ipvq9             1.140    0.023   49.886    0.000
    ##    .ipvq10            1.113    0.021   53.310    0.000
    ##    .ipvq11            1.683    0.021   81.032    0.000
    ##    .ipvq12            1.109    0.020   56.418    0.000
    ##    .ipvq13            1.061    0.014   75.862    0.000
    ##    .ipvq14            1.034    0.011   96.524    0.000
    ##    .ipvq15            1.920    0.012  153.777    0.000
    ##    .bdiq2             0.365    0.029   12.443    0.000
    ##    .bdiq3             0.427    0.032   13.319    0.000
    ##    .bdiq4             0.624    0.037   17.068    0.000
    ##    .bdiq5             0.386    0.029   13.362    0.000
    ##    .bdiq6             0.409    0.034   11.988    0.000
    ##    .bdiq7             0.425    0.033   12.816    0.000
    ##    .bdiq8             0.575    0.041   13.939    0.000
    ##    .bdiq10            0.501    0.038   13.216    0.000
    ##    .bdiq12            0.485    0.032   15.095    0.000
    ##    .bdiq13            0.431    0.032   13.598    0.000
    ##    .bdiq14            0.339    0.030   11.307    0.000
    ##    .bdiq15            0.475    0.032   14.983    0.000
    ##    .bdiq17            0.448    0.031   14.232    0.000
    ##    .bdiq20            0.536    0.035   15.507    0.000
    ##    .bdiq21            0.657    0.045   14.569    0.000
    ##    .bdiq22_1          0.039    0.008    4.779    0.000
    ##    .bdiq23_1          0.018    0.006    3.183    0.001
    ##    .bdiq24_1          0.005    0.003    1.727    0.084
    ##    .bdiq25_1          0.004    0.003    1.407    0.159
    ##     pov               0.000                           
    ##    .trauf1            0.000                           
    ##    .trauf2            0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         0.752    0.048   15.819    0.000
    ##    .SESaQ14           0.587    0.036   16.529    0.000
    ##    .SESaQ15_1         3.606    0.215   16.754    0.000
    ##    .SESaQ20_1         0.678    0.042   16.325    0.000
    ##    .SESaQ21           0.302    0.019   16.268    0.000
    ##    .SESaQ31_1         0.285    0.039    7.363    0.000
    ##    .SESaQ32           0.775    0.046   16.792    0.000
    ##    .SESaQ34_1         0.323    0.020   16.066    0.000
    ##    .sumses9           0.541    0.034   15.725    0.000
    ##    .sumses33          1.491    0.126   11.880    0.000
    ##    .MPSSQ2            0.418    0.044    9.424    0.000
    ##    .MPSSQ3            0.547    0.057    9.534    0.000
    ##    .MPSSQ4            0.495    0.052    9.503    0.000
    ##    .MPSSQ5            0.578    0.060    9.626    0.000
    ##    .MPSSQ6            0.544    0.057    9.587    0.000
    ##    .MPSSQ7            0.526    0.055    9.635    0.000
    ##    .MPSSQ8            0.416    0.044    9.409    0.000
    ##    .MPSSQ9            0.308    0.034    9.081    0.000
    ##    .MPSSQ10           0.394    0.043    9.265    0.000
    ##    .MPSSQ11           0.358    0.039    9.076    0.000
    ##    .MPSSQ12           0.427    0.046    9.348    0.000
    ##    .MPSSQ13           0.474    0.050    9.557    0.000
    ##    .MPSSQ14           0.361    0.038    9.376    0.000
    ##    .MPSSQ15           0.482    0.050    9.587    0.000
    ##    .MPSSQ16           0.351    0.038    9.219    0.000
    ##    .MPSSQ17           0.454    0.048    9.533    0.000
    ##    .MPSSQ18           0.755    0.077    9.808    0.000
    ##    .ipvq1             0.516    0.033   15.533    0.000
    ##    .ipvq2             0.269    0.018   15.324    0.000
    ##    .ipvq3             0.243    0.016   15.148    0.000
    ##    .ipvq4             0.175    0.013   13.640    0.000
    ##    .ipvq5             0.140    0.009   14.844    0.000
    ##    .ipvq6             0.420    0.027   15.276    0.000
    ##    .ipvq7             0.282    0.019   14.842    0.000
    ##    .ipvq8             0.198    0.013   15.022    0.000
    ##    .ipvq9             0.159    0.010   15.460    0.000
    ##    .ipvq10            0.138    0.009   15.562    0.000
    ##    .ipvq11            0.165    0.011   15.148    0.000
    ##    .ipvq12            0.160    0.010   16.173    0.000
    ##    .ipvq13            0.095    0.006   16.489    0.000
    ##    .ipvq14            0.054    0.003   16.419    0.000
    ##    .ipvq15            0.052    0.004   14.280    0.000
    ##    .bdiq2             0.303    0.019   15.885    0.000
    ##    .bdiq3             0.356    0.022   15.846    0.000
    ##    .bdiq4             0.369    0.024   15.280    0.000
    ##    .bdiq5             0.313    0.020   16.030    0.000
    ##    .bdiq6             0.330    0.022   15.274    0.000
    ##    .bdiq7             0.337    0.022   15.491    0.000
    ##    .bdiq8             0.517    0.033   15.533    0.000
    ##    .bdiq10            0.534    0.033   16.032    0.000
    ##    .bdiq12            0.318    0.020   15.558    0.000
    ##    .bdiq13            0.261    0.017   15.054    0.000
    ##    .bdiq14            0.243    0.016   15.184    0.000
    ##    .bdiq15            0.275    0.018   15.179    0.000
    ##    .bdiq17            0.373    0.023   16.011    0.000
    ##    .bdiq20            0.371    0.024   15.536    0.000
    ##    .bdiq21            0.853    0.053   16.247    0.000
    ##    .bdiq22_1          0.028    0.003    9.863    0.000
    ##    .bdiq23_1          0.014    0.001   10.228    0.000
    ##    .bdiq24_1          0.004    0.000   11.385    0.000
    ##    .bdiq25_1          0.002    0.000    6.719    0.000
    ##     pov               0.171    0.036    4.791    0.000
    ##    .trauf1            0.606    0.094    6.467    0.000
    ##    .trauf2            0.384    0.046    8.408    0.000
    ##    .depf1             0.169    0.022    7.661    0.000
    ##    .depf2             0.010    0.003    3.534    0.000
