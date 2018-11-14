drak\_mbekweni
================
Tatini Mal-Sarkar
11/14/2018

Mbekweni Model 3
================

Model 3, applied to Mbekweni

``` r
drak_mbek = read_csv("./data/KerryDataRequestV2_visit1_c.csv") %>% 
  select(-ASSIST_Tobacco_Score:-opioids_preg_any,
         -AQ1a:-AQ14b,
         -pdiq1_afr:-pdiq13_xho,
         -leq1_1_afr:-leq51_2_xho,
         -ipvq1_afr:-ipvq17_xho,
         -epdsq1_afr:-epdsq10_xho,
         -bdiq1_afr:-bdiq25_2_xho,
         -srqq1_afr:-srqq20_xho) %>% 
  filter(Clinic == 1)
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

nomed_mod_fit_3 <- sem(nomed_mod, data = drak_mbek, missing = 'fiml.x')
summary(nomed_mod_fit_3)
```

    ## lavaan 0.6-3 ended normally after 106 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         75
    ## 
    ##   Number of observations                           566
    ##   Number of missing patterns                        16
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                     683.612
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
    ##     ethnicity         0.016    0.029    0.560    0.576
    ##     SESaQ31_1         2.319    0.258    8.982    0.000
    ##     sumses33          4.170    0.508    8.209    0.000
    ##     SESaQ34_1        -0.787    0.115   -6.862    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.123    0.087   12.878    0.000
    ##     bdiq4             1.456    0.104   14.014    0.000
    ##     bdiq5             0.935    0.079   11.884    0.000
    ##     bdiq6             1.338    0.096   13.911    0.000
    ##     bdiq7             1.247    0.093   13.437    0.000
    ##     bdiq8             1.564    0.116   13.536    0.000
    ##     bdiq10            1.216    0.102   11.944    0.000
    ##     bdiq12            1.220    0.090   13.537    0.000
    ##     bdiq13            1.276    0.089   14.257    0.000
    ##     bdiq14            1.201    0.084   14.254    0.000
    ##     bdiq15            1.255    0.090   13.944    0.000
    ##     bdiq17            1.004    0.085   11.757    0.000
    ##     bdiq20            1.296    0.097   13.301    0.000
    ##     bdiq21            1.270    0.120   10.562    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.646    0.087    7.393    0.000
    ##     bdiq24_1          0.337    0.087    3.894    0.000
    ##     bdiq25_1          0.367    0.087    4.215    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     pov              -0.163    0.066   -2.480    0.013
    ##   depf2 ~                                             
    ##     pov               0.012    0.018    0.660    0.509
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.009    0.003    2.967    0.003
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           2.025    0.035   58.531    0.000
    ##    .ethnicity         1.021    0.009  118.411    0.000
    ##    .SESaQ31_1         2.169    0.042   52.021    0.000
    ##    .sumses33          4.317    0.076   56.681    0.000
    ##    .SESaQ34_1         1.894    0.026   73.342    0.000
    ##    .bdiq2             0.361    0.029   12.391    0.000
    ##    .bdiq3             0.426    0.032   13.279    0.000
    ##    .bdiq4             0.610    0.036   16.781    0.000
    ##    .bdiq5             0.379    0.029   13.150    0.000
    ##    .bdiq6             0.404    0.034   11.952    0.000
    ##    .bdiq7             0.420    0.033   12.714    0.000
    ##    .bdiq8             0.570    0.041   13.897    0.000
    ##    .bdiq10            0.487    0.037   12.997    0.000
    ##    .bdiq12            0.481    0.032   15.003    0.000
    ##    .bdiq13            0.422    0.031   13.522    0.000
    ##    .bdiq14            0.334    0.030   11.283    0.000
    ##    .bdiq15            0.466    0.031   14.822    0.000
    ##    .bdiq17            0.441    0.031   14.027    0.000
    ##    .bdiq20            0.530    0.035   15.365    0.000
    ##    .bdiq21            0.643    0.045   14.281    0.000
    ##    .bdiq22_1          0.039    0.008    4.781    0.000
    ##    .bdiq23_1          0.018    0.006    3.185    0.001
    ##    .bdiq24_1          0.005    0.003    1.729    0.084
    ##    .bdiq25_1          0.004    0.003    1.410    0.159
    ##     pov               0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           0.556    0.035   15.775    0.000
    ##    .ethnicity         0.042    0.002   16.803    0.000
    ##    .SESaQ31_1         0.348    0.053    6.604    0.000
    ##    .sumses33          1.223    0.172    7.107    0.000
    ##    .SESaQ34_1         0.304    0.019   15.718    0.000
    ##    .bdiq2             0.298    0.019   15.886    0.000
    ##    .bdiq3             0.350    0.022   15.823    0.000
    ##    .bdiq4             0.361    0.024   15.271    0.000
    ##    .bdiq5             0.311    0.019   16.038    0.000
    ##    .bdiq6             0.321    0.021   15.270    0.000
    ##    .bdiq7             0.334    0.022   15.517    0.000
    ##    .bdiq8             0.505    0.033   15.525    0.000
    ##    .bdiq10            0.522    0.033   16.045    0.000
    ##    .bdiq12            0.310    0.020   15.521    0.000
    ##    .bdiq13            0.252    0.017   15.071    0.000
    ##    .bdiq14            0.231    0.015   15.132    0.000
    ##    .bdiq15            0.270    0.018   15.190    0.000
    ##    .bdiq17            0.372    0.023   16.023    0.000
    ##    .bdiq20            0.367    0.024   15.525    0.000
    ##    .bdiq21            0.846    0.052   16.246    0.000
    ##    .bdiq22_1          0.028    0.003    9.560    0.000
    ##    .bdiq23_1          0.013    0.001    9.924    0.000
    ##    .bdiq24_1          0.004    0.000   11.146    0.000
    ##    .bdiq25_1          0.002    0.000    6.770    0.000
    ##     pov               0.118    0.026    4.595    0.000
    ##    .depf1             0.178    0.023    7.799    0.000
    ##    .depf2             0.010    0.003    3.515    0.000

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

med_mod_fit_3 <- sem(med_mod, data = drak_mbek, missing = 'fiml.x')
summary(med_mod_fit_3)
```

    ## lavaan 0.6-3 ended normally after 143 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        123
    ## 
    ##   Number of observations                           566
    ##   Number of missing patterns                        27
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    2547.510
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
    ##     ethnicity         0.017    0.029    0.572    0.567
    ##     SESaQ31_1         2.317    0.258    8.987    0.000
    ##     sumses33          4.167    0.506    8.235    0.000
    ##     SESaQ34_1        -0.789    0.115   -6.877    0.000
    ##   trauf2 =~                                           
    ##     ipvq1             1.000                           
    ##     ipvq2             0.820    0.057   14.515    0.000
    ##     ipvq3             0.784    0.055   14.301    0.000
    ##     ipvq4             0.940    0.059   16.016    0.000
    ##     ipvq5            -0.417    0.035  -11.913    0.000
    ##     ipvq6             1.013    0.070   14.446    0.000
    ##     ipvq7             0.978    0.064   15.173    0.000
    ##     ipvq8             0.794    0.053   15.027    0.000
    ##     ipvq9             0.604    0.044   13.875    0.000
    ##     ipvq10            0.548    0.040   13.763    0.000
    ##     ipvq11           -0.384    0.036  -10.692    0.000
    ##     ipvq12            0.387    0.035   10.942    0.000
    ##     ipvq13            0.178    0.022    8.019    0.000
    ##     ipvq14            0.174    0.019    9.012    0.000
    ##     ipvq15           -0.236    0.022  -10.505    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.123    0.087   12.921    0.000
    ##     bdiq4             1.451    0.103   14.039    0.000
    ##     bdiq5             0.931    0.078   11.889    0.000
    ##     bdiq6             1.334    0.096   13.943    0.000
    ##     bdiq7             1.242    0.092   13.452    0.000
    ##     bdiq8             1.559    0.115   13.562    0.000
    ##     bdiq10            1.212    0.101   11.960    0.000
    ##     bdiq12            1.218    0.090   13.576    0.000
    ##     bdiq13            1.272    0.089   14.286    0.000
    ##     bdiq14            1.198    0.084   14.290    0.000
    ##     bdiq15            1.253    0.090   13.986    0.000
    ##     bdiq17            1.003    0.085   11.790    0.000
    ##     bdiq20            1.293    0.097   13.329    0.000
    ##     bdiq21            1.268    0.120   10.582    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.643    0.087    7.366    0.000
    ##     bdiq24_1          0.340    0.085    3.975    0.000
    ##     bdiq25_1          0.372    0.087    4.280    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf2            0.121    0.033    3.689    0.000
    ##     pov              -0.175    0.066   -2.661    0.008
    ##   depf2 ~                                             
    ##     trauf2            0.018    0.010    1.909    0.056
    ##     pov               0.010    0.018    0.571    0.568
    ##   trauf2 ~                                            
    ##     pov               0.091    0.088    1.031    0.303
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.008    0.003    2.778    0.005
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           2.025    0.035   58.531    0.000
    ##    .ethnicity         1.021    0.009  118.411    0.000
    ##    .SESaQ31_1         2.169    0.042   52.020    0.000
    ##    .sumses33          4.317    0.076   56.681    0.000
    ##    .SESaQ34_1         1.894    0.026   73.342    0.000
    ##    .ipvq1             1.569    0.040   39.491    0.000
    ##    .ipvq2             1.277    0.030   42.013    0.000
    ##    .ipvq3             1.240    0.029   42.906    0.000
    ##    .ipvq4             1.233    0.030   41.104    0.000
    ##    .ipvq5             1.701    0.020   83.203    0.000
    ##    .ipvq6             1.595    0.038   42.404    0.000
    ##    .ipvq7             1.391    0.034   41.359    0.000
    ##    .ipvq8             1.218    0.028   44.089    0.000
    ##    .ipvq9             1.141    0.023   49.855    0.000
    ##    .ipvq10            1.116    0.021   52.805    0.000
    ##    .ipvq11            1.678    0.021   80.444    0.000
    ##    .ipvq12            1.106    0.019   57.076    0.000
    ##    .ipvq13            1.050    0.012   84.777    0.000
    ##    .ipvq14            1.034    0.011   96.523    0.000
    ##    .ipvq15            1.922    0.012  155.140    0.000
    ##    .bdiq2             0.361    0.029   12.389    0.000
    ##    .bdiq3             0.425    0.032   13.278    0.000
    ##    .bdiq4             0.610    0.036   16.779    0.000
    ##    .bdiq5             0.379    0.029   13.148    0.000
    ##    .bdiq6             0.404    0.034   11.950    0.000
    ##    .bdiq7             0.420    0.033   12.713    0.000
    ##    .bdiq8             0.570    0.041   13.895    0.000
    ##    .bdiq10            0.486    0.037   12.995    0.000
    ##    .bdiq12            0.481    0.032   15.001    0.000
    ##    .bdiq13            0.422    0.031   13.520    0.000
    ##    .bdiq14            0.334    0.030   11.282    0.000
    ##    .bdiq15            0.466    0.031   14.820    0.000
    ##    .bdiq17            0.441    0.031   14.026    0.000
    ##    .bdiq20            0.530    0.035   15.363    0.000
    ##    .bdiq21            0.643    0.045   14.280    0.000
    ##    .bdiq22_1          0.039    0.008    4.779    0.000
    ##    .bdiq23_1          0.018    0.006    3.183    0.001
    ##    .bdiq24_1          0.005    0.003    1.727    0.084
    ##    .bdiq25_1          0.004    0.003    1.407    0.159
    ##     pov               0.000                           
    ##    .trauf2            0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           0.556    0.035   15.777    0.000
    ##    .ethnicity         0.042    0.002   16.803    0.000
    ##    .SESaQ31_1         0.349    0.052    6.696    0.000
    ##    .sumses33          1.224    0.170    7.202    0.000
    ##    .SESaQ34_1         0.304    0.019   15.707    0.000
    ##    .ipvq1             0.519    0.033   15.568    0.000
    ##    .ipvq2             0.270    0.018   15.349    0.000
    ##    .ipvq3             0.241    0.016   15.246    0.000
    ##    .ipvq4             0.177    0.013   13.833    0.000
    ##    .ipvq5             0.145    0.010   14.873    0.000
    ##    .ipvq6             0.415    0.027   15.250    0.000
    ##    .ipvq7             0.281    0.019   14.758    0.000
    ##    .ipvq8             0.195    0.013   14.926    0.000
    ##    .ipvq9             0.159    0.010   15.451    0.000
    ##    .ipvq10            0.140    0.009   15.516    0.000
    ##    .ipvq11            0.163    0.011   15.090    0.000
    ##    .ipvq12            0.156    0.010   16.166    0.000
    ##    .ipvq13            0.075    0.005   16.486    0.000
    ##    .ipvq14            0.053    0.003   16.396    0.000
    ##    .ipvq15            0.051    0.004   14.232    0.000
    ##    .bdiq2             0.297    0.019   15.881    0.000
    ##    .bdiq3             0.349    0.022   15.820    0.000
    ##    .bdiq4             0.362    0.024   15.280    0.000
    ##    .bdiq5             0.312    0.019   16.043    0.000
    ##    .bdiq6             0.321    0.021   15.273    0.000
    ##    .bdiq7             0.335    0.022   15.525    0.000
    ##    .bdiq8             0.505    0.033   15.531    0.000
    ##    .bdiq10            0.522    0.033   16.048    0.000
    ##    .bdiq12            0.310    0.020   15.520    0.000
    ##    .bdiq13            0.252    0.017   15.078    0.000
    ##    .bdiq14            0.231    0.015   15.135    0.000
    ##    .bdiq15            0.270    0.018   15.186    0.000
    ##    .bdiq17            0.372    0.023   16.021    0.000
    ##    .bdiq20            0.367    0.024   15.529    0.000
    ##    .bdiq21            0.845    0.052   16.246    0.000
    ##    .bdiq22_1          0.028    0.003    9.709    0.000
    ##    .bdiq23_1          0.013    0.001   10.119    0.000
    ##    .bdiq24_1          0.004    0.000   11.319    0.000
    ##    .bdiq25_1          0.002    0.000    6.756    0.000
    ##     pov               0.118    0.026    4.597    0.000
    ##    .trauf2            0.371    0.045    8.288    0.000
    ##    .depf1             0.173    0.022    7.821    0.000
    ##    .depf2             0.010    0.003    3.536    0.000

Mbekweni Model 1
================

Model 1, applied to Mbekweni

No mediator
-----------

``` r
nomed_mod <- 'pov =~ SESaQ13_1 + SESaQ14 + SESaQ15_1 + SESaQ20_1 + SESaQ21 + SESaQ31_1 + SESaQ32 + SESaQ34_1 + sumses9 + sumses33
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21
depf2 =~ bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1

depf1 ~ pov
depf2 ~ pov '

nomed_mod_fit_1 <- sem(nomed_mod, data = drak_mbek, missing = 'fiml.x')
```

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, :
    ## lavaan WARNING: some observed variances are (at least) a factor 1000 times
    ## larger than others; use varTable(fit) to investigate

``` r
summary(nomed_mod_fit_1)
```

    ## lavaan 0.6-3 ended normally after 114 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         90
    ## 
    ##   Number of observations                           566
    ##   Number of missing patterns                        20
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    1511.026
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
    ##     SESaQ14          -0.489    0.098   -4.984    0.000
    ##     SESaQ15_1        -0.408    0.217   -1.880    0.060
    ##     SESaQ20_1         0.609    0.110    5.529    0.000
    ##     SESaQ21           0.429    0.075    5.731    0.000
    ##     SESaQ31_1        -1.969    0.217   -9.060    0.000
    ##     SESaQ32           0.038    0.099    0.378    0.705
    ##     SESaQ34_1         0.614    0.089    6.915    0.000
    ##     sumses9          -0.886    0.119   -7.459    0.000
    ##     sumses33         -3.179    0.362   -8.781    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.123    0.087   12.876    0.000
    ##     bdiq4             1.455    0.104   14.010    0.000
    ##     bdiq5             0.936    0.079   11.888    0.000
    ##     bdiq6             1.338    0.096   13.913    0.000
    ##     bdiq7             1.248    0.093   13.444    0.000
    ##     bdiq8             1.563    0.116   13.530    0.000
    ##     bdiq10            1.216    0.102   11.946    0.000
    ##     bdiq12            1.220    0.090   13.536    0.000
    ##     bdiq13            1.276    0.090   14.258    0.000
    ##     bdiq14            1.201    0.084   14.252    0.000
    ##     bdiq15            1.254    0.090   13.938    0.000
    ##     bdiq17            1.003    0.085   11.753    0.000
    ##     bdiq20            1.295    0.097   13.294    0.000
    ##     bdiq21            1.269    0.120   10.559    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.646    0.088    7.368    0.000
    ##     bdiq24_1          0.340    0.087    3.925    0.000
    ##     bdiq25_1          0.370    0.087    4.234    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     pov               0.080    0.052    1.562    0.118
    ##   depf2 ~                                             
    ##     pov              -0.012    0.015   -0.786    0.432
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.009    0.003    2.948    0.003
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         2.230    0.040   55.255    0.000
    ##    .SESaQ14           4.340    0.033  131.693    0.000
    ##    .SESaQ15_1         3.798    0.080   47.561    0.000
    ##    .SESaQ20_1         1.609    0.036   44.548    0.000
    ##    .SESaQ21           0.411    0.024   16.990    0.000
    ##    .SESaQ31_1         2.169    0.042   52.020    0.000
    ##    .SESaQ32           2.066    0.037   55.885    0.000
    ##    .SESaQ34_1         1.894    0.026   73.342    0.000
    ##    .sumses9           2.025    0.035   58.544    0.000
    ##    .sumses33          4.318    0.076   56.713    0.000
    ##    .bdiq2             0.361    0.029   12.391    0.000
    ##    .bdiq3             0.426    0.032   13.280    0.000
    ##    .bdiq4             0.610    0.036   16.781    0.000
    ##    .bdiq5             0.379    0.029   13.150    0.000
    ##    .bdiq6             0.404    0.034   11.953    0.000
    ##    .bdiq7             0.420    0.033   12.715    0.000
    ##    .bdiq8             0.570    0.041   13.897    0.000
    ##    .bdiq10            0.487    0.037   12.997    0.000
    ##    .bdiq12            0.481    0.032   15.003    0.000
    ##    .bdiq13            0.422    0.031   13.523    0.000
    ##    .bdiq14            0.334    0.030   11.284    0.000
    ##    .bdiq15            0.466    0.031   14.822    0.000
    ##    .bdiq17            0.441    0.031   14.027    0.000
    ##    .bdiq20            0.530    0.035   15.365    0.000
    ##    .bdiq21            0.643    0.045   14.281    0.000
    ##    .bdiq22_1          0.039    0.008    4.782    0.000
    ##    .bdiq23_1          0.018    0.006    3.185    0.001
    ##    .bdiq24_1          0.005    0.003    1.730    0.084
    ##    .bdiq25_1          0.004    0.003    1.410    0.159
    ##     pov               0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         0.747    0.048   15.720    0.000
    ##    .SESaQ14           0.572    0.035   16.491    0.000
    ##    .SESaQ15_1         3.561    0.213   16.742    0.000
    ##    .SESaQ20_1         0.673    0.041   16.215    0.000
    ##    .SESaQ21           0.299    0.019   16.143    0.000
    ##    .SESaQ31_1         0.311    0.039    7.993    0.000
    ##    .SESaQ32           0.770    0.046   16.792    0.000
    ##    .SESaQ34_1         0.312    0.020   15.921    0.000
    ##    .sumses9           0.538    0.034   15.690    0.000
    ##    .sumses33          1.523    0.130   11.735    0.000
    ##    .bdiq2             0.298    0.019   15.886    0.000
    ##    .bdiq3             0.350    0.022   15.823    0.000
    ##    .bdiq4             0.362    0.024   15.272    0.000
    ##    .bdiq5             0.311    0.019   16.037    0.000
    ##    .bdiq6             0.321    0.021   15.266    0.000
    ##    .bdiq7             0.334    0.022   15.512    0.000
    ##    .bdiq8             0.505    0.033   15.526    0.000
    ##    .bdiq10            0.522    0.033   16.043    0.000
    ##    .bdiq12            0.310    0.020   15.520    0.000
    ##    .bdiq13            0.252    0.017   15.068    0.000
    ##    .bdiq14            0.231    0.015   15.131    0.000
    ##    .bdiq15            0.270    0.018   15.190    0.000
    ##    .bdiq17            0.373    0.023   16.023    0.000
    ##    .bdiq20            0.367    0.024   15.527    0.000
    ##    .bdiq21            0.846    0.052   16.246    0.000
    ##    .bdiq22_1          0.028    0.003    9.636    0.000
    ##    .bdiq23_1          0.013    0.001    9.994    0.000
    ##    .bdiq24_1          0.004    0.000   11.200    0.000
    ##    .bdiq25_1          0.002    0.000    6.740    0.000
    ##     pov               0.173    0.036    4.800    0.000
    ##    .depf1             0.180    0.023    7.807    0.000
    ##    .depf2             0.010    0.003    3.512    0.000

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

med_mod_fit_1 <- sem(med_mod, data = drak_mbek, missing = 'fiml.x')
```

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, :
    ## lavaan WARNING: some observed variances are (at least) a factor 1000 times
    ## larger than others; use varTable(fit) to investigate

``` r
summary(med_mod_fit_1)
```

    ## lavaan 0.6-3 ended normally after 165 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        192
    ## 
    ##   Number of observations                           566
    ##   Number of missing patterns                        43
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    4810.022
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
    ##     SESaQ14          -0.485    0.098   -4.956    0.000
    ##     SESaQ15_1        -0.404    0.217   -1.863    0.062
    ##     SESaQ20_1         0.602    0.110    5.479    0.000
    ##     SESaQ21           0.425    0.075    5.693    0.000
    ##     SESaQ31_1        -1.971    0.217   -9.070    0.000
    ##     SESaQ32           0.034    0.099    0.347    0.728
    ##     SESaQ34_1         0.614    0.089    6.934    0.000
    ##     sumses9          -0.886    0.119   -7.470    0.000
    ##     sumses33         -3.174    0.361   -8.802    0.000
    ##   trauf1 =~                                           
    ##     MPSSQ2            1.000                           
    ##     MPSSQ3            1.042    0.089   11.701    0.000
    ##     MPSSQ4            0.969    0.085   11.442    0.000
    ##     MPSSQ5            0.945    0.087   10.893    0.000
    ##     MPSSQ6            0.942    0.086   10.909    0.000
    ##     MPSSQ7            0.908    0.083   10.985    0.000
    ##     MPSSQ8            0.986    0.082   12.071    0.000
    ##     MPSSQ9            1.064    0.079   13.506    0.000
    ##     MPSSQ10           1.076    0.085   12.736    0.000
    ##     MPSSQ11           1.146    0.084   13.596    0.000
    ##     MPSSQ12           1.071    0.084   12.700    0.000
    ##     MPSSQ13           0.942    0.082   11.426    0.000
    ##     MPSSQ14           0.952    0.076   12.535    0.000
    ##     MPSSQ15           0.925    0.082   11.312    0.000
    ##     MPSSQ16           1.037    0.080   12.941    0.000
    ##     MPSSQ17           0.950    0.081   11.659    0.000
    ##     MPSSQ18           0.555    0.088    6.307    0.000
    ##   trauf2 =~                                           
    ##     ipvq1             1.000                           
    ##     ipvq2             0.820    0.057   14.511    0.000
    ##     ipvq3             0.784    0.055   14.297    0.000
    ##     ipvq4             0.940    0.059   16.010    0.000
    ##     ipvq5            -0.417    0.035  -11.907    0.000
    ##     ipvq6             1.013    0.070   14.445    0.000
    ##     ipvq7             0.979    0.065   15.171    0.000
    ##     ipvq8             0.794    0.053   15.024    0.000
    ##     ipvq9             0.604    0.044   13.872    0.000
    ##     ipvq10            0.548    0.040   13.760    0.000
    ##     ipvq11           -0.384    0.036  -10.690    0.000
    ##     ipvq12            0.387    0.035   10.942    0.000
    ##     ipvq13            0.178    0.022    8.021    0.000
    ##     ipvq14            0.175    0.019    9.015    0.000
    ##     ipvq15           -0.236    0.022  -10.507    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.122    0.087   12.927    0.000
    ##     bdiq4             1.450    0.103   14.047    0.000
    ##     bdiq5             0.931    0.078   11.892    0.000
    ##     bdiq6             1.333    0.096   13.951    0.000
    ##     bdiq7             1.242    0.092   13.462    0.000
    ##     bdiq8             1.558    0.115   13.568    0.000
    ##     bdiq10            1.211    0.101   11.958    0.000
    ##     bdiq12            1.216    0.090   13.576    0.000
    ##     bdiq13            1.273    0.089   14.301    0.000
    ##     bdiq14            1.197    0.084   14.297    0.000
    ##     bdiq15            1.253    0.090   13.996    0.000
    ##     bdiq17            1.002    0.085   11.794    0.000
    ##     bdiq20            1.291    0.097   13.331    0.000
    ##     bdiq21            1.268    0.120   10.596    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.644    0.088    7.335    0.000
    ##     bdiq24_1          0.342    0.085    4.011    0.000
    ##     bdiq25_1          0.374    0.087    4.311    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf1           -0.097    0.038   -2.541    0.011
    ##     trauf2            0.127    0.033    3.838    0.000
    ##     pov               0.075    0.052    1.451    0.147
    ##   depf2 ~                                             
    ##     trauf1           -0.005    0.008   -0.659    0.510
    ##     trauf2            0.019    0.010    1.951    0.051
    ##     pov              -0.011    0.015   -0.766    0.444
    ##   trauf1 ~                                            
    ##     pov              -0.127    0.149   -0.851    0.395
    ##   trauf2 ~                                            
    ##     pov              -0.053    0.072   -0.733    0.464
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.007    0.003    2.711    0.007
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         2.230    0.040   55.256    0.000
    ##    .SESaQ14           4.340    0.033  131.692    0.000
    ##    .SESaQ15_1         3.798    0.080   47.561    0.000
    ##    .SESaQ20_1         1.609    0.036   44.548    0.000
    ##    .SESaQ21           0.411    0.024   16.990    0.000
    ##    .SESaQ31_1         2.169    0.042   52.019    0.000
    ##    .SESaQ32           2.066    0.037   55.885    0.000
    ##    .SESaQ34_1         1.894    0.026   73.342    0.000
    ##    .sumses9           2.025    0.035   58.543    0.000
    ##    .sumses33          4.318    0.076   56.712    0.000
    ##    .MPSSQ2            1.689    0.071   23.815    0.000
    ##    .MPSSQ3            1.645    0.078   20.982    0.000
    ##    .MPSSQ4            1.880    0.074   25.327    0.000
    ##    .MPSSQ5            1.757    0.075   23.403    0.000
    ##    .MPSSQ6            1.821    0.075   24.429    0.000
    ##    .MPSSQ7            1.541    0.072   21.481    0.000
    ##    .MPSSQ8            1.618    0.072   22.411    0.000
    ##    .MPSSQ9            1.591    0.072   22.174    0.000
    ##    .MPSSQ10           1.651    0.076   21.785    0.000
    ##    .MPSSQ11           1.603    0.076   20.960    0.000
    ##    .MPSSQ12           1.561    0.075   20.746    0.000
    ##    .MPSSQ13           1.572    0.072   21.926    0.000
    ##    .MPSSQ14           1.455    0.068   21.484    0.000
    ##    .MPSSQ15           1.492    0.071   21.004    0.000
    ##    .MPSSQ16           1.453    0.072   20.156    0.000
    ##    .MPSSQ17           1.427    0.071   19.983    0.000
    ##    .MPSSQ18           1.922    0.069   27.840    0.000
    ##    .ipvq1             1.569    0.040   39.491    0.000
    ##    .ipvq2             1.277    0.030   42.013    0.000
    ##    .ipvq3             1.239    0.029   42.907    0.000
    ##    .ipvq4             1.233    0.030   41.105    0.000
    ##    .ipvq5             1.701    0.020   83.202    0.000
    ##    .ipvq6             1.595    0.038   42.405    0.000
    ##    .ipvq7             1.391    0.034   41.359    0.000
    ##    .ipvq8             1.218    0.028   44.089    0.000
    ##    .ipvq9             1.141    0.023   49.855    0.000
    ##    .ipvq10            1.116    0.021   52.806    0.000
    ##    .ipvq11            1.678    0.021   80.445    0.000
    ##    .ipvq12            1.106    0.019   57.076    0.000
    ##    .ipvq13            1.050    0.012   84.778    0.000
    ##    .ipvq14            1.034    0.011   96.524    0.000
    ##    .ipvq15            1.922    0.012  155.150    0.000
    ##    .bdiq2             0.361    0.029   12.380    0.000
    ##    .bdiq3             0.425    0.032   13.267    0.000
    ##    .bdiq4             0.610    0.036   16.761    0.000
    ##    .bdiq5             0.379    0.029   13.139    0.000
    ##    .bdiq6             0.404    0.034   11.938    0.000
    ##    .bdiq7             0.420    0.033   12.701    0.000
    ##    .bdiq8             0.570    0.041   13.881    0.000
    ##    .bdiq10            0.486    0.037   12.986    0.000
    ##    .bdiq12            0.481    0.032   14.986    0.000
    ##    .bdiq13            0.422    0.031   13.506    0.000
    ##    .bdiq14            0.334    0.030   11.270    0.000
    ##    .bdiq15            0.466    0.031   14.804    0.000
    ##    .bdiq17            0.441    0.031   14.016    0.000
    ##    .bdiq20            0.530    0.035   15.349    0.000
    ##    .bdiq21            0.643    0.045   14.272    0.000
    ##    .bdiq22_1          0.039    0.008    4.779    0.000
    ##    .bdiq23_1          0.018    0.006    3.183    0.001
    ##    .bdiq24_1          0.005    0.003    1.727    0.084
    ##    .bdiq25_1          0.004    0.003    1.407    0.160
    ##     pov               0.000                           
    ##    .trauf1            0.000                           
    ##    .trauf2            0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         0.747    0.047   15.723    0.000
    ##    .SESaQ14           0.573    0.035   16.497    0.000
    ##    .SESaQ15_1         3.561    0.213   16.743    0.000
    ##    .SESaQ20_1         0.674    0.042   16.233    0.000
    ##    .SESaQ21           0.299    0.019   16.161    0.000
    ##    .SESaQ31_1         0.308    0.039    7.902    0.000
    ##    .SESaQ32           0.770    0.046   16.792    0.000
    ##    .SESaQ34_1         0.312    0.020   15.917    0.000
    ##    .sumses9           0.538    0.034   15.692    0.000
    ##    .sumses33          1.525    0.130   11.763    0.000
    ##    .MPSSQ2            0.373    0.040    9.276    0.000
    ##    .MPSSQ3            0.543    0.057    9.465    0.000
    ##    .MPSSQ4            0.504    0.053    9.461    0.000
    ##    .MPSSQ5            0.562    0.059    9.555    0.000
    ##    .MPSSQ6            0.547    0.057    9.529    0.000
    ##    .MPSSQ7            0.507    0.053    9.557    0.000
    ##    .MPSSQ8            0.428    0.046    9.391    0.000
    ##    .MPSSQ9            0.314    0.035    9.033    0.000
    ##    .MPSSQ10           0.416    0.045    9.251    0.000
    ##    .MPSSQ11           0.339    0.038    8.949    0.000
    ##    .MPSSQ12           0.406    0.044    9.216    0.000
    ##    .MPSSQ13           0.467    0.049    9.474    0.000
    ##    .MPSSQ14           0.342    0.037    9.265    0.000
    ##    .MPSSQ15           0.468    0.049    9.508    0.000
    ##    .MPSSQ16           0.357    0.039    9.182    0.000
    ##    .MPSSQ17           0.449    0.048    9.454    0.000
    ##    .MPSSQ18           0.735    0.076    9.737    0.000
    ##    .ipvq1             0.519    0.033   15.568    0.000
    ##    .ipvq2             0.270    0.018   15.349    0.000
    ##    .ipvq3             0.241    0.016   15.246    0.000
    ##    .ipvq4             0.178    0.013   13.836    0.000
    ##    .ipvq5             0.145    0.010   14.873    0.000
    ##    .ipvq6             0.415    0.027   15.250    0.000
    ##    .ipvq7             0.281    0.019   14.757    0.000
    ##    .ipvq8             0.195    0.013   14.926    0.000
    ##    .ipvq9             0.159    0.010   15.451    0.000
    ##    .ipvq10            0.140    0.009   15.517    0.000
    ##    .ipvq11            0.163    0.011   15.091    0.000
    ##    .ipvq12            0.156    0.010   16.166    0.000
    ##    .ipvq13            0.075    0.005   16.485    0.000
    ##    .ipvq14            0.053    0.003   16.395    0.000
    ##    .ipvq15            0.051    0.004   14.231    0.000
    ##    .bdiq2             0.297    0.019   15.879    0.000
    ##    .bdiq3             0.349    0.022   15.820    0.000
    ##    .bdiq4             0.362    0.024   15.281    0.000
    ##    .bdiq5             0.312    0.019   16.044    0.000
    ##    .bdiq6             0.321    0.021   15.273    0.000
    ##    .bdiq7             0.335    0.022   15.524    0.000
    ##    .bdiq8             0.506    0.033   15.532    0.000
    ##    .bdiq10            0.522    0.033   16.049    0.000
    ##    .bdiq12            0.310    0.020   15.524    0.000
    ##    .bdiq13            0.252    0.017   15.074    0.000
    ##    .bdiq14            0.231    0.015   15.136    0.000
    ##    .bdiq15            0.270    0.018   15.184    0.000
    ##    .bdiq17            0.372    0.023   16.022    0.000
    ##    .bdiq20            0.367    0.024   15.532    0.000
    ##    .bdiq21            0.845    0.052   16.244    0.000
    ##    .bdiq22_1          0.028    0.003    9.800    0.000
    ##    .bdiq23_1          0.013    0.001   10.180    0.000
    ##    .bdiq24_1          0.004    0.000   11.363    0.000
    ##    .bdiq25_1          0.002    0.000    6.753    0.000
    ##     pov               0.174    0.036    4.806    0.000
    ##    .trauf1            0.616    0.092    6.659    0.000
    ##    .trauf2            0.371    0.045    8.286    0.000
    ##    .depf1             0.170    0.022    7.739    0.000
    ##    .depf2             0.010    0.003    3.545    0.000
