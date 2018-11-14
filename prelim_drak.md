prelim drak
================
Tatini Mal-Sarkar
11/14/2018

``` r
drak = read_csv("./data/KerryDataRequestV2_visit1_c.csv") %>% 
  select(-ASSIST_Tobacco_Score:-opioids_preg_any,
         -AQ1a:-AQ14b,
         -pdiq1_afr:-pdiq13_xho,
         -leq1_1_afr:-leq51_2_xho,
         -ipvq1_afr:-ipvq17_xho,
         -epdsq1_afr:-epdsq10_xho,
         -bdiq1_afr:-bdiq25_2_xho,
         -srqq1_afr:-srqq20_xho)
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

``` r
nomed_mod <- 'pov =~ SESaQ13_1 + SESaQ14 + SESaQ15_1 + SESaQ20_1 + SESaQ21 + SESaQ31_1 + SESaQ32 + SESaQ34_1 + sumses9 + sumses33
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21
depf2 =~ bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1

depf1 ~ pov
depf2 ~ pov '

nomed_mod_fit_1 <- sem(nomed_mod, data = drak, missing = 'fiml.x')
nomed_mod_fit_sum_1 = summary(nomed_mod_fit_1)
```

    ## lavaan 0.6-3 ended normally after 91 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         90
    ## 
    ##   Number of observations                          1036
    ##   Number of missing patterns                        25
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    2194.900
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
    ##     SESaQ14          -0.382    0.061   -6.215    0.000
    ##     SESaQ15_1         0.521    0.133    3.923    0.000
    ##     SESaQ20_1         0.386    0.064    5.989    0.000
    ##     SESaQ21           0.292    0.049    5.915    0.000
    ##     SESaQ31_1        -1.844    0.126  -14.669    0.000
    ##     SESaQ32          -0.560    0.098   -5.738    0.000
    ##     SESaQ34_1         0.388    0.050    7.832    0.000
    ##     sumses9          -0.857    0.078  -11.030    0.000
    ##     sumses33         -2.793    0.201  -13.872    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.099    0.067   16.442    0.000
    ##     bdiq4             1.311    0.075   17.515    0.000
    ##     bdiq5             0.924    0.059   15.723    0.000
    ##     bdiq6             1.346    0.080   16.815    0.000
    ##     bdiq7             1.138    0.068   16.859    0.000
    ##     bdiq8             1.480    0.087   17.077    0.000
    ##     bdiq10            1.273    0.088   14.459    0.000
    ##     bdiq12            1.164    0.070   16.654    0.000
    ##     bdiq13            1.265    0.073   17.210    0.000
    ##     bdiq14            1.125    0.065   17.437    0.000
    ##     bdiq15            1.097    0.069   16.018    0.000
    ##     bdiq17            0.965    0.068   14.160    0.000
    ##     bdiq20            1.060    0.072   14.680    0.000
    ##     bdiq21            1.057    0.087   12.144    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.734    0.054   13.486    0.000
    ##     bdiq24_1          0.729    0.049   14.731    0.000
    ##     bdiq25_1          0.491    0.044   11.205    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     pov               0.016    0.032    0.481    0.631
    ##   depf2 ~                                             
    ##     pov              -0.045    0.017   -2.686    0.007
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.026    0.004    7.240    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         1.725    0.029   59.069    0.000
    ##    .SESaQ14           4.356    0.024  180.460    0.000
    ##    .SESaQ15_1         3.295    0.055   59.451    0.000
    ##    .SESaQ20_1         1.627    0.026   63.027    0.000
    ##    .SESaQ21           0.442    0.020   22.420    0.000
    ##    .SESaQ31_1         2.560    0.033   77.346    0.000
    ##    .SESaQ32           2.434    0.040   61.176    0.000
    ##    .SESaQ34_1         1.748    0.019   89.883    0.000
    ##    .sumses9           2.232    0.028   79.775    0.000
    ##    .sumses33          4.779    0.055   86.904    0.000
    ##    .bdiq2             0.357    0.022   16.501    0.000
    ##    .bdiq3             0.405    0.023   17.281    0.000
    ##    .bdiq4             0.588    0.025   23.376    0.000
    ##    .bdiq5             0.404    0.020   19.859    0.000
    ##    .bdiq6             0.457    0.027   16.822    0.000
    ##    .bdiq7             0.378    0.023   16.418    0.000
    ##    .bdiq8             0.525    0.029   17.876    0.000
    ##    .bdiq10            0.582    0.031   18.758    0.000
    ##    .bdiq12            0.500    0.024   21.106    0.000
    ##    .bdiq13            0.490    0.025   19.867    0.000
    ##    .bdiq14            0.333    0.022   15.261    0.000
    ##    .bdiq15            0.585    0.023   25.001    0.000
    ##    .bdiq17            0.618    0.024   25.667    0.000
    ##    .bdiq20            0.690    0.025   27.352    0.000
    ##    .bdiq21            0.774    0.032   24.349    0.000
    ##    .bdiq22_1          0.089    0.009   10.046    0.000
    ##    .bdiq23_1          0.058    0.007    7.976    0.000
    ##    .bdiq24_1          0.046    0.007    7.007    0.000
    ##    .bdiq25_1          0.048    0.007    7.168    0.000
    ##     pov               0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         0.657    0.032   20.699    0.000
    ##    .SESaQ14           0.570    0.025   22.360    0.000
    ##    .SESaQ15_1         3.102    0.137   22.582    0.000
    ##    .SESaQ20_1         0.656    0.029   22.404    0.000
    ##    .SESaQ21           0.378    0.017   22.286    0.000
    ##    .SESaQ31_1         0.366    0.034   10.932    0.000
    ##    .SESaQ32           1.562    0.070   22.433    0.000
    ##    .SESaQ34_1         0.357    0.016   22.116    0.000
    ##    .sumses9           0.643    0.030   21.179    0.000
    ##    .sumses33          1.366    0.092   14.822    0.000
    ##    .bdiq2             0.310    0.015   21.251    0.000
    ##    .bdiq3             0.359    0.017   21.177    0.000
    ##    .bdiq4             0.356    0.017   20.590    0.000
    ##    .bdiq5             0.280    0.013   21.363    0.000
    ##    .bdiq6             0.450    0.022   20.901    0.000
    ##    .bdiq7             0.324    0.015   20.906    0.000
    ##    .bdiq8             0.510    0.025   20.801    0.000
    ##    .bdiq10            0.713    0.033   21.704    0.000
    ##    .bdiq12            0.346    0.016   20.970    0.000
    ##    .bdiq13            0.351    0.017   20.663    0.000
    ##    .bdiq14            0.271    0.013   20.608    0.000
    ##    .bdiq15            0.356    0.017   21.120    0.000
    ##    .bdiq17            0.437    0.020   21.698    0.000
    ##    .bdiq20            0.462    0.021   21.553    0.000
    ##    .bdiq21            0.849    0.038   22.109    0.000
    ##    .bdiq22_1          0.044    0.003   14.870    0.000
    ##    .bdiq23_1          0.035    0.002   17.283    0.000
    ##    .bdiq24_1          0.024    0.002   15.082    0.000
    ##    .bdiq25_1          0.036    0.002   20.471    0.000
    ##     pov               0.225    0.029    7.759    0.000
    ##    .depf1             0.173    0.017   10.133    0.000
    ##    .depf2             0.037    0.004    9.872    0.000

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

med_mod_fit_1 <- sem(med_mod, data = drak, missing = 'fiml.x')
summary(med_mod_fit_1)
```

    ## lavaan 0.6-3 ended normally after 136 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        192
    ## 
    ##   Number of observations                          1036
    ##   Number of missing patterns                        59
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    6169.287
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
    ##     SESaQ14          -0.378    0.061   -6.204    0.000
    ##     SESaQ15_1         0.533    0.132    4.032    0.000
    ##     SESaQ20_1         0.379    0.064    5.924    0.000
    ##     SESaQ21           0.285    0.049    5.832    0.000
    ##     SESaQ31_1        -1.829    0.124  -14.748    0.000
    ##     SESaQ32          -0.556    0.097   -5.741    0.000
    ##     SESaQ34_1         0.387    0.049    7.872    0.000
    ##     sumses9          -0.848    0.077  -11.038    0.000
    ##     sumses33         -2.781    0.200  -13.927    0.000
    ##   trauf1 =~                                           
    ##     MPSSQ2            1.000                           
    ##     MPSSQ3            1.008    0.070   14.359    0.000
    ##     MPSSQ4            0.950    0.069   13.668    0.000
    ##     MPSSQ5            0.961    0.070   13.800    0.000
    ##     MPSSQ6            1.025    0.072   14.276    0.000
    ##     MPSSQ7            0.925    0.066   13.956    0.000
    ##     MPSSQ8            1.054    0.067   15.642    0.000
    ##     MPSSQ9            1.086    0.067   16.268    0.000
    ##     MPSSQ10           1.084    0.069   15.661    0.000
    ##     MPSSQ11           1.102    0.070   15.750    0.000
    ##     MPSSQ12           1.034    0.068   15.174    0.000
    ##     MPSSQ13           0.982    0.067   14.638    0.000
    ##     MPSSQ14           0.995    0.063   15.864    0.000
    ##     MPSSQ15           0.969    0.067   14.462    0.000
    ##     MPSSQ16           1.029    0.065   15.895    0.000
    ##     MPSSQ17           0.968    0.065   14.878    0.000
    ##     MPSSQ18           0.579    0.073    7.943    0.000
    ##   trauf2 =~                                           
    ##     ipvq1             1.000                           
    ##     ipvq2             0.864    0.039   22.426    0.000
    ##     ipvq3             0.877    0.038   22.980    0.000
    ##     ipvq4             0.920    0.038   24.135    0.000
    ##     ipvq5            -0.396    0.022  -18.036    0.000
    ##     ipvq6             0.939    0.042   22.540    0.000
    ##     ipvq7             0.941    0.041   23.203    0.000
    ##     ipvq8             0.739    0.033   22.406    0.000
    ##     ipvq9             0.697    0.031   22.229    0.000
    ##     ipvq10            0.409    0.023   18.159    0.000
    ##     ipvq11           -0.338    0.021  -15.734    0.000
    ##     ipvq12            0.400    0.024   16.826    0.000
    ##     ipvq13            0.418    0.025   16.966    0.000
    ##     ipvq14            0.214    0.015   14.173    0.000
    ##     ipvq15           -0.249    0.016  -15.677    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.098    0.066   16.558    0.000
    ##     bdiq4             1.303    0.074   17.574    0.000
    ##     bdiq5             0.918    0.058   15.765    0.000
    ##     bdiq6             1.343    0.079   16.917    0.000
    ##     bdiq7             1.130    0.067   16.896    0.000
    ##     bdiq8             1.472    0.086   17.137    0.000
    ##     bdiq10            1.266    0.087   14.492    0.000
    ##     bdiq12            1.159    0.069   16.730    0.000
    ##     bdiq13            1.259    0.073   17.285    0.000
    ##     bdiq14            1.119    0.064   17.495    0.000
    ##     bdiq15            1.092    0.068   16.078    0.000
    ##     bdiq17            0.962    0.068   14.221    0.000
    ##     bdiq20            1.052    0.072   14.698    0.000
    ##     bdiq21            1.054    0.086   12.187    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.751    0.055   13.736    0.000
    ##     bdiq24_1          0.734    0.049   15.083    0.000
    ##     bdiq25_1          0.504    0.044   11.381    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf1            0.001    0.028    0.043    0.966
    ##     trauf2            0.176    0.022    8.132    0.000
    ##     pov               0.031    0.034    0.916    0.360
    ##   depf2 ~                                             
    ##     trauf1           -0.004    0.013   -0.357    0.721
    ##     trauf2            0.094    0.011    8.640    0.000
    ##     pov              -0.035    0.017   -2.084    0.037
    ##   trauf1 ~                                            
    ##     pov               0.423    0.104    4.084    0.000
    ##   trauf2 ~                                            
    ##     pov              -0.082    0.054   -1.517    0.129
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.017    0.003    5.637    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         1.725    0.029   59.068    0.000
    ##    .SESaQ14           4.356    0.024  180.460    0.000
    ##    .SESaQ15_1         3.295    0.055   59.451    0.000
    ##    .SESaQ20_1         1.627    0.026   63.027    0.000
    ##    .SESaQ21           0.442    0.020   22.419    0.000
    ##    .SESaQ31_1         2.560    0.033   77.342    0.000
    ##    .SESaQ32           2.434    0.040   61.177    0.000
    ##    .SESaQ34_1         1.748    0.019   89.882    0.000
    ##    .sumses9           2.232    0.028   79.776    0.000
    ##    .sumses33          4.779    0.055   86.908    0.000
    ##    .MPSSQ2            1.229    0.058   21.330    0.000
    ##    .MPSSQ3            1.294    0.059   21.808    0.000
    ##    .MPSSQ4            1.623    0.058   27.791    0.000
    ##    .MPSSQ5            1.502    0.058   25.783    0.000
    ##    .MPSSQ6            1.476    0.060   24.482    0.000
    ##    .MPSSQ7            1.062    0.056   19.023    0.000
    ##    .MPSSQ8            1.219    0.057   21.299    0.000
    ##    .MPSSQ9            1.260    0.057   22.196    0.000
    ##    .MPSSQ10           1.263    0.059   21.471    0.000
    ##    .MPSSQ11           1.283    0.059   21.631    0.000
    ##    .MPSSQ12           1.296    0.058   22.504    0.000
    ##    .MPSSQ13           1.335    0.056   23.696    0.000
    ##    .MPSSQ14           1.161    0.053   21.800    0.000
    ##    .MPSSQ15           1.188    0.056   21.065    0.000
    ##    .MPSSQ16           1.159    0.055   21.092    0.000
    ##    .MPSSQ17           1.115    0.055   20.290    0.000
    ##    .MPSSQ18           1.933    0.058   33.361    0.000
    ##    .ipvq1             1.716    0.031   55.163    0.000
    ##    .ipvq2             1.442    0.027   53.799    0.000
    ##    .ipvq3             1.410    0.026   53.941    0.000
    ##    .ipvq4             1.369    0.026   52.509    0.000
    ##    .ipvq5             1.649    0.016  106.213    0.000
    ##    .ipvq6             1.666    0.029   58.333    0.000
    ##    .ipvq7             1.515    0.028   54.841    0.000
    ##    .ipvq8             1.276    0.022   56.913    0.000
    ##    .ipvq9             1.246    0.021   58.780    0.000
    ##    .ipvq10            1.124    0.015   73.426    0.000
    ##    .ipvq11            1.688    0.015  111.376    0.000
    ##    .ipvq12            1.138    0.016   70.429    0.000
    ##    .ipvq13            1.156    0.017   69.196    0.000
    ##    .ipvq14            1.055    0.010  103.076    0.000
    ##    .ipvq15            1.879    0.011  171.353    0.000
    ##    .bdiq2             0.357    0.022   16.499    0.000
    ##    .bdiq3             0.405    0.023   17.279    0.000
    ##    .bdiq4             0.588    0.025   23.374    0.000
    ##    .bdiq5             0.404    0.020   19.857    0.000
    ##    .bdiq6             0.457    0.027   16.820    0.000
    ##    .bdiq7             0.378    0.023   16.416    0.000
    ##    .bdiq8             0.525    0.029   17.873    0.000
    ##    .bdiq10            0.582    0.031   18.755    0.000
    ##    .bdiq12            0.500    0.024   21.104    0.000
    ##    .bdiq13            0.490    0.025   19.864    0.000
    ##    .bdiq14            0.333    0.022   15.259    0.000
    ##    .bdiq15            0.585    0.023   24.998    0.000
    ##    .bdiq17            0.618    0.024   25.665    0.000
    ##    .bdiq20            0.690    0.025   27.350    0.000
    ##    .bdiq21            0.774    0.032   24.347    0.000
    ##    .bdiq22_1          0.089    0.009   10.038    0.000
    ##    .bdiq23_1          0.058    0.007    7.970    0.000
    ##    .bdiq24_1          0.046    0.007    6.997    0.000
    ##    .bdiq25_1          0.048    0.007    7.163    0.000
    ##     pov               0.000                           
    ##    .trauf1            0.000                           
    ##    .trauf2            0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         0.654    0.032   20.644    0.000
    ##    .SESaQ14           0.570    0.025   22.362    0.000
    ##    .SESaQ15_1         3.099    0.137   22.575    0.000
    ##    .SESaQ20_1         0.657    0.029   22.411    0.000
    ##    .SESaQ21           0.379    0.017   22.295    0.000
    ##    .SESaQ31_1         0.369    0.033   11.100    0.000
    ##    .SESaQ32           1.562    0.070   22.433    0.000
    ##    .SESaQ34_1         0.357    0.016   22.118    0.000
    ##    .sumses9           0.644    0.030   21.204    0.000
    ##    .sumses33          1.359    0.092   14.775    0.000
    ##    .MPSSQ2            0.530    0.042   12.603    0.000
    ##    .MPSSQ3            0.592    0.047   12.701    0.000
    ##    .MPSSQ4            0.627    0.049   12.788    0.000
    ##    .MPSSQ5            0.610    0.048   12.778    0.000
    ##    .MPSSQ6            0.610    0.048   12.685    0.000
    ##    .MPSSQ7            0.555    0.043   12.762    0.000
    ##    .MPSSQ8            0.441    0.036   12.389    0.000
    ##    .MPSSQ9            0.379    0.031   12.158    0.000
    ##    .MPSSQ10           0.467    0.038   12.382    0.000
    ##    .MPSSQ11           0.461    0.037   12.355    0.000
    ##    .MPSSQ12           0.485    0.039   12.505    0.000
    ##    .MPSSQ13           0.503    0.040   12.620    0.000
    ##    .MPSSQ14           0.364    0.030   12.288    0.000
    ##    .MPSSQ15           0.521    0.041   12.680    0.000
    ##    .MPSSQ16           0.384    0.031   12.291    0.000
    ##    .MPSSQ17           0.466    0.037   12.596    0.000
    ##    .MPSSQ18           0.912    0.071   12.800    0.000
    ##    .ipvq1             0.492    0.024   20.686    0.000
    ##    .ipvq2             0.364    0.018   20.746    0.000
    ##    .ipvq3             0.314    0.016   20.283    0.000
    ##    .ipvq4             0.272    0.014   19.724    0.000
    ##    .ipvq5             0.150    0.007   20.480    0.000
    ##    .ipvq6             0.396    0.019   20.448    0.000
    ##    .ipvq7             0.339    0.017   20.137    0.000
    ##    .ipvq8             0.242    0.012   20.535    0.000
    ##    .ipvq9             0.217    0.011   20.531    0.000
    ##    .ipvq10            0.157    0.007   21.722    0.000
    ##    .ipvq11            0.158    0.008   20.745    0.000
    ##    .ipvq12            0.188    0.009   21.863    0.000
    ##    .ipvq13            0.200    0.009   21.802    0.000
    ##    .ipvq14            0.085    0.004   22.186    0.000
    ##    .ipvq15            0.076    0.004   19.828    0.000
    ##    .bdiq2             0.308    0.015   21.236    0.000
    ##    .bdiq3             0.357    0.017   21.165    0.000
    ##    .bdiq4             0.357    0.017   20.617    0.000
    ##    .bdiq5             0.281    0.013   21.378    0.000
    ##    .bdiq6             0.449    0.021   20.894    0.000
    ##    .bdiq7             0.325    0.016   20.933    0.000
    ##    .bdiq8             0.511    0.025   20.823    0.000
    ##    .bdiq10            0.714    0.033   21.714    0.000
    ##    .bdiq12            0.345    0.016   20.979    0.000
    ##    .bdiq13            0.351    0.017   20.675    0.000
    ##    .bdiq14            0.272    0.013   20.632    0.000
    ##    .bdiq15            0.356    0.017   21.127    0.000
    ##    .bdiq17            0.436    0.020   21.700    0.000
    ##    .bdiq20            0.463    0.021   21.570    0.000
    ##    .bdiq21            0.849    0.038   22.110    0.000
    ##    .bdiq22_1          0.045    0.003   15.481    0.000
    ##    .bdiq23_1          0.034    0.002   17.267    0.000
    ##    .bdiq24_1          0.024    0.002   15.485    0.000
    ##    .bdiq25_1          0.036    0.002   20.438    0.000
    ##     pov               0.228    0.029    7.804    0.000
    ##    .trauf1            0.624    0.077    8.130    0.000
    ##    .trauf2            0.506    0.039   12.839    0.000
    ##    .depf1             0.159    0.016   10.181    0.000
    ##    .depf2             0.031    0.003    9.587    0.000