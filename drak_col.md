drak\_col
================
Tatini Mal-Sarkar
11/14/2018

Model 3, applied to Colored mothers
===================================

``` r
drak_col = read_csv("./data/KerryDataRequestV2_visit1_c.csv") %>%
  select(-ASSIST_Tobacco_Score:-opioids_preg_any,
         -AQ1a:-AQ14b,
         -pdiq1_afr:-pdiq13_xho,
         -leq1_1_afr:-leq51_2_xho,
         -ipvq1_afr:-ipvq17_xho,
         -epdsq1_afr:-epdsq10_xho,
         -bdiq1_afr:-bdiq25_2_xho,
         -srqq1_afr:-srqq20_xho) %>% 
  filter(ethnicity == 3)
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

nomed_mod_fit_3 <- sem(nomed_mod, data = drak_col, missing = 'fiml.x')
```

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
    ## variances are negative

``` r
summary(nomed_mod_fit_3)
```

    ## lavaan 0.6-3 ended normally after 116 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         72
    ## 
    ##   Number of observations                           469
    ##   Number of missing patterns                         7
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                     532.301
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
    ##     SESaQ31_1        10.274    7.386    1.391    0.164
    ##     sumses33          5.113    1.718    2.977    0.003
    ##     SESaQ34_1         0.949    0.353    2.688    0.007
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.087    0.100   10.869    0.000
    ##     bdiq4             1.086    0.099   10.917    0.000
    ##     bdiq5             0.887    0.084   10.548    0.000
    ##     bdiq6             1.303    0.128   10.206    0.000
    ##     bdiq7             0.970    0.092   10.588    0.000
    ##     bdiq8             1.372    0.124   11.033    0.000
    ##     bdiq10            1.288    0.146    8.828    0.000
    ##     bdiq12            1.086    0.106   10.261    0.000
    ##     bdiq13            1.157    0.113   10.196    0.000
    ##     bdiq14            1.025    0.094   10.852    0.000
    ##     bdiq15            0.784    0.095    8.261    0.000
    ##     bdiq17            0.808    0.098    8.210    0.000
    ##     bdiq20            0.645    0.096    6.726    0.000
    ##     bdiq21            0.699    0.117    6.001    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.701    0.078    9.039    0.000
    ##     bdiq24_1          0.807    0.074   10.870    0.000
    ##     bdiq25_1          0.482    0.067    7.183    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     pov               0.126    0.131    0.957    0.338
    ##   depf2 ~                                             
    ##     pov              -0.057    0.085   -0.671    0.502
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.048    0.008    6.246    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           2.470    0.043   57.636    0.000
    ##    .SESaQ31_1         3.014    0.045   67.583    0.000
    ##    .sumses33          5.331    0.072   74.337    0.000
    ##    .SESaQ34_1         1.568    0.027   58.149    0.000
    ##    .bdiq2             0.348    0.032   10.834    0.000
    ##    .bdiq3             0.380    0.034   11.030    0.000
    ##    .bdiq4             0.546    0.034   16.182    0.000
    ##    .bdiq5             0.426    0.028   15.045    0.000
    ##    .bdiq6             0.516    0.044   11.849    0.000
    ##    .bdiq7             0.322    0.031   10.313    0.000
    ##    .bdiq8             0.465    0.041   11.233    0.000
    ##    .bdiq10            0.681    0.051   13.434    0.000
    ##    .bdiq12            0.520    0.035   14.766    0.000
    ##    .bdiq13            0.563    0.039   14.590    0.000
    ##    .bdiq14            0.326    0.032   10.244    0.000
    ##    .bdiq15            0.719    0.034   21.278    0.000
    ##    .bdiq17            0.823    0.035   23.565    0.000
    ##    .bdiq20            0.876    0.035   24.974    0.000
    ##    .bdiq21            0.917    0.044   21.038    0.000
    ##    .bdiq22_1          0.150    0.016    9.078    0.000
    ##    .bdiq23_1          0.107    0.014    7.486    0.000
    ##    .bdiq24_1          0.094    0.013    6.973    0.000
    ##    .bdiq25_1          0.100    0.014    7.230    0.000
    ##     pov               0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           0.842    0.056   14.984    0.000
    ##    .SESaQ31_1        -0.673    0.756   -0.889    0.374
    ##    .sumses33          1.999    0.228    8.785    0.000
    ##    .SESaQ34_1         0.326    0.022   14.933    0.000
    ##    .bdiq2             0.310    0.022   14.010    0.000
    ##    .bdiq3             0.351    0.025   13.943    0.000
    ##    .bdiq4             0.329    0.024   13.898    0.000
    ##    .bdiq5             0.240    0.017   14.037    0.000
    ##    .bdiq6             0.595    0.042   14.195    0.000
    ##    .bdiq7             0.294    0.021   14.057    0.000
    ##    .bdiq8             0.477    0.035   13.776    0.000
    ##    .bdiq10            0.917    0.063   14.588    0.000
    ##    .bdiq12            0.378    0.027   14.069    0.000
    ##    .bdiq13            0.466    0.033   14.171    0.000
    ##    .bdiq14            0.294    0.021   13.892    0.000
    ##    .bdiq15            0.429    0.029   14.723    0.000
    ##    .bdiq17            0.459    0.031   14.727    0.000
    ##    .bdiq20            0.505    0.034   14.973    0.000
    ##    .bdiq21            0.806    0.053   15.076    0.000
    ##    .bdiq22_1          0.063    0.007    9.519    0.000
    ##    .bdiq23_1          0.064    0.005   12.368    0.000
    ##    .bdiq24_1          0.043    0.004    9.859    0.000
    ##    .bdiq25_1          0.075    0.005   14.170    0.000
    ##     pov               0.015    0.015    0.987    0.324
    ##    .depf1             0.173    0.026    6.715    0.000
    ##    .depf2             0.064    0.009    7.208    0.000

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

med_mod_fit_3 <- sem(med_mod, data = drak_col, missing = 'fiml.x')
```

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
    ## variances are negative

``` r
summary(med_mod_fit_3)
```

    ## lavaan 0.6-3 ended normally after 153 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        120
    ## 
    ##   Number of observations                           469
    ##   Number of missing patterns                        16
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    2210.915
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
    ##     SESaQ31_1         9.394    6.486    1.448    0.148
    ##     sumses33          4.974    1.633    3.046    0.002
    ##     SESaQ34_1         0.930    0.346    2.692    0.007
    ##   trauf2 =~                                           
    ##     ipvq1             1.000                           
    ##     ipvq2             0.885    0.057   15.567    0.000
    ##     ipvq3             0.925    0.056   16.436    0.000
    ##     ipvq4             0.913    0.056   16.405    0.000
    ##     ipvq5            -0.380    0.031  -12.398    0.000
    ##     ipvq6             0.944    0.056   16.735    0.000
    ##     ipvq7             0.962    0.058   16.487    0.000
    ##     ipvq8             0.761    0.048   15.925    0.000
    ##     ipvq9             0.783    0.048   16.180    0.000
    ##     ipvq10            0.360    0.030   11.971    0.000
    ##     ipvq11           -0.348    0.029  -12.139    0.000
    ##     ipvq12            0.428    0.035   12.133    0.000
    ##     ipvq13            0.565    0.042   13.364    0.000
    ##     ipvq14            0.253    0.025   10.255    0.000
    ##     ipvq15           -0.260    0.024  -10.782    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.086    0.098   11.060    0.000
    ##     bdiq4             1.074    0.097   11.033    0.000
    ##     bdiq5             0.877    0.082   10.655    0.000
    ##     bdiq6             1.301    0.125   10.375    0.000
    ##     bdiq7             0.954    0.090   10.648    0.000
    ##     bdiq8             1.354    0.122   11.140    0.000
    ##     bdiq10            1.264    0.143    8.846    0.000
    ##     bdiq12            1.072    0.104   10.355    0.000
    ##     bdiq13            1.142    0.111   10.285    0.000
    ##     bdiq14            1.007    0.092   10.915    0.000
    ##     bdiq15            0.769    0.093    8.263    0.000
    ##     bdiq17            0.795    0.097    8.240    0.000
    ##     bdiq20            0.626    0.094    6.649    0.000
    ##     bdiq21            0.696    0.115    6.064    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.724    0.079    9.184    0.000
    ##     bdiq24_1          0.816    0.074   11.084    0.000
    ##     bdiq25_1          0.494    0.068    7.242    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf2            0.249    0.032    7.739    0.000
    ##     pov               0.143    0.119    1.206    0.228
    ##   depf2 ~                                             
    ##     trauf2            0.124    0.019    6.504    0.000
    ##     pov              -0.047    0.077   -0.615    0.539
    ##   trauf2 ~                                            
    ##     pov              -0.083    0.223   -0.373    0.709
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.030    0.006    4.935    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           2.470    0.043   57.635    0.000
    ##    .SESaQ31_1         3.014    0.045   67.583    0.000
    ##    .sumses33          5.331    0.072   74.347    0.000
    ##    .SESaQ34_1         1.568    0.027   58.148    0.000
    ##    .ipvq1             1.887    0.048   39.539    0.000
    ##    .ipvq2             1.640    0.045   36.740    0.000
    ##    .ipvq3             1.608    0.044   36.769    0.000
    ##    .ipvq4             1.527    0.043   35.321    0.000
    ##    .ipvq5             1.583    0.023   67.417    0.000
    ##    .ipvq6             1.755    0.043   40.409    0.000
    ##    .ipvq7             1.676    0.045   37.355    0.000
    ##    .ipvq8             1.350    0.036   37.002    0.000
    ##    .ipvq9             1.371    0.037   37.158    0.000
    ##    .ipvq10            1.139    0.023   50.460    0.000
    ##    .ipvq11            1.695    0.022   76.569    0.000
    ##    .ipvq12            1.173    0.027   44.176    0.000
    ##    .ipvq13            1.271    0.032   39.768    0.000
    ##    .ipvq14            1.081    0.018   58.574    0.000
    ##    .ipvq15            1.831    0.018   99.944    0.000
    ##    .bdiq2             0.348    0.032   10.834    0.000
    ##    .bdiq3             0.380    0.034   11.030    0.000
    ##    .bdiq4             0.546    0.034   16.182    0.000
    ##    .bdiq5             0.426    0.028   15.045    0.000
    ##    .bdiq6             0.516    0.044   11.849    0.000
    ##    .bdiq7             0.322    0.031   10.313    0.000
    ##    .bdiq8             0.465    0.041   11.232    0.000
    ##    .bdiq10            0.681    0.051   13.434    0.000
    ##    .bdiq12            0.520    0.035   14.766    0.000
    ##    .bdiq13            0.563    0.039   14.590    0.000
    ##    .bdiq14            0.326    0.032   10.244    0.000
    ##    .bdiq15            0.719    0.034   21.278    0.000
    ##    .bdiq17            0.823    0.035   23.565    0.000
    ##    .bdiq20            0.876    0.035   24.974    0.000
    ##    .bdiq21            0.917    0.044   21.038    0.000
    ##    .bdiq22_1          0.150    0.016    9.078    0.000
    ##    .bdiq23_1          0.107    0.014    7.486    0.000
    ##    .bdiq24_1          0.094    0.013    6.974    0.000
    ##    .bdiq25_1          0.100    0.014    7.231    0.000
    ##     pov               0.000                           
    ##    .trauf2            0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .sumses9           0.841    0.056   14.921    0.000
    ##    .SESaQ31_1        -0.573    0.675   -0.849    0.396
    ##    .sumses33          1.974    0.229    8.637    0.000
    ##    .SESaQ34_1         0.325    0.022   14.958    0.000
    ##    .ipvq1             0.480    0.035   13.768    0.000
    ##    .ipvq2             0.473    0.034   14.080    0.000
    ##    .ipvq3             0.393    0.029   13.683    0.000
    ##    .ipvq4             0.386    0.028   13.782    0.000
    ##    .ipvq5             0.161    0.011   14.117    0.000
    ##    .ipvq6             0.360    0.027   13.392    0.000
    ##    .ipvq7             0.400    0.029   13.579    0.000
    ##    .ipvq8             0.284    0.021   13.789    0.000
    ##    .ipvq9             0.278    0.020   13.643    0.000
    ##    .ipvq10            0.163    0.011   14.740    0.000
    ##    .ipvq11            0.143    0.010   14.048    0.000
    ##    .ipvq12            0.223    0.015   14.684    0.000
    ##    .ipvq13            0.291    0.020   14.472    0.000
    ##    .ipvq14            0.122    0.008   14.937    0.000
    ##    .ipvq15            0.101    0.007   13.742    0.000
    ##    .bdiq2             0.306    0.022   13.995    0.000
    ##    .bdiq3             0.347    0.025   13.935    0.000
    ##    .bdiq4             0.329    0.024   13.934    0.000
    ##    .bdiq5             0.241    0.017   14.072    0.000
    ##    .bdiq6             0.590    0.042   14.192    0.000
    ##    .bdiq7             0.296    0.021   14.111    0.000
    ##    .bdiq8             0.479    0.035   13.826    0.000
    ##    .bdiq10            0.921    0.063   14.618    0.000
    ##    .bdiq12            0.379    0.027   14.106    0.000
    ##    .bdiq13            0.467    0.033   14.205    0.000
    ##    .bdiq14            0.296    0.021   13.959    0.000
    ##    .bdiq15            0.430    0.029   14.750    0.000
    ##    .bdiq17            0.460    0.031   14.750    0.000
    ##    .bdiq20            0.508    0.034   14.996    0.000
    ##    .bdiq21            0.805    0.053   15.079    0.000
    ##    .bdiq22_1          0.065    0.007    9.921    0.000
    ##    .bdiq23_1          0.063    0.005   12.248    0.000
    ##    .bdiq24_1          0.044    0.004   10.035    0.000
    ##    .bdiq25_1          0.075    0.005   14.153    0.000
    ##     pov               0.017    0.017    1.022    0.307
    ##    .trauf2            0.588    0.064    9.169    0.000
    ##    .depf1             0.140    0.021    6.773    0.000
    ##    .depf2             0.053    0.008    6.856    0.000

Mbekweni Model 1
================

Model 1, applied to Colored mothers

No mediator
-----------

``` r
nomed_mod <- 'pov =~ SESaQ13_1 + SESaQ14 + SESaQ15_1 + SESaQ20_1 + SESaQ21 + SESaQ31_1 + SESaQ32 + SESaQ34_1 + sumses9 + sumses33
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21
depf2 =~ bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1

depf1 ~ pov
depf2 ~ pov '

nomed_mod_fit_1 <- sem(nomed_mod, data = drak_col, missing = 'fiml.x')
summary(nomed_mod_fit_1)
```

    ## lavaan 0.6-3 ended normally after 238 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         90
    ## 
    ##   Number of observations                           469
    ##   Number of missing patterns                        13
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    1054.020
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
    ##     SESaQ14          -7.965    8.823   -0.903    0.367
    ##     SESaQ15_1        -5.792    7.534   -0.769    0.442
    ##     SESaQ20_1        36.742   39.378    0.933    0.351
    ##     SESaQ21          33.735   36.126    0.934    0.350
    ##     SESaQ31_1       -14.912   16.164   -0.923    0.356
    ##     SESaQ32          -5.745    7.612   -0.755    0.450
    ##     SESaQ34_1         0.844    1.928    0.438    0.662
    ##     sumses9          -6.904    7.864   -0.878    0.380
    ##     sumses33        -20.634   22.539   -0.916    0.360
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.087    0.100   10.850    0.000
    ##     bdiq4             1.086    0.100   10.898    0.000
    ##     bdiq5             0.889    0.084   10.542    0.000
    ##     bdiq6             1.302    0.128   10.186    0.000
    ##     bdiq7             0.974    0.092   10.596    0.000
    ##     bdiq8             1.374    0.125   11.030    0.000
    ##     bdiq10            1.292    0.146    8.837    0.000
    ##     bdiq12            1.086    0.106   10.250    0.000
    ##     bdiq13            1.160    0.114   10.198    0.000
    ##     bdiq14            1.025    0.095   10.842    0.000
    ##     bdiq15            0.785    0.095    8.262    0.000
    ##     bdiq17            0.810    0.099    8.213    0.000
    ##     bdiq20            0.648    0.096    6.737    0.000
    ##     bdiq21            0.702    0.117    6.010    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.702    0.077    9.067    0.000
    ##     bdiq24_1          0.810    0.074   10.889    0.000
    ##     bdiq25_1          0.485    0.067    7.201    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     pov              -2.351    2.828   -0.831    0.406
    ##   depf2 ~                                             
    ##     pov               0.597    1.081    0.552    0.581
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.048    0.008    6.281    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         1.107    0.017   65.439    0.000
    ##    .SESaQ14           4.368    0.035  124.680    0.000
    ##    .SESaQ15_1         2.706    0.065   41.564    0.000
    ##    .SESaQ20_1         1.650    0.037   44.938    0.000
    ##    .SESaQ21           0.481    0.032   14.882    0.000
    ##    .SESaQ31_1         3.011    0.045   67.446    0.000
    ##    .SESaQ32           2.874    0.071   40.719    0.000
    ##    .SESaQ34_1         1.567    0.027   58.120    0.000
    ##    .sumses9           2.471    0.043   57.662    0.000
    ##    .sumses33          5.339    0.072   74.532    0.000
    ##    .bdiq2             0.348    0.032   10.834    0.000
    ##    .bdiq3             0.380    0.034   11.030    0.000
    ##    .bdiq4             0.546    0.034   16.182    0.000
    ##    .bdiq5             0.426    0.028   15.045    0.000
    ##    .bdiq6             0.516    0.044   11.849    0.000
    ##    .bdiq7             0.322    0.031   10.313    0.000
    ##    .bdiq8             0.465    0.041   11.232    0.000
    ##    .bdiq10            0.681    0.051   13.435    0.000
    ##    .bdiq12            0.520    0.035   14.766    0.000
    ##    .bdiq13            0.563    0.039   14.590    0.000
    ##    .bdiq14            0.326    0.032   10.244    0.000
    ##    .bdiq15            0.719    0.034   21.278    0.000
    ##    .bdiq17            0.823    0.035   23.565    0.000
    ##    .bdiq20            0.876    0.035   24.974    0.000
    ##    .bdiq21            0.917    0.044   21.038    0.000
    ##    .bdiq22_1          0.150    0.016    9.076    0.000
    ##    .bdiq23_1          0.107    0.014    7.485    0.000
    ##    .bdiq24_1          0.094    0.013    6.972    0.000
    ##    .bdiq25_1          0.100    0.014    7.230    0.000
    ##     pov               0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         0.134    0.009   15.289    0.000
    ##    .SESaQ14           0.555    0.037   15.165    0.000
    ##    .SESaQ15_1         1.966    0.129   15.246    0.000
    ##    .SESaQ20_1         0.222    0.035    6.363    0.000
    ##    .SESaQ21           0.138    0.029    4.790    0.000
    ##    .SESaQ31_1         0.861    0.058   14.920    0.000
    ##    .SESaQ32           2.311    0.152   15.248    0.000
    ##    .SESaQ34_1         0.339    0.022   15.278    0.000
    ##    .sumses9           0.843    0.055   15.215    0.000
    ##    .sumses33          2.258    0.150   15.004    0.000
    ##    .bdiq2             0.310    0.022   14.015    0.000
    ##    .bdiq3             0.351    0.025   13.949    0.000
    ##    .bdiq4             0.330    0.024   13.907    0.000
    ##    .bdiq5             0.240    0.017   14.037    0.000
    ##    .bdiq6             0.597    0.042   14.204    0.000
    ##    .bdiq7             0.293    0.021   14.050    0.000
    ##    .bdiq8             0.477    0.035   13.776    0.000
    ##    .bdiq10            0.916    0.063   14.584    0.000
    ##    .bdiq12            0.378    0.027   14.076    0.000
    ##    .bdiq13            0.466    0.033   14.167    0.000
    ##    .bdiq14            0.294    0.021   13.895    0.000
    ##    .bdiq15            0.428    0.029   14.723    0.000
    ##    .bdiq17            0.459    0.031   14.726    0.000
    ##    .bdiq20            0.505    0.034   14.972    0.000
    ##    .bdiq21            0.806    0.053   15.075    0.000
    ##    .bdiq22_1          0.063    0.007    9.598    0.000
    ##    .bdiq23_1          0.064    0.005   12.391    0.000
    ##    .bdiq24_1          0.043    0.004    9.829    0.000
    ##    .bdiq25_1          0.075    0.005   14.151    0.000
    ##     pov               0.000    0.001    0.467    0.640
    ##    .depf1             0.171    0.026    6.695    0.000
    ##    .depf2             0.064    0.009    7.194    0.000

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

med_mod_fit_1 <- sem(med_mod, data = drak_col, missing = 'fiml.x')
```

    ## Warning in lavaan::lavaan(model = med_mod, data = drak_col, missing = "fiml.x", : lavaan WARNING: not all elements of the gradient are (near) zero;
    ##                   the optimizer may not have found a local solution;
    ##                   use lavInspect(fit, "optim.gradient") to investigate

``` r
summary(med_mod_fit_1)
```

    ## lavaan 0.6-3 ended normally after 285 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        192
    ## 
    ##   Number of observations                           469
    ##   Number of missing patterns                        31
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    4240.446
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
    ##     SESaQ14          -8.437    9.361   -0.901    0.367
    ##     SESaQ15_1        -5.509    7.316   -0.753    0.451
    ##     SESaQ20_1        36.781   39.644    0.928    0.354
    ##     SESaQ21          34.029   36.659    0.928    0.353
    ##     SESaQ31_1       -15.129   16.486   -0.918    0.359
    ##     SESaQ32          -5.591    7.486   -0.747    0.455
    ##     SESaQ34_1         1.076    2.067    0.521    0.603
    ##     sumses9          -6.973    7.991   -0.873    0.383
    ##     sumses33        -21.505   23.594   -0.911    0.362
    ##   trauf1 =~                                           
    ##     MPSSQ2            1.000                           
    ##     MPSSQ3            1.212    0.203    5.972    0.000
    ##     MPSSQ4            1.326    0.224    5.930    0.000
    ##     MPSSQ5            1.485    0.232    6.400    0.000
    ##     MPSSQ6            1.549    0.242    6.412    0.000
    ##     MPSSQ7            0.823    0.153    5.388    0.000
    ##     MPSSQ8            1.361    0.208    6.540    0.000
    ##     MPSSQ9            1.644    0.240    6.849    0.000
    ##     MPSSQ10           1.382    0.217    6.375    0.000
    ##     MPSSQ11           1.481    0.229    6.461    0.000
    ##     MPSSQ12           1.577    0.235    6.706    0.000
    ##     MPSSQ13           1.621    0.239    6.777    0.000
    ##     MPSSQ14           1.547    0.222    6.974    0.000
    ##     MPSSQ15           1.399    0.219    6.394    0.000
    ##     MPSSQ16           1.442    0.215    6.708    0.000
    ##     MPSSQ17           1.367    0.210    6.518    0.000
    ##     MPSSQ18           1.273    0.239    5.321    0.000
    ##   trauf2 =~                                           
    ##     ipvq1             1.000                           
    ##     ipvq2             0.885    0.057   15.605    0.000
    ##     ipvq3             0.923    0.056   16.453    0.000
    ##     ipvq4             0.911    0.056   16.419    0.000
    ##     ipvq5            -0.378    0.031  -12.385    0.000
    ##     ipvq6             0.943    0.056   16.761    0.000
    ##     ipvq7             0.959    0.058   16.492    0.000
    ##     ipvq8             0.760    0.048   15.960    0.000
    ##     ipvq9             0.782    0.048   16.203    0.000
    ##     ipvq10            0.360    0.030   11.995    0.000
    ##     ipvq11           -0.347    0.029  -12.128    0.000
    ##     ipvq12            0.427    0.035   12.132    0.000
    ##     ipvq13            0.564    0.042   13.366    0.000
    ##     ipvq14            0.252    0.025   10.254    0.000
    ##     ipvq15           -0.260    0.024  -10.784    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.084    0.098   11.098    0.000
    ##     bdiq4             1.064    0.097   11.008    0.000
    ##     bdiq5             0.876    0.082   10.694    0.000
    ##     bdiq6             1.301    0.125   10.429    0.000
    ##     bdiq7             0.956    0.089   10.713    0.000
    ##     bdiq8             1.350    0.121   11.172    0.000
    ##     bdiq10            1.264    0.142    8.887    0.000
    ##     bdiq12            1.064    0.103   10.353    0.000
    ##     bdiq13            1.138    0.110   10.301    0.000
    ##     bdiq14            0.999    0.092   10.905    0.000
    ##     bdiq15            0.767    0.093    8.282    0.000
    ##     bdiq17            0.796    0.096    8.284    0.000
    ##     bdiq20            0.630    0.094    6.708    0.000
    ##     bdiq21            0.699    0.114    6.112    0.000
    ##   depf2 =~                                            
    ##     bdiq22_1          1.000                           
    ##     bdiq23_1          0.724    0.079    9.200    0.000
    ##     bdiq24_1          0.820    0.074   11.087    0.000
    ##     bdiq25_1          0.497    0.069    7.258    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf1            0.303    0.077    3.965    0.000
    ##     trauf2            0.228    0.032    7.155    0.000
    ##     pov              -4.170    4.691   -0.889    0.374
    ##   depf2 ~                                             
    ##     trauf1            0.087    0.042    2.084    0.037
    ##     trauf2            0.115    0.020    5.845    0.000
    ##     pov              -0.252    0.895   -0.281    0.779
    ##   trauf1 ~                                            
    ##     pov               1.086    2.997    0.362    0.717
    ##   trauf2 ~                                            
    ##     pov               6.724    7.607    0.884    0.377
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .depf1 ~~                                            
    ##    .depf2             0.024    0.006    4.009    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         1.107    0.017   65.439    0.000
    ##    .SESaQ14           4.368    0.035  124.684    0.000
    ##    .SESaQ15_1         2.706    0.065   41.564    0.000
    ##    .SESaQ20_1         1.650    0.037   44.938    0.000
    ##    .SESaQ21           0.481    0.032   14.879    0.000
    ##    .SESaQ31_1         3.011    0.045   67.448    0.000
    ##    .SESaQ32           2.874    0.071   40.720    0.000
    ##    .SESaQ34_1         1.567    0.027   58.120    0.000
    ##    .sumses9           2.470    0.043   57.659    0.000
    ##    .sumses33          5.339    0.072   74.532    0.000
    ##    .MPSSQ2            0.644    0.073    8.855    0.000
    ##    .MPSSQ3            0.837    0.079   10.558    0.000
    ##    .MPSSQ4            1.291    0.088   14.697    0.000
    ##    .MPSSQ5            1.180    0.086   13.799    0.000
    ##    .MPSSQ6            1.038    0.089   11.658    0.000
    ##    .MPSSQ7            0.451    0.062    7.233    0.000
    ##    .MPSSQ8            0.694    0.075    9.213    0.000
    ##    .MPSSQ9            0.851    0.083   10.275    0.000
    ##    .MPSSQ10           0.777    0.080    9.665    0.000
    ##    .MPSSQ11           0.859    0.084   10.229    0.000
    ##    .MPSSQ12           0.967    0.084   11.500    0.000
    ##    .MPSSQ13           1.005    0.084   11.968    0.000
    ##    .MPSSQ14           0.775    0.076   10.225    0.000
    ##    .MPSSQ15           0.784    0.081    9.642    0.000
    ##    .MPSSQ16           0.764    0.076   10.068    0.000
    ##    .MPSSQ17           0.713    0.077    9.313    0.000
    ##    .MPSSQ18           1.910    0.098   19.448    0.000
    ##    .ipvq1             1.887    0.048   39.540    0.000
    ##    .ipvq2             1.640    0.045   36.741    0.000
    ##    .ipvq3             1.608    0.044   36.771    0.000
    ##    .ipvq4             1.527    0.043   35.322    0.000
    ##    .ipvq5             1.583    0.023   67.416    0.000
    ##    .ipvq6             1.755    0.043   40.410    0.000
    ##    .ipvq7             1.676    0.045   37.356    0.000
    ##    .ipvq8             1.350    0.036   37.003    0.000
    ##    .ipvq9             1.371    0.037   37.159    0.000
    ##    .ipvq10            1.139    0.023   50.461    0.000
    ##    .ipvq11            1.695    0.022   76.565    0.000
    ##    .ipvq12            1.173    0.027   44.176    0.000
    ##    .ipvq13            1.271    0.032   39.769    0.000
    ##    .ipvq14            1.081    0.018   58.575    0.000
    ##    .ipvq15            1.831    0.018   99.937    0.000
    ##    .bdiq2             0.348    0.032   10.932    0.000
    ##    .bdiq3             0.380    0.034   11.132    0.000
    ##    .bdiq4             0.546    0.033   16.332    0.000
    ##    .bdiq5             0.426    0.028   15.179    0.000
    ##    .bdiq6             0.516    0.043   11.947    0.000
    ##    .bdiq7             0.322    0.031   10.403    0.000
    ##    .bdiq8             0.465    0.041   11.344    0.000
    ##    .bdiq10            0.681    0.050   13.510    0.000
    ##    .bdiq12            0.520    0.035   14.891    0.000
    ##    .bdiq13            0.563    0.038   14.708    0.000
    ##    .bdiq14            0.326    0.032   10.338    0.000
    ##    .bdiq15            0.719    0.034   21.379    0.000
    ##    .bdiq17            0.823    0.035   23.678    0.000
    ##    .bdiq20            0.876    0.035   25.049    0.000
    ##    .bdiq21            0.917    0.043   21.088    0.000
    ##    .bdiq22_1          0.150    0.016    9.122    0.000
    ##    .bdiq23_1          0.107    0.014    7.511    0.000
    ##    .bdiq24_1          0.094    0.013    7.007    0.000
    ##    .bdiq25_1          0.100    0.014    7.242    0.000
    ##     pov               0.000                           
    ##    .trauf1            0.000                           
    ##    .trauf2            0.000                           
    ##    .depf1             0.000                           
    ##    .depf2             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .SESaQ13_1         0.134    0.009   15.289    0.000
    ##    .SESaQ14           0.553    0.037   15.148    0.000
    ##    .SESaQ15_1         1.967    0.129   15.248    0.000
    ##    .SESaQ20_1         0.227    0.032    7.071    0.000
    ##    .SESaQ21           0.137    0.027    5.134    0.000
    ##    .SESaQ31_1         0.860    0.058   14.909    0.000
    ##    .SESaQ32           2.311    0.152   15.249    0.000
    ##    .SESaQ34_1         0.339    0.022   15.277    0.000
    ##    .sumses9           0.843    0.055   15.214    0.000
    ##    .sumses33          2.249    0.150   14.977    0.000
    ##    .MPSSQ2            0.588    0.069    8.527    0.000
    ##    .MPSSQ3            0.642    0.076    8.481    0.000
    ##    .MPSSQ4            0.798    0.094    8.490    0.000
    ##    .MPSSQ5            0.633    0.076    8.310    0.000
    ##    .MPSSQ6            0.684    0.082    8.312    0.000
    ##    .MPSSQ7            0.451    0.052    8.608    0.000
    ##    .MPSSQ8            0.460    0.056    8.233    0.000
    ##    .MPSSQ9            0.448    0.056    7.945    0.000
    ##    .MPSSQ10           0.569    0.068    8.328    0.000
    ##    .MPSSQ11           0.597    0.072    8.298    0.000
    ##    .MPSSQ12           0.531    0.065    8.146    0.000
    ##    .MPSSQ13           0.495    0.061    8.060    0.000
    ##    .MPSSQ14           0.346    0.044    7.821    0.000
    ##    .MPSSQ15           0.580    0.069    8.343    0.000
    ##    .MPSSQ16           0.419    0.052    8.118    0.000
    ##    .MPSSQ17           0.486    0.059    8.264    0.000
    ##    .MPSSQ18           0.980    0.121    8.083    0.000
    ##    .ipvq1             0.478    0.035   13.753    0.000
    ##    .ipvq2             0.472    0.034   14.071    0.000
    ##    .ipvq3             0.393    0.029   13.683    0.000
    ##    .ipvq4             0.386    0.028   13.784    0.000
    ##    .ipvq5             0.161    0.011   14.120    0.000
    ##    .ipvq6             0.360    0.027   13.387    0.000
    ##    .ipvq7             0.401    0.030   13.584    0.000
    ##    .ipvq8             0.283    0.021   13.781    0.000
    ##    .ipvq9             0.277    0.020   13.637    0.000
    ##    .ipvq10            0.162    0.011   14.737    0.000
    ##    .ipvq11            0.144    0.010   14.050    0.000
    ##    .ipvq12            0.223    0.015   14.685    0.000
    ##    .ipvq13            0.291    0.020   14.474    0.000
    ##    .ipvq14            0.122    0.008   14.937    0.000
    ##    .ipvq15            0.101    0.007   13.742    0.000
    ##    .bdiq2             0.305    0.022   13.998    0.000
    ##    .bdiq3             0.346    0.025   13.947    0.000
    ##    .bdiq4             0.332    0.024   13.978    0.000
    ##    .bdiq5             0.240    0.017   14.084    0.000
    ##    .bdiq6             0.588    0.041   14.195    0.000
    ##    .bdiq7             0.295    0.021   14.116    0.000
    ##    .bdiq8             0.479    0.035   13.849    0.000
    ##    .bdiq10            0.919    0.063   14.622    0.000
    ##    .bdiq12            0.381    0.027   14.140    0.000
    ##    .bdiq13            0.468    0.033   14.221    0.000
    ##    .bdiq14            0.298    0.021   13.991    0.000
    ##    .bdiq15            0.430    0.029   14.758    0.000
    ##    .bdiq17            0.459    0.031   14.752    0.000
    ##    .bdiq20            0.507    0.034   14.994    0.000
    ##    .bdiq21            0.804    0.053   15.078    0.000
    ##    .bdiq22_1          0.065    0.007    9.999    0.000
    ##    .bdiq23_1          0.063    0.005   12.268    0.000
    ##    .bdiq24_1          0.043    0.004    9.968    0.000
    ##    .bdiq25_1          0.075    0.005   14.128    0.000
    ##     pov               0.000    0.001    0.464    0.642
    ##    .trauf1            0.246    0.069    3.572    0.000
    ##    .trauf2            0.576    0.063    9.169    0.000
    ##    .depf1             0.115    0.019    6.212    0.000
    ##    .depf2             0.051    0.008    6.717    0.000
