drak\_ct
================
Tatini Mal-Sarkar
12/24/2018

Loading the data
================

``` r
drak = read_csv("/Users/tatinimal-sarkar/Documents/mph2/practicum/thesis/data/KerryDataRequestV2_visit1_c.csv") %>% 
  select(-ASSIST_Tobacco_Score:-opioids_preg_any,
         -AQ1a:-AQ14b,
         -pdiq1_afr:-pdiq13_xho,
         -leq1_1_afr:-leq51_2_xho,
         -ipvq1_afr:-ipvq17_xho,
         -epdsq1_afr:-epdsq10_xho,
         -bdiq1_afr:-bdiq25_2_xho,
         -srqq1_afr:-srqq20_xho) %>% 
  mutate(ethnicity = as.numeric(ethnicity > 1),
         clinic = as.numeric(Clinic > 1),
         bdi = bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1,
         revsum9 = 5 - sumses9) %>% 
  filter(SESaQ31_1 != 6,
         SESaQ34_1 != 4) %>% 
  mutate(housing = as.numeric(SESaQ31_1 > 2),
         owner = as.numeric(SESaQ34_1 == 1),
         ses = revsum9 + housing + owner + sumses33) %>% 
  select(-Clinic) %>% 
  mutate(kids = 5 - SESaQ1,
         people = 20 - SESaQ2,
         adults = 3 - SESaQ3_1,
         school = SESaQ14 - 2,
         job = 6 - SESaQ15_1,
         aid = 1 - SESaQ16,
         ownincome = SESaQ18,
         houseincome = SESaQ19,
         ipvq1 = ipvq1 - 1,
         ipvq2 = ipvq2 - 1,
         ipvq3 = ipvq3 - 1,
         ipvq4 = ipvq4 - 1,
         ipvq5 = ifelse(ipvq5 == 2, 0, ipvq5),
         ipvq6 = ipvq6 - 1,
         ipvq7 = ipvq7 - 1,
         ipvq8 = ipvq8 - 1,
         ipvq9 = ipvq9 - 1,
         ipvq10 = ipvq10 - 1,
         ipvq11 = ifelse(ipvq11 == 2, 0, ipvq11),
         ipvq12 = ipvq12 - 1,
         ipvq13 = ipvq13 - 1,
         ipvq14 = ipvq14 - 1,
         ipvq15 = ifelse(ipvq15 == 2, 0, ipvq15)
         ) %>%
  filter(
         job != -3,
         ownincome != 99,
         ownincome != 9999,
         houseincome != 99,
         houseincome != 9999) %>% 
  mutate(ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome,
         bdi = bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1)
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

Socioeconomic status and depression
===================================

``` r
model <- 'ses =~ housing + owner + sumses33 + school + job + aid + ownincome + houseincome 
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1

depf1 ~ ses '

fit <- sem(model, data = drak, missing = 'fiml.x', group = "clinic")
```

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
    ## variances are negative

``` r
summary(fit)
```

    ## lavaan 0.6-3 ended normally after 134 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        164
    ## 
    ##   Number of observations per group         
    ##   0                                                542
    ##   1                                                424
    ##   Number of missing patterns per group     
    ##   0                                                 12
    ##   1                                                  5
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    2591.920
    ##   Degrees of freedom                               646
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Chi-square for each group:
    ## 
    ##   0                                           1441.797
    ##   1                                           1150.122
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Observed
    ##   Observed information based on                Hessian
    ##   Standard Errors                             Standard
    ## 
    ## 
    ## Group 1 [0]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   ses =~                                              
    ##     housing           1.000                           
    ##     owner             0.277    0.054    5.093    0.000
    ##     sumses33          4.270    0.563    7.582    0.000
    ##     school            0.559    0.101    5.525    0.000
    ##     job              -0.066    0.235   -0.283    0.777
    ##     aid               0.098    0.067    1.457    0.145
    ##     ownincome        -0.005    0.055   -0.095    0.924
    ##     houseincome       0.061    0.099    0.621    0.534
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.155    0.091   12.628    0.000
    ##     bdiq4             1.493    0.109   13.636    0.000
    ##     bdiq5             0.938    0.082   11.490    0.000
    ##     bdiq6             1.347    0.100   13.485    0.000
    ##     bdiq7             1.229    0.094   13.032    0.000
    ##     bdiq8             1.582    0.121   13.109    0.000
    ##     bdiq10            1.220    0.105   11.595    0.000
    ##     bdiq12            1.240    0.094   13.187    0.000
    ##     bdiq13            1.291    0.093   13.847    0.000
    ##     bdiq14            1.232    0.088   13.989    0.000
    ##     bdiq15            1.287    0.094   13.663    0.000
    ##     bdiq17            1.020    0.088   11.548    0.000
    ##     bdiq20            1.305    0.100   12.991    0.000
    ##     bdiq21            1.259    0.124   10.191    0.000
    ##     bdiq22_1          0.057    0.020    2.835    0.005
    ##     bdiq23_1          0.041    0.013    3.187    0.001
    ##     bdiq24_1          0.018    0.008    2.324    0.020
    ##     bdiq25_1          0.014    0.006    2.179    0.029
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     ses              -0.195    0.061   -3.192    0.001
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.561    0.021   26.312    0.000
    ##    .owner             0.247    0.019   13.342    0.000
    ##    .sumses33          4.319    0.078   55.588    0.000
    ##    .school            2.345    0.033   70.128    0.000
    ##    .job               2.293    0.075   30.616    0.000
    ##    .aid               0.511    0.021   23.802    0.000
    ##    .ownincome         1.212    0.018   66.846    0.000
    ##    .houseincome       1.662    0.029   57.538    0.000
    ##    .bdiq2             0.359    0.030   12.021    0.000
    ##    .bdiq3             0.428    0.033   12.947    0.000
    ##    .bdiq4             0.618    0.038   16.423    0.000
    ##    .bdiq5             0.374    0.030   12.661    0.000
    ##    .bdiq6             0.405    0.035   11.733    0.000
    ##    .bdiq7             0.404    0.033   12.222    0.000
    ##    .bdiq8             0.577    0.042   13.674    0.000
    ##    .bdiq10            0.480    0.038   12.571    0.000
    ##    .bdiq12            0.482    0.033   14.663    0.000
    ##    .bdiq13            0.420    0.032   13.152    0.000
    ##    .bdiq14            0.335    0.030   11.084    0.000
    ##    .bdiq15            0.464    0.032   14.374    0.000
    ##    .bdiq17            0.444    0.032   13.844    0.000
    ##    .bdiq20            0.524    0.035   14.979    0.000
    ##    .bdiq21            0.638    0.046   13.928    0.000
    ##    .bdiq22_1          0.037    0.008    4.560    0.000
    ##    .bdiq23_1          0.015    0.005    2.852    0.004
    ##    .bdiq24_1          0.006    0.003    1.739    0.082
    ##    .bdiq25_1          0.004    0.003    1.418    0.156
    ##     ses               0.000                           
    ##    .depf1             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.107    0.019    5.715    0.000
    ##    .owner             0.175    0.011   16.083    0.000
    ##    .sumses33          0.721    0.325    2.221    0.026
    ##    .school            0.562    0.035   16.187    0.000
    ##    .job               3.041    0.185   16.461    0.000
    ##    .aid               0.249    0.015   16.422    0.000
    ##    .ownincome         0.178    0.011   16.462    0.000
    ##    .houseincome       0.452    0.027   16.451    0.000
    ##    .bdiq2             0.304    0.019   15.601    0.000
    ##    .bdiq3             0.351    0.023   15.480    0.000
    ##    .bdiq4             0.368    0.025   14.945    0.000
    ##    .bdiq5             0.314    0.020   15.721    0.000
    ##    .bdiq6             0.321    0.021   14.968    0.000
    ##    .bdiq7             0.320    0.021   15.210    0.000
    ##    .bdiq8             0.513    0.034   15.225    0.000
    ##    .bdiq10            0.522    0.033   15.720    0.000
    ##    .bdiq12            0.309    0.020   15.179    0.000
    ##    .bdiq13            0.252    0.017   14.759    0.000
    ##    .bdiq14            0.222    0.015   14.715    0.000
    ##    .bdiq15            0.266    0.018   14.822    0.000
    ##    .bdiq17            0.368    0.023   15.683    0.000
    ##    .bdiq20            0.357    0.023   15.185    0.000
    ##    .bdiq21            0.845    0.053   15.921    0.000
    ##    .bdiq22_1          0.035    0.002   16.363    0.000
    ##    .bdiq23_1          0.014    0.001   16.357    0.000
    ##    .bdiq24_1          0.006    0.000   16.355    0.000
    ##    .bdiq25_1          0.004    0.000   16.372    0.000
    ##     ses               0.140    0.022    6.312    0.000
    ##    .depf1             0.173    0.023    7.543    0.000
    ## 
    ## 
    ## Group 2 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   ses =~                                              
    ##     housing           1.000                           
    ##     owner            -0.128    0.079   -1.614    0.107
    ##     sumses33          4.930    0.442   11.141    0.000
    ##     school            0.899    0.120    7.493    0.000
    ##     job               0.457    0.198    2.312    0.021
    ##     aid               0.339    0.082    4.125    0.000
    ##     ownincome         0.293    0.078    3.743    0.000
    ##     houseincome       0.598    0.128    4.673    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.050    0.100   10.489    0.000
    ##     bdiq4             1.048    0.098   10.649    0.000
    ##     bdiq5             0.893    0.085   10.454    0.000
    ##     bdiq6             1.269    0.129    9.837    0.000
    ##     bdiq7             0.942    0.092   10.204    0.000
    ##     bdiq8             1.364    0.126   10.854    0.000
    ##     bdiq10            1.236    0.142    8.675    0.000
    ##     bdiq12            1.052    0.104   10.085    0.000
    ##     bdiq13            1.077    0.110    9.833    0.000
    ##     bdiq14            0.987    0.095   10.422    0.000
    ##     bdiq15            0.726    0.093    7.796    0.000
    ##     bdiq17            0.756    0.097    7.757    0.000
    ##     bdiq20            0.578    0.093    6.191    0.000
    ##     bdiq21            0.636    0.116    5.486    0.000
    ##     bdiq22_1          0.294    0.044    6.648    0.000
    ##     bdiq23_1          0.275    0.039    6.956    0.000
    ##     bdiq24_1          0.207    0.037    5.578    0.000
    ##     bdiq25_1          0.224    0.038    5.842    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     ses              -0.078    0.071   -1.096    0.273
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.684    0.023   30.292    0.000
    ##    .owner             0.469    0.024   19.365    0.000
    ##    .sumses33          5.317    0.075   70.933    0.000
    ##    .school            2.366    0.038   62.646    0.000
    ##    .job               3.340    0.063   53.101    0.000
    ##    .aid               0.492    0.024   20.236    0.000
    ##    .ownincome         1.276    0.025   51.886    0.000
    ##    .houseincome       1.962    0.040   49.077    0.000
    ##    .bdiq2             0.368    0.035   10.600    0.000
    ##    .bdiq3             0.396    0.037   10.812    0.000
    ##    .bdiq4             0.540    0.035   15.256    0.000
    ##    .bdiq5             0.429    0.030   14.131    0.000
    ##    .bdiq6             0.528    0.047   11.290    0.000
    ##    .bdiq7             0.335    0.033   10.030    0.000
    ##    .bdiq8             0.474    0.044   10.718    0.000
    ##    .bdiq10            0.673    0.052   12.832    0.000
    ##    .bdiq12            0.514    0.037   13.958    0.000
    ##    .bdiq13            0.545    0.040   13.632    0.000
    ##    .bdiq14            0.337    0.034    9.897    0.000
    ##    .bdiq15            0.729    0.035   20.655    0.000
    ##    .bdiq17            0.825    0.037   22.542    0.000
    ##    .bdiq20            0.877    0.036   24.235    0.000
    ##    .bdiq21            0.939    0.046   20.514    0.000
    ##    .bdiq22_1          0.144    0.017    8.449    0.000
    ##    .bdiq23_1          0.109    0.015    7.191    0.000
    ##    .bdiq24_1          0.099    0.015    6.833    0.000
    ##    .bdiq25_1          0.106    0.015    7.101    0.000
    ##     ses               0.000                           
    ##    .depf1             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.116    0.010   11.434    0.000
    ##    .owner             0.247    0.017   14.550    0.000
    ##    .sumses33         -0.054    0.199   -0.273    0.785
    ##    .school            0.524    0.037   14.341    0.000
    ##    .job               1.656    0.114   14.561    0.000
    ##    .aid               0.238    0.017   14.348    0.000
    ##    .ownincome         0.248    0.017   14.524    0.000
    ##    .houseincome       0.642    0.044   14.476    0.000
    ##    .bdiq2             0.322    0.024   13.338    0.000
    ##    .bdiq3             0.361    0.027   13.350    0.000
    ##    .bdiq4             0.324    0.024   13.243    0.000
    ##    .bdiq5             0.240    0.018   13.296    0.000
    ##    .bdiq6             0.624    0.046   13.573    0.000
    ##    .bdiq7             0.305    0.023   13.449    0.000
    ##    .bdiq8             0.478    0.037   13.078    0.000
    ##    .bdiq10            0.876    0.063   13.879    0.000
    ##    .bdiq12            0.366    0.027   13.394    0.000
    ##    .bdiq13            0.458    0.034   13.581    0.000
    ##    .bdiq14            0.308    0.023   13.325    0.000
    ##    .bdiq15            0.428    0.030   14.061    0.000
    ##    .bdiq17            0.461    0.033   14.060    0.000
    ##    .bdiq20            0.493    0.034   14.290    0.000
    ##    .bdiq21            0.811    0.056   14.370    0.000
    ##    .bdiq22_1          0.107    0.008   14.212    0.000
    ##    .bdiq23_1          0.083    0.006   14.173    0.000
    ##    .bdiq24_1          0.081    0.006   14.321    0.000
    ##    .bdiq25_1          0.086    0.006   14.316    0.000
    ##     ses               0.100    0.014    7.128    0.000
    ##    .depf1             0.188    0.029    6.537    0.000

Socioeconomic status, depression, and 1-factor childhood trauma
===============================================================

``` r
model <- 'ses =~ housing + owner + sumses33 + school + job + aid + ownincome + houseincome
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1
trauf =~ ctq1 + ctq3 + ctq4 + ctq6 + ctq8 + ctq9 + ctq11 + ctq14 + ctq20 + ctq23 

depf1 ~ b*trauf + c*ses
trauf ~ a*ses

indirect := a*b
total := c + (a*b)
'

fit <- sem(model, drak, missing = 'fiml.x', group = "clinic")
summary(fit)
```

    ## lavaan 0.6-3 ended normally after 174 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        228
    ## 
    ##   Number of observations per group         
    ##   0                                                542
    ##   1                                                424
    ##   Number of missing patterns per group     
    ##   0                                                 17
    ##   1                                                  6
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    4063.596
    ##   Degrees of freedom                              1252
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Chi-square for each group:
    ## 
    ##   0                                           2198.173
    ##   1                                           1865.423
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Observed
    ##   Observed information based on                Hessian
    ##   Standard Errors                             Standard
    ## 
    ## 
    ## Group 1 [0]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   ses =~                                              
    ##     housing           1.000                           
    ##     owner             0.275    0.054    5.051    0.000
    ##     sumses33          4.292    0.550    7.805    0.000
    ##     school            0.564    0.101    5.598    0.000
    ##     job              -0.070    0.234   -0.301    0.764
    ##     aid               0.096    0.067    1.434    0.151
    ##     ownincome        -0.005    0.055   -0.089    0.929
    ##     houseincome       0.062    0.098    0.634    0.526
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.154    0.091   12.650    0.000
    ##     bdiq4             1.489    0.109   13.653    0.000
    ##     bdiq5             0.935    0.081   11.493    0.000
    ##     bdiq6             1.344    0.100   13.500    0.000
    ##     bdiq7             1.227    0.094   13.051    0.000
    ##     bdiq8             1.578    0.120   13.122    0.000
    ##     bdiq10            1.220    0.105   11.626    0.000
    ##     bdiq12            1.237    0.094   13.201    0.000
    ##     bdiq13            1.290    0.093   13.872    0.000
    ##     bdiq14            1.231    0.088   14.017    0.000
    ##     bdiq15            1.287    0.094   13.692    0.000
    ##     bdiq17            1.018    0.088   11.557    0.000
    ##     bdiq20            1.304    0.100   13.019    0.000
    ##     bdiq21            1.256    0.123   10.191    0.000
    ##     bdiq22_1          0.057    0.020    2.848    0.004
    ##     bdiq23_1          0.041    0.013    3.179    0.001
    ##     bdiq24_1          0.018    0.008    2.303    0.021
    ##     bdiq25_1          0.014    0.006    2.169    0.030
    ##   trauf =~                                            
    ##     ctq1              1.000                           
    ##     ctq3              0.979    0.216    4.524    0.000
    ##     ctq4              1.390    0.288    4.828    0.000
    ##     ctq6              0.819    0.171    4.786    0.000
    ##     ctq8              1.102    0.222    4.959    0.000
    ##     ctq9              1.565    0.306    5.122    0.000
    ##     ctq11             1.571    0.290    5.407    0.000
    ##     ctq14             1.729    0.330    5.233    0.000
    ##     ctq20             1.470    0.293    5.015    0.000
    ##     ctq23             1.347    0.266    5.074    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf      (b)    0.293    0.088    3.316    0.001
    ##     ses        (c)   -0.167    0.060   -2.792    0.005
    ##   trauf ~                                             
    ##     ses        (a)   -0.098    0.046   -2.122    0.034
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.561    0.021   26.312    0.000
    ##    .owner             0.247    0.019   13.342    0.000
    ##    .sumses33          4.319    0.078   55.592    0.000
    ##    .school            2.345    0.033   70.128    0.000
    ##    .job               2.293    0.075   30.616    0.000
    ##    .aid               0.511    0.021   23.802    0.000
    ##    .ownincome         1.212    0.018   66.846    0.000
    ##    .houseincome       1.662    0.029   57.538    0.000
    ##    .bdiq2             0.359    0.030   12.019    0.000
    ##    .bdiq3             0.428    0.033   12.944    0.000
    ##    .bdiq4             0.618    0.038   16.420    0.000
    ##    .bdiq5             0.374    0.030   12.658    0.000
    ##    .bdiq6             0.405    0.035   11.730    0.000
    ##    .bdiq7             0.403    0.033   12.219    0.000
    ##    .bdiq8             0.577    0.042   13.671    0.000
    ##    .bdiq10            0.480    0.038   12.569    0.000
    ##    .bdiq12            0.482    0.033   14.661    0.000
    ##    .bdiq13            0.420    0.032   13.149    0.000
    ##    .bdiq14            0.335    0.030   11.082    0.000
    ##    .bdiq15            0.464    0.032   14.372    0.000
    ##    .bdiq17            0.444    0.032   13.842    0.000
    ##    .bdiq20            0.524    0.035   14.976    0.000
    ##    .bdiq21            0.638    0.046   13.926    0.000
    ##    .bdiq22_1          0.037    0.008    4.559    0.000
    ##    .bdiq23_1          0.015    0.005    2.851    0.004
    ##    .bdiq24_1          0.006    0.003    1.738    0.082
    ##    .bdiq25_1          0.004    0.003    1.418    0.156
    ##    .ctq1              1.590    0.043   36.975    0.000
    ##    .ctq3              1.395    0.038   36.692    0.000
    ##    .ctq4              1.408    0.043   32.772    0.000
    ##    .ctq6              1.160    0.028   41.885    0.000
    ##    .ctq8              1.201    0.031   39.048    0.000
    ##    .ctq9              1.263    0.039   32.142    0.000
    ##    .ctq11             1.240    0.035   35.421    0.000
    ##    .ctq14             1.351    0.041   33.248    0.000
    ##    .ctq20             1.205    0.035   34.389    0.000
    ##    .ctq23             1.188    0.032   37.388    0.000
    ##     ses               0.000                           
    ##    .depf1             0.000                           
    ##    .trauf             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.107    0.018    5.941    0.000
    ##    .owner             0.176    0.011   16.106    0.000
    ##    .sumses33          0.708    0.316    2.240    0.025
    ##    .school            0.562    0.035   16.180    0.000
    ##    .job               3.041    0.185   16.461    0.000
    ##    .aid               0.249    0.015   16.425    0.000
    ##    .ownincome         0.178    0.011   16.462    0.000
    ##    .houseincome       0.452    0.027   16.452    0.000
    ##    .bdiq2             0.303    0.019   15.599    0.000
    ##    .bdiq3             0.351    0.023   15.481    0.000
    ##    .bdiq4             0.369    0.025   14.953    0.000
    ##    .bdiq5             0.315    0.020   15.725    0.000
    ##    .bdiq6             0.322    0.021   14.976    0.000
    ##    .bdiq7             0.320    0.021   15.213    0.000
    ##    .bdiq8             0.514    0.034   15.232    0.000
    ##    .bdiq10            0.521    0.033   15.718    0.000
    ##    .bdiq12            0.310    0.020   15.187    0.000
    ##    .bdiq13            0.251    0.017   14.761    0.000
    ##    .bdiq14            0.222    0.015   14.716    0.000
    ##    .bdiq15            0.266    0.018   14.821    0.000
    ##    .bdiq17            0.368    0.023   15.686    0.000
    ##    .bdiq20            0.356    0.023   15.185    0.000
    ##    .bdiq21            0.845    0.053   15.923    0.000
    ##    .bdiq22_1          0.035    0.002   16.363    0.000
    ##    .bdiq23_1          0.014    0.001   16.357    0.000
    ##    .bdiq24_1          0.006    0.000   16.356    0.000
    ##    .bdiq25_1          0.004    0.000   16.373    0.000
    ##    .ctq1              0.916    0.058   15.863    0.000
    ##    .ctq3              0.702    0.045   15.703    0.000
    ##    .ctq4              0.835    0.055   15.190    0.000
    ##    .ctq6              0.357    0.023   15.428    0.000
    ##    .ctq8              0.410    0.028   14.883    0.000
    ##    .ctq9              0.629    0.045   14.025    0.000
    ##    .ctq11             0.456    0.035   13.126    0.000
    ##    .ctq14             0.642    0.047   13.718    0.000
    ##    .ctq20             0.482    0.038   12.578    0.000
    ##    .ctq23             0.393    0.032   12.456    0.000
    ##     ses               0.139    0.022    6.431    0.000
    ##    .depf1             0.166    0.022    7.534    0.000
    ##    .trauf             0.083    0.028    2.918    0.004
    ## 
    ## 
    ## Group 2 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   ses =~                                              
    ##     housing           1.000                           
    ##     owner            -0.135    0.079   -1.701    0.089
    ##     sumses33          4.816    0.419   11.496    0.000
    ##     school            0.902    0.120    7.509    0.000
    ##     job               0.458    0.198    2.312    0.021
    ##     aid               0.349    0.082    4.265    0.000
    ##     ownincome         0.298    0.078    3.800    0.000
    ##     houseincome       0.605    0.128    4.733    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.063    0.100   10.659    0.000
    ##     bdiq4             1.039    0.097   10.657    0.000
    ##     bdiq5             0.894    0.085   10.534    0.000
    ##     bdiq6             1.278    0.128    9.958    0.000
    ##     bdiq7             0.935    0.092   10.213    0.000
    ##     bdiq8             1.362    0.125   10.922    0.000
    ##     bdiq10            1.225    0.141    8.670    0.000
    ##     bdiq12            1.039    0.103   10.068    0.000
    ##     bdiq13            1.065    0.109    9.818    0.000
    ##     bdiq14            0.979    0.094   10.433    0.000
    ##     bdiq15            0.711    0.092    7.709    0.000
    ##     bdiq17            0.749    0.097    7.746    0.000
    ##     bdiq20            0.576    0.093    6.211    0.000
    ##     bdiq21            0.621    0.115    5.399    0.000
    ##     bdiq22_1          0.297    0.044    6.742    0.000
    ##     bdiq23_1          0.276    0.039    7.025    0.000
    ##     bdiq24_1          0.209    0.037    5.660    0.000
    ##     bdiq25_1          0.225    0.038    5.904    0.000
    ##   trauf =~                                            
    ##     ctq1              1.000                           
    ##     ctq3              1.370    0.219    6.249    0.000
    ##     ctq4              0.720    0.136    5.304    0.000
    ##     ctq6              0.503    0.098    5.117    0.000
    ##     ctq8              1.372    0.212    6.471    0.000
    ##     ctq9              0.702    0.115    6.121    0.000
    ##     ctq11             0.890    0.145    6.122    0.000
    ##     ctq14             1.655    0.251    6.584    0.000
    ##     ctq20             0.652    0.121    5.411    0.000
    ##     ctq23             0.489    0.092    5.308    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf             0.401    0.076    5.299    0.000
    ##     ses              -0.017    0.067   -0.260    0.795
    ##   trauf ~                                             
    ##     ses              -0.156    0.093   -1.676    0.094
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.684    0.023   30.292    0.000
    ##    .owner             0.469    0.024   19.365    0.000
    ##    .sumses33          5.317    0.075   70.939    0.000
    ##    .school            2.366    0.038   62.646    0.000
    ##    .job               3.340    0.063   53.100    0.000
    ##    .aid               0.492    0.024   20.236    0.000
    ##    .ownincome         1.276    0.025   51.886    0.000
    ##    .houseincome       1.962    0.040   49.078    0.000
    ##    .bdiq2             0.368    0.035   10.600    0.000
    ##    .bdiq3             0.396    0.037   10.812    0.000
    ##    .bdiq4             0.540    0.035   15.256    0.000
    ##    .bdiq5             0.429    0.030   14.131    0.000
    ##    .bdiq6             0.528    0.047   11.290    0.000
    ##    .bdiq7             0.335    0.033   10.030    0.000
    ##    .bdiq8             0.474    0.044   10.718    0.000
    ##    .bdiq10            0.673    0.052   12.831    0.000
    ##    .bdiq12            0.514    0.037   13.958    0.000
    ##    .bdiq13            0.545    0.040   13.632    0.000
    ##    .bdiq14            0.337    0.034    9.897    0.000
    ##    .bdiq15            0.729    0.035   20.655    0.000
    ##    .bdiq17            0.825    0.037   22.542    0.000
    ##    .bdiq20            0.877    0.036   24.235    0.000
    ##    .bdiq21            0.939    0.046   20.514    0.000
    ##    .bdiq22_1          0.144    0.017    8.449    0.000
    ##    .bdiq23_1          0.109    0.015    7.191    0.000
    ##    .bdiq24_1          0.099    0.015    6.834    0.000
    ##    .bdiq25_1          0.106    0.015    7.102    0.000
    ##    .ctq1              1.836    0.064   28.837    0.000
    ##    .ctq3              1.809    0.061   29.642    0.000
    ##    .ctq4              1.347    0.047   28.912    0.000
    ##    .ctq6              1.184    0.033   35.351    0.000
    ##    .ctq8              1.506    0.056   26.852    0.000
    ##    .ctq9              1.156    0.031   37.392    0.000
    ##    .ctq11             1.283    0.038   33.352    0.000
    ##    .ctq14             2.347    0.064   36.429    0.000
    ##    .ctq20             1.284    0.038   33.693    0.000
    ##    .ctq23             1.175    0.030   39.656    0.000
    ##     ses               0.000                           
    ##    .depf1             0.000                           
    ##    .trauf             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.114    0.010   11.458    0.000
    ##    .owner             0.247    0.017   14.547    0.000
    ##    .sumses33          0.009    0.188    0.049    0.961
    ##    .school            0.522    0.036   14.311    0.000
    ##    .job               1.656    0.114   14.556    0.000
    ##    .aid               0.237    0.017   14.328    0.000
    ##    .ownincome         0.247    0.017   14.516    0.000
    ##    .houseincome       0.640    0.044   14.466    0.000
    ##    .bdiq2             0.321    0.024   13.360    0.000
    ##    .bdiq3             0.354    0.027   13.312    0.000
    ##    .bdiq4             0.326    0.025   13.297    0.000
    ##    .bdiq5             0.239    0.018   13.316    0.000
    ##    .bdiq6             0.618    0.046   13.566    0.000
    ##    .bdiq7             0.306    0.023   13.493    0.000
    ##    .bdiq8             0.476    0.036   13.109    0.000
    ##    .bdiq10            0.880    0.063   13.906    0.000
    ##    .bdiq12            0.370    0.027   13.454    0.000
    ##    .bdiq13            0.461    0.034   13.625    0.000
    ##    .bdiq14            0.310    0.023   13.378    0.000
    ##    .bdiq15            0.432    0.031   14.094    0.000
    ##    .bdiq17            0.462    0.033   14.081    0.000
    ##    .bdiq20            0.493    0.034   14.297    0.000
    ##    .bdiq21            0.814    0.057   14.382    0.000
    ##    .bdiq22_1          0.107    0.008   14.211    0.000
    ##    .bdiq23_1          0.082    0.006   14.177    0.000
    ##    .bdiq24_1          0.081    0.006   14.321    0.000
    ##    .bdiq25_1          0.085    0.006   14.317    0.000
    ##    .ctq1              1.464    0.106   13.832    0.000
    ##    .ctq3              1.108    0.087   12.743    0.000
    ##    .ctq4              0.790    0.057   13.831    0.000
    ##    .ctq6              0.412    0.030   13.914    0.000
    ##    .ctq8              0.859    0.070   12.287    0.000
    ##    .ctq9              0.281    0.023   12.442    0.000
    ##    .ctq11             0.429    0.035   12.337    0.000
    ##    .ctq14             1.072    0.091   11.806    0.000
    ##    .ctq20             0.508    0.038   13.438    0.000
    ##    .ctq23             0.311    0.023   13.496    0.000
    ##     ses               0.102    0.014    7.264    0.000
    ##    .depf1             0.150    0.023    6.398    0.000
    ##    .trauf             0.249    0.069    3.615    0.000
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     indirect         -0.029    0.015   -1.964    0.049
    ##     total            -0.196    0.061   -3.219    0.001

Socioeconomic status, depression, and 2-factor childhood trauma
===============================================================

``` r
model <- 'ses =~ housing + owner + sumses33 + school + job + aid + ownincome + houseincome
depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1
trauf1 =~ ctq3 + ctq4 + ctq8 + ctq9 + ctq11 + ctq12 + ctq14 + ctq15 + ctq17 + ctq18+ ctq25 
trauf2 =~ ctq20 + ctq21 + ctq23 + ctq24 + ctq27

depf1 ~ c*trauf1 + d*trauf2 + e*ses
trauf1 ~ a*ses 
trauf2 ~ b*ses

indirect := a*c + b*d
total := e + (a*c + b*d)
'

fit <- sem(model, drak, missing = 'fiml.x', group = "clinic")
summary(fit)
```

    ## lavaan 0.6-3 ended normally after 189 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        268
    ## 
    ##   Number of observations per group         
    ##   0                                                542
    ##   1                                                424
    ##   Number of missing patterns per group     
    ##   0                                                 19
    ##   1                                                  8
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    4916.118
    ##   Degrees of freedom                              1710
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Chi-square for each group:
    ## 
    ##   0                                           2475.638
    ##   1                                           2440.480
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Observed
    ##   Observed information based on                Hessian
    ##   Standard Errors                             Standard
    ## 
    ## 
    ## Group 1 [0]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   ses =~                                              
    ##     housing           1.000                           
    ##     owner             0.276    0.055    5.024    0.000
    ##     sumses33          4.348    0.543    8.012    0.000
    ##     school            0.567    0.100    5.645    0.000
    ##     job              -0.060    0.233   -0.258    0.796
    ##     aid               0.093    0.067    1.390    0.164
    ##     ownincome        -0.004    0.055   -0.068    0.946
    ##     houseincome       0.067    0.097    0.696    0.487
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.153    0.091   12.641    0.000
    ##     bdiq4             1.489    0.109   13.651    0.000
    ##     bdiq5             0.935    0.081   11.495    0.000
    ##     bdiq6             1.343    0.100   13.496    0.000
    ##     bdiq7             1.226    0.094   13.043    0.000
    ##     bdiq8             1.577    0.120   13.117    0.000
    ##     bdiq10            1.221    0.105   11.632    0.000
    ##     bdiq12            1.238    0.094   13.212    0.000
    ##     bdiq13            1.288    0.093   13.862    0.000
    ##     bdiq14            1.231    0.088   14.012    0.000
    ##     bdiq15            1.288    0.094   13.696    0.000
    ##     bdiq17            1.021    0.088   11.580    0.000
    ##     bdiq20            1.305    0.100   13.026    0.000
    ##     bdiq21            1.256    0.123   10.194    0.000
    ##     bdiq22_1          0.057    0.020    2.849    0.004
    ##     bdiq23_1          0.041    0.013    3.176    0.001
    ##     bdiq24_1          0.018    0.008    2.301    0.021
    ##     bdiq25_1          0.014    0.006    2.161    0.031
    ##   trauf1 =~                                           
    ##     ctq3              1.000                           
    ##     ctq4              1.470    0.301    4.882    0.000
    ##     ctq8              1.084    0.219    4.954    0.000
    ##     ctq9              2.141    0.389    5.508    0.000
    ##     ctq11             2.346    0.410    5.717    0.000
    ##     ctq12             2.588    0.480    5.397    0.000
    ##     ctq14             2.049    0.367    5.590    0.000
    ##     ctq15             2.427    0.425    5.714    0.000
    ##     ctq17             2.092    0.367    5.703    0.000
    ##     ctq18             2.388    0.443    5.388    0.000
    ##     ctq25             2.311    0.411    5.619    0.000
    ##   trauf2 =~                                           
    ##     ctq20             1.000                           
    ##     ctq21             0.921    0.040   23.277    0.000
    ##     ctq23             0.825    0.041   20.377    0.000
    ##     ctq24             0.552    0.042   13.061    0.000
    ##     ctq27             0.513    0.048   10.603    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf1     (c)    0.197    0.096    2.048    0.041
    ##     trauf2     (d)    0.087    0.031    2.789    0.005
    ##     ses        (e)   -0.176    0.059   -2.999    0.003
    ##   trauf1 ~                                            
    ##     ses        (a)   -0.051    0.036   -1.436    0.151
    ##   trauf2 ~                                            
    ##     ses        (b)   -0.157    0.095   -1.645    0.100
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.561    0.021   26.312    0.000
    ##    .owner             0.247    0.019   13.342    0.000
    ##    .sumses33          4.319    0.078   55.590    0.000
    ##    .school            2.345    0.033   70.128    0.000
    ##    .job               2.293    0.075   30.616    0.000
    ##    .aid               0.511    0.021   23.802    0.000
    ##    .ownincome         1.212    0.018   66.846    0.000
    ##    .houseincome       1.662    0.029   57.538    0.000
    ##    .bdiq2             0.359    0.030   12.043    0.000
    ##    .bdiq3             0.428    0.033   12.973    0.000
    ##    .bdiq4             0.618    0.038   16.467    0.000
    ##    .bdiq5             0.374    0.029   12.682    0.000
    ##    .bdiq6             0.405    0.034   11.763    0.000
    ##    .bdiq7             0.403    0.033   12.250    0.000
    ##    .bdiq8             0.577    0.042   13.706    0.000
    ##    .bdiq10            0.480    0.038   12.592    0.000
    ##    .bdiq12            0.482    0.033   14.698    0.000
    ##    .bdiq13            0.420    0.032   13.188    0.000
    ##    .bdiq14            0.336    0.030   11.115    0.000
    ##    .bdiq15            0.464    0.032   14.413    0.000
    ##    .bdiq17            0.444    0.032   13.868    0.000
    ##    .bdiq20            0.524    0.035   15.014    0.000
    ##    .bdiq21            0.638    0.046   13.945    0.000
    ##    .bdiq22_1          0.037    0.008    4.559    0.000
    ##    .bdiq23_1          0.015    0.005    2.852    0.004
    ##    .bdiq24_1          0.006    0.003    1.738    0.082
    ##    .bdiq25_1          0.004    0.003    1.418    0.156
    ##    .ctq3              1.396    0.038   36.693    0.000
    ##    .ctq4              1.408    0.043   32.771    0.000
    ##    .ctq8              1.201    0.031   39.050    0.000
    ##    .ctq9              1.264    0.039   32.154    0.000
    ##    .ctq11             1.240    0.035   35.422    0.000
    ##    .ctq12             2.142    0.053   40.265    0.000
    ##    .ctq14             1.351    0.041   33.250    0.000
    ##    .ctq15             1.283    0.039   32.506    0.000
    ##    .ctq17             1.200    0.033   36.020    0.000
    ##    .ctq18             1.556    0.051   30.672    0.000
    ##    .ctq25             1.339    0.042   31.922    0.000
    ##    .ctq20             1.205    0.035   34.386    0.000
    ##    .ctq21             1.190    0.032   36.666    0.000
    ##    .ctq23             1.189    0.032   37.385    0.000
    ##    .ctq24             1.174    0.030   38.747    0.000
    ##    .ctq27             1.187    0.033   35.552    0.000
    ##     ses               0.000                           
    ##    .depf1             0.000                           
    ##    .trauf1            0.000                           
    ##    .trauf2            0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.109    0.017    6.276    0.000
    ##    .owner             0.176    0.011   16.101    0.000
    ##    .sumses33          0.676    0.311    2.171    0.030
    ##    .school            0.562    0.035   16.179    0.000
    ##    .job               3.041    0.185   16.461    0.000
    ##    .aid               0.249    0.015   16.429    0.000
    ##    .ownincome         0.178    0.011   16.462    0.000
    ##    .houseincome       0.452    0.027   16.451    0.000
    ##    .bdiq2             0.303    0.019   15.600    0.000
    ##    .bdiq3             0.352    0.023   15.484    0.000
    ##    .bdiq4             0.369    0.025   14.954    0.000
    ##    .bdiq5             0.315    0.020   15.725    0.000
    ##    .bdiq6             0.322    0.022   14.979    0.000
    ##    .bdiq7             0.320    0.021   15.217    0.000
    ##    .bdiq8             0.515    0.034   15.235    0.000
    ##    .bdiq10            0.521    0.033   15.717    0.000
    ##    .bdiq12            0.309    0.020   15.182    0.000
    ##    .bdiq13            0.252    0.017   14.769    0.000
    ##    .bdiq14            0.223    0.015   14.720    0.000
    ##    .bdiq15            0.265    0.018   14.817    0.000
    ##    .bdiq17            0.367    0.023   15.680    0.000
    ##    .bdiq20            0.356    0.023   15.182    0.000
    ##    .bdiq21            0.845    0.053   15.923    0.000
    ##    .bdiq22_1          0.035    0.002   16.363    0.000
    ##    .bdiq23_1          0.014    0.001   16.357    0.000
    ##    .bdiq24_1          0.006    0.000   16.356    0.000
    ##    .bdiq25_1          0.004    0.000   16.373    0.000
    ##    .ctq3              0.723    0.045   16.155    0.000
    ##    .ctq4              0.868    0.055   15.920    0.000
    ##    .ctq8              0.442    0.028   15.897    0.000
    ##    .ctq9              0.562    0.038   14.732    0.000
    ##    .ctq11             0.335    0.026   12.988    0.000
    ##    .ctq12             1.129    0.074   15.210    0.000
    ##    .ctq14             0.643    0.043   15.051    0.000
    ##    .ctq15             0.491    0.035   13.905    0.000
    ##    .ctq17             0.338    0.024   13.779    0.000
    ##    .ctq18             1.049    0.069   15.302    0.000
    ##    .ctq25             0.630    0.043   14.662    0.000
    ##    .ctq20             0.174    0.017   10.105    0.000
    ##    .ctq21             0.154    0.015   10.314    0.000
    ##    .ctq23             0.212    0.017   12.731    0.000
    ##    .ctq24             0.347    0.022   15.436    0.000
    ##    .ctq27             0.472    0.030   15.805    0.000
    ##     ses               0.137    0.021    6.534    0.000
    ##    .depf1             0.165    0.022    7.550    0.000
    ##    .trauf1            0.059    0.020    2.972    0.003
    ##    .trauf2            0.486    0.041   11.879    0.000
    ## 
    ## 
    ## Group 2 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   ses =~                                              
    ##     housing           1.000                           
    ##     owner            -0.136    0.079   -1.712    0.087
    ##     sumses33          4.806    0.419   11.463    0.000
    ##     school            0.902    0.120    7.507    0.000
    ##     job               0.458    0.198    2.311    0.021
    ##     aid               0.350    0.082    4.268    0.000
    ##     ownincome         0.298    0.079    3.799    0.000
    ##     houseincome       0.606    0.128    4.734    0.000
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.060    0.099   10.684    0.000
    ##     bdiq4             1.037    0.097   10.692    0.000
    ##     bdiq5             0.888    0.084   10.532    0.000
    ##     bdiq6             1.275    0.128    9.982    0.000
    ##     bdiq7             0.932    0.091   10.234    0.000
    ##     bdiq8             1.355    0.124   10.931    0.000
    ##     bdiq10            1.221    0.141    8.686    0.000
    ##     bdiq12            1.034    0.103   10.079    0.000
    ##     bdiq13            1.060    0.108    9.824    0.000
    ##     bdiq14            0.975    0.093   10.445    0.000
    ##     bdiq15            0.710    0.092    7.733    0.000
    ##     bdiq17            0.746    0.096    7.749    0.000
    ##     bdiq20            0.572    0.092    6.194    0.000
    ##     bdiq21            0.626    0.115    5.457    0.000
    ##     bdiq22_1          0.297    0.044    6.778    0.000
    ##     bdiq23_1          0.276    0.039    7.058    0.000
    ##     bdiq24_1          0.210    0.037    5.698    0.000
    ##     bdiq25_1          0.226    0.038    5.952    0.000
    ##   trauf1 =~                                           
    ##     ctq3              1.000                           
    ##     ctq4              0.466    0.090    5.185    0.000
    ##     ctq8              1.076    0.131    8.214    0.000
    ##     ctq9              0.641    0.076    8.422    0.000
    ##     ctq11             0.917    0.104    8.822    0.000
    ##     ctq12             1.090    0.135    8.095    0.000
    ##     ctq14             1.332    0.155    8.617    0.000
    ##     ctq15             0.992    0.111    8.896    0.000
    ##     ctq17             0.919    0.104    8.804    0.000
    ##     ctq18             1.397    0.153    9.109    0.000
    ##     ctq25             1.264    0.142    8.915    0.000
    ##   trauf2 =~                                           
    ##     ctq20             1.000                           
    ##     ctq21             0.957    0.051   18.834    0.000
    ##     ctq23             0.849    0.046   18.449    0.000
    ##     ctq24             0.949    0.053   17.802    0.000
    ##     ctq27             0.922    0.049   18.781    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf1            0.296    0.053    5.568    0.000
    ##     trauf2            0.053    0.040    1.326    0.185
    ##     ses              -0.037    0.066   -0.562    0.574
    ##   trauf1 ~                                            
    ##     ses              -0.130    0.102   -1.278    0.201
    ##   trauf2 ~                                            
    ##     ses              -0.095    0.098   -0.970    0.332
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.684    0.023   30.292    0.000
    ##    .owner             0.469    0.024   19.365    0.000
    ##    .sumses33          5.317    0.075   70.941    0.000
    ##    .school            2.366    0.038   62.646    0.000
    ##    .job               3.340    0.063   53.101    0.000
    ##    .aid               0.492    0.024   20.236    0.000
    ##    .ownincome         1.276    0.025   51.886    0.000
    ##    .houseincome       1.962    0.040   49.077    0.000
    ##    .bdiq2             0.368    0.035   10.645    0.000
    ##    .bdiq3             0.396    0.036   10.859    0.000
    ##    .bdiq4             0.540    0.035   15.323    0.000
    ##    .bdiq5             0.429    0.030   14.193    0.000
    ##    .bdiq6             0.528    0.047   11.333    0.000
    ##    .bdiq7             0.335    0.033   10.070    0.000
    ##    .bdiq8             0.474    0.044   10.769    0.000
    ##    .bdiq10            0.673    0.052   12.866    0.000
    ##    .bdiq12            0.514    0.037   14.015    0.000
    ##    .bdiq13            0.545    0.040   13.681    0.000
    ##    .bdiq14            0.337    0.034    9.939    0.000
    ##    .bdiq15            0.729    0.035   20.698    0.000
    ##    .bdiq17            0.825    0.037   22.589    0.000
    ##    .bdiq20            0.877    0.036   24.265    0.000
    ##    .bdiq21            0.939    0.046   20.534    0.000
    ##    .bdiq22_1          0.144    0.017    8.462    0.000
    ##    .bdiq23_1          0.109    0.015    7.203    0.000
    ##    .bdiq24_1          0.099    0.015    6.840    0.000
    ##    .bdiq25_1          0.106    0.015    7.109    0.000
    ##    .ctq3              1.809    0.061   29.642    0.000
    ##    .ctq4              1.347    0.047   28.912    0.000
    ##    .ctq8              1.505    0.056   26.838    0.000
    ##    .ctq9              1.156    0.031   37.392    0.000
    ##    .ctq11             1.283    0.038   33.353    0.000
    ##    .ctq12             1.764    0.056   31.597    0.000
    ##    .ctq14             2.347    0.064   36.429    0.000
    ##    .ctq15             1.290    0.042   30.664    0.000
    ##    .ctq17             1.242    0.038   32.644    0.000
    ##    .ctq18             1.731    0.060   28.938    0.000
    ##    .ctq25             1.547    0.055   27.996    0.000
    ##    .ctq20             1.283    0.038   33.707    0.000
    ##    .ctq21             1.198    0.032   37.313    0.000
    ##    .ctq23             1.175    0.030   39.701    0.000
    ##    .ctq24             1.193    0.034   34.906    0.000
    ##    .ctq27             1.167    0.031   37.622    0.000
    ##     ses               0.000                           
    ##    .depf1             0.000                           
    ##    .trauf1            0.000                           
    ##    .trauf2            0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.114    0.010   11.443    0.000
    ##    .owner             0.247    0.017   14.547    0.000
    ##    .sumses33          0.015    0.190    0.077    0.939
    ##    .school            0.521    0.036   14.302    0.000
    ##    .job               1.656    0.114   14.556    0.000
    ##    .aid               0.237    0.017   14.321    0.000
    ##    .ownincome         0.247    0.017   14.512    0.000
    ##    .houseincome       0.640    0.044   14.461    0.000
    ##    .bdiq2             0.319    0.024   13.349    0.000
    ##    .bdiq3             0.354    0.027   13.315    0.000
    ##    .bdiq4             0.326    0.024   13.295    0.000
    ##    .bdiq5             0.240    0.018   13.332    0.000
    ##    .bdiq6             0.617    0.046   13.566    0.000
    ##    .bdiq7             0.306    0.023   13.494    0.000
    ##    .bdiq8             0.478    0.036   13.123    0.000
    ##    .bdiq10            0.880    0.063   13.905    0.000
    ##    .bdiq12            0.370    0.028   13.460    0.000
    ##    .bdiq13            0.462    0.034   13.631    0.000
    ##    .bdiq14            0.311    0.023   13.385    0.000
    ##    .bdiq15            0.431    0.031   14.093    0.000
    ##    .bdiq17            0.462    0.033   14.083    0.000
    ##    .bdiq20            0.493    0.034   14.300    0.000
    ##    .bdiq21            0.813    0.057   14.378    0.000
    ##    .bdiq22_1          0.106    0.007   14.207    0.000
    ##    .bdiq23_1          0.082    0.006   14.173    0.000
    ##    .bdiq24_1          0.081    0.006   14.318    0.000
    ##    .bdiq25_1          0.085    0.006   14.313    0.000
    ##    .ctq3              1.216    0.088   13.870    0.000
    ##    .ctq4              0.841    0.059   14.347    0.000
    ##    .ctq8              0.912    0.067   13.558    0.000
    ##    .ctq9              0.256    0.019   13.248    0.000
    ##    .ctq11             0.322    0.027   12.125    0.000
    ##    .ctq12             0.890    0.066   13.511    0.000
    ##    .ctq14             1.115    0.085   13.175    0.000
    ##    .ctq15             0.392    0.031   12.519    0.000
    ##    .ctq17             0.306    0.025   12.086    0.000
    ##    .ctq18             0.808    0.065   12.514    0.000
    ##    .ctq25             0.715    0.056   12.708    0.000
    ##    .ctq20             0.253    0.020   12.606    0.000
    ##    .ctq21             0.107    0.010   10.462    0.000
    ##    .ctq23             0.111    0.010   11.427    0.000
    ##    .ctq24             0.170    0.014   12.003    0.000
    ##    .ctq27             0.101    0.010   10.548    0.000
    ##     ses               0.102    0.014    7.270    0.000
    ##    .depf1             0.154    0.024    6.519    0.000
    ##    .trauf1            0.361    0.074    4.896    0.000
    ##    .trauf2            0.360    0.040    9.096    0.000
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     indirect         -0.024    0.012   -1.940    0.052
    ##     total            -0.200    0.060   -3.327    0.001
