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
         houseincome = SESaQ19
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

``` r
semPaths(fit, title = FALSE, curvePivot = TRUE)
```

![](drak_ct_files/figure-markdown_github/drak_med-1.png)![](drak_ct_files/figure-markdown_github/drak_med-2.png)

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

Plot of CTQ by SES category
===========================

``` r
drak_plot = drak %>% 
  mutate(ctq = ctq1 + ctq3 + ctq4 + ctq6 + ctq8 + ctq9 + ctq11 + ctq14 + ctq20 + ctq23,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome,
         ses1 = ses <= 5,
         ses2 = ses > 5 & ses <= 10,
         ses3 = ses > 10 & ses <= 15,
         ses4 = ses > 15 & ses <= 20,
         ses5 = ses > 20 & ses <= 25,
         ses6 = ses > 25) %>%
  gather(ses1:ses6, key = "sescat", value = "bin") %>% 
  filter(bin == TRUE) %>% 
  select(-bin) %>% 
  group_by(sescat) %>% 
  summarize(mean_ctq = mean(ctq, na.rm = TRUE),
            sd_ctq = sd(ctq, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(x = sescat, y = mean_ctq)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_ctq - sd_ctq, ymax = mean_ctq + sd_ctq), width = 0.2) +
  labs(x = "Category of socioeconomic status",
       y = "Childhood trauma")
```

Socioeconomic status and 1-factor childhood trauma
==================================================

``` r
model <- 'ses =~ housing + owner + sumses33 + school + job + aid + ownincome + houseincome
trauf1 =~ ctq3 + ctq4 + ctq8 + ctq9 + ctq11 + ctq12 + ctq14 + ctq15 + ctq17 + ctq18+ ctq25 
trauf2 =~ ctq20 + ctq21 + ctq23 + ctq24 + ctq27 

trauf1 ~ ses
trauf2 ~ ses'

fit <- sem(model, drak, missing = 'fiml.x', group = "clinic")
```

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
    ## variances are negative

``` r
summary(fit)
```

    ## lavaan 0.6-3 ended normally after 126 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        150
    ## 
    ##   Number of observations per group         
    ##   0                                                542
    ##   1                                                424
    ##   Number of missing patterns per group     
    ##   0                                                 10
    ##   1                                                  7
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    2084.021
    ##   Degrees of freedom                               498
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Chi-square for each group:
    ## 
    ##   0                                            981.233
    ##   1                                           1102.788
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
    ##     owner             0.254    0.054    4.672    0.000
    ##     sumses33          3.491    0.622    5.610    0.000
    ##     school            0.484    0.124    3.894    0.000
    ##     job              -0.266    0.233   -1.142    0.254
    ##     aid               0.129    0.059    2.169    0.030
    ##     ownincome        -0.040    0.054   -0.742    0.458
    ##     houseincome      -0.036    0.103   -0.347    0.728
    ##   trauf1 =~                                           
    ##     ctq3              1.000                           
    ##     ctq4              1.481    0.300    4.941    0.000
    ##     ctq8              1.104    0.219    5.033    0.000
    ##     ctq9              2.122    0.382    5.557    0.000
    ##     ctq11             2.299    0.399    5.766    0.000
    ##     ctq12             2.595    0.476    5.456    0.000
    ##     ctq14             2.040    0.362    5.633    0.000
    ##     ctq15             2.373    0.413    5.748    0.000
    ##     ctq17             2.079    0.361    5.756    0.000
    ##     ctq18             2.378    0.438    5.434    0.000
    ##     ctq25             2.367    0.415    5.698    0.000
    ##   trauf2 =~                                           
    ##     ctq20             1.000                           
    ##     ctq21             0.913    0.039   23.282    0.000
    ##     ctq23             0.834    0.041   20.441    0.000
    ##     ctq24             0.578    0.043   13.580    0.000
    ##     ctq27             0.515    0.049   10.609    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   trauf1 ~                                            
    ##     ses              -0.030    0.035   -0.849    0.396
    ##   trauf2 ~                                            
    ##     ses              -0.120    0.090   -1.325    0.185
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .trauf1 ~~                                           
    ##    .trauf2            0.080    0.016    4.967    0.000
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
    ##    .ctq3              1.396    0.038   36.693    0.000
    ##    .ctq4              1.408    0.043   32.773    0.000
    ##    .ctq8              1.201    0.031   39.051    0.000
    ##    .ctq9              1.264    0.039   32.155    0.000
    ##    .ctq11             1.240    0.035   35.424    0.000
    ##    .ctq12             2.142    0.053   40.266    0.000
    ##    .ctq14             1.351    0.041   33.251    0.000
    ##    .ctq15             1.283    0.039   32.507    0.000
    ##    .ctq17             1.200    0.033   36.024    0.000
    ##    .ctq18             1.557    0.051   30.673    0.000
    ##    .ctq25             1.339    0.042   31.926    0.000
    ##    .ctq20             1.206    0.035   34.409    0.000
    ##    .ctq21             1.191    0.032   36.688    0.000
    ##    .ctq23             1.189    0.032   37.406    0.000
    ##    .ctq24             1.174    0.030   38.760    0.000
    ##    .ctq27             1.187    0.033   35.561    0.000
    ##     ses               0.000                           
    ##    .trauf1            0.000                           
    ##    .trauf2            0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.075    0.030    2.503    0.012
    ##    .owner             0.175    0.011   16.207    0.000
    ##    .sumses33          1.181    0.370    3.188    0.001
    ##    .school            0.566    0.036   15.822    0.000
    ##    .job               3.029    0.185   16.408    0.000
    ##    .aid               0.247    0.015   16.372    0.000
    ##    .ownincome         0.178    0.011   16.446    0.000
    ##    .houseincome       0.452    0.027   16.452    0.000
    ##    .ctq3              0.722    0.045   16.165    0.000
    ##    .ctq4              0.865    0.054   15.924    0.000
    ##    .ctq8              0.439    0.028   15.889    0.000
    ##    .ctq9              0.563    0.038   14.817    0.000
    ##    .ctq11             0.345    0.026   13.304    0.000
    ##    .ctq12             1.123    0.074   15.232    0.000
    ##    .ctq14             0.643    0.043   15.117    0.000
    ##    .ctq15             0.503    0.036   14.133    0.000
    ##    .ctq17             0.338    0.024   13.894    0.000
    ##    .ctq18             1.048    0.068   15.352    0.000
    ##    .ctq25             0.611    0.042   14.544    0.000
    ##    .ctq20             0.177    0.017   10.373    0.000
    ##    .ctq21             0.164    0.015   10.968    0.000
    ##    .ctq23             0.207    0.016   12.632    0.000
    ##    .ctq24             0.333    0.022   15.250    0.000
    ##    .ctq27             0.472    0.030   15.811    0.000
    ##     ses               0.171    0.033    5.183    0.000
    ##    .trauf1            0.060    0.020    3.001    0.003
    ##    .trauf2            0.483    0.041   11.832    0.000
    ## 
    ## 
    ## Group 2 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   ses =~                                              
    ##     housing           1.000                           
    ##     owner            -0.133    0.079   -1.677    0.094
    ##     sumses33          4.846    0.427   11.357    0.000
    ##     school            0.901    0.120    7.503    0.000
    ##     job               0.459    0.198    2.315    0.021
    ##     aid               0.346    0.082    4.217    0.000
    ##     ownincome         0.297    0.078    3.784    0.000
    ##     houseincome       0.603    0.128    4.719    0.000
    ##   trauf1 =~                                           
    ##     ctq3              1.000                           
    ##     ctq4              0.475    0.094    5.063    0.000
    ##     ctq8              1.110    0.139    7.981    0.000
    ##     ctq9              0.643    0.080    8.081    0.000
    ##     ctq11             0.951    0.111    8.598    0.000
    ##     ctq12             1.097    0.141    7.789    0.000
    ##     ctq14             1.338    0.162    8.244    0.000
    ##     ctq15             1.091    0.124    8.798    0.000
    ##     ctq17             0.949    0.111    8.563    0.000
    ##     ctq18             1.394    0.161    8.679    0.000
    ##     ctq25             1.371    0.156    8.774    0.000
    ##   trauf2 =~                                           
    ##     ctq20             1.000                           
    ##     ctq21             0.949    0.050   18.955    0.000
    ##     ctq23             0.845    0.045   18.603    0.000
    ##     ctq24             0.946    0.053   17.974    0.000
    ##     ctq27             0.916    0.048   18.935    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   trauf1 ~                                            
    ##     ses              -0.131    0.097   -1.341    0.180
    ##   trauf2 ~                                            
    ##     ses              -0.093    0.097   -0.957    0.338
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .trauf1 ~~                                           
    ##    .trauf2            0.172    0.028    6.227    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.684    0.023   30.292    0.000
    ##    .owner             0.469    0.024   19.365    0.000
    ##    .sumses33          5.318    0.075   70.945    0.000
    ##    .school            2.366    0.038   62.646    0.000
    ##    .job               3.340    0.063   53.101    0.000
    ##    .aid               0.492    0.024   20.236    0.000
    ##    .ownincome         1.276    0.025   51.886    0.000
    ##    .houseincome       1.962    0.040   49.077    0.000
    ##    .ctq3              1.809    0.061   29.642    0.000
    ##    .ctq4              1.347    0.047   28.912    0.000
    ##    .ctq8              1.505    0.056   26.836    0.000
    ##    .ctq9              1.156    0.031   37.392    0.000
    ##    .ctq11             1.283    0.038   33.352    0.000
    ##    .ctq12             1.764    0.056   31.597    0.000
    ##    .ctq14             2.347    0.064   36.429    0.000
    ##    .ctq15             1.290    0.042   30.666    0.000
    ##    .ctq17             1.242    0.038   32.643    0.000
    ##    .ctq18             1.731    0.060   28.938    0.000
    ##    .ctq25             1.547    0.055   27.996    0.000
    ##    .ctq20             1.283    0.038   33.707    0.000
    ##    .ctq21             1.198    0.032   37.313    0.000
    ##    .ctq23             1.175    0.030   39.700    0.000
    ##    .ctq24             1.193    0.034   34.906    0.000
    ##    .ctq27             1.167    0.031   37.622    0.000
    ##     ses               0.000                           
    ##    .trauf1            0.000                           
    ##    .trauf2            0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.115    0.010   11.419    0.000
    ##    .owner             0.247    0.017   14.548    0.000
    ##    .sumses33         -0.008    0.192   -0.041    0.967
    ##    .school            0.522    0.036   14.322    0.000
    ##    .job               1.656    0.114   14.557    0.000
    ##    .aid               0.238    0.017   14.329    0.000
    ##    .ownincome         0.247    0.017   14.518    0.000
    ##    .houseincome       0.641    0.044   14.469    0.000
    ##    .ctq3              1.241    0.089   13.953    0.000
    ##    .ctq4              0.844    0.059   14.363    0.000
    ##    .ctq8              0.916    0.067   13.602    0.000
    ##    .ctq9              0.265    0.020   13.332    0.000
    ##    .ctq11             0.322    0.026   12.209    0.000
    ##    .ctq12             0.915    0.067   13.596    0.000
    ##    .ctq14             1.155    0.086   13.371    0.000
    ##    .ctq15             0.348    0.029   11.885    0.000
    ##    .ctq17             0.309    0.025   12.225    0.000
    ##    .ctq18             0.862    0.067   12.844    0.000
    ##    .ctq25             0.660    0.054   12.288    0.000
    ##    .ctq20             0.249    0.020   12.595    0.000
    ##    .ctq21             0.109    0.010   10.648    0.000
    ##    .ctq23             0.110    0.010   11.513    0.000
    ##    .ctq24             0.169    0.014   12.047    0.000
    ##    .ctq27             0.102    0.010   10.652    0.000
    ##     ses               0.101    0.014    7.218    0.000
    ##    .trauf1            0.336    0.071    4.709    0.000
    ##    .trauf2            0.364    0.040    9.173    0.000

Socioeconomic status and 2-factor childhood trauma
==================================================

``` r
model <- 'ses =~ housing + owner + sumses33 + school + job + aid + ownincome + houseincome
trauf =~ ctq1 + ctq3 + ctq4 + ctq6 + ctq8 + ctq9 + ctq11 + ctq14 + ctq20 + ctq23 

trauf ~ ses '

fit <- sem(model, drak, missing = 'fiml.x', group = "clinic")
summary(fit)
```

    ## lavaan 0.6-3 ended normally after 108 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        110
    ## 
    ##   Number of observations per group         
    ##   0                                                542
    ##   1                                                424
    ##   Number of missing patterns per group     
    ##   0                                                  7
    ##   1                                                  5
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    1696.783
    ##   Degrees of freedom                               268
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Chi-square for each group:
    ## 
    ##   0                                            914.337
    ##   1                                            782.446
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
    ##     owner             0.255    0.054    4.765    0.000
    ##     sumses33          3.549    0.593    5.984    0.000
    ##     school            0.494    0.119    4.146    0.000
    ##     job              -0.258    0.233   -1.107    0.268
    ##     aid               0.127    0.060    2.109    0.035
    ##     ownincome        -0.037    0.054   -0.685    0.493
    ##     houseincome      -0.029    0.102   -0.285    0.776
    ##   trauf =~                                            
    ##     ctq1              1.000                           
    ##     ctq3              1.036    0.238    4.344    0.000
    ##     ctq4              1.497    0.322    4.643    0.000
    ##     ctq6              0.865    0.189    4.573    0.000
    ##     ctq8              1.188    0.250    4.756    0.000
    ##     ctq9              1.693    0.345    4.904    0.000
    ##     ctq11             1.695    0.329    5.153    0.000
    ##     ctq14             1.827    0.368    4.964    0.000
    ##     ctq20             1.549    0.325    4.761    0.000
    ##     ctq23             1.433    0.297    4.830    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   trauf ~                                             
    ##     ses              -0.078    0.042   -1.846    0.065
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
    ##    .ctq1              1.590    0.043   36.976    0.000
    ##    .ctq3              1.395    0.038   36.692    0.000
    ##    .ctq4              1.408    0.043   32.772    0.000
    ##    .ctq6              1.160    0.028   41.884    0.000
    ##    .ctq8              1.201    0.031   39.049    0.000
    ##    .ctq9              1.263    0.039   32.141    0.000
    ##    .ctq11             1.240    0.035   35.422    0.000
    ##    .ctq14             1.351    0.041   33.249    0.000
    ##    .ctq20             1.205    0.035   34.391    0.000
    ##    .ctq23             1.188    0.032   37.390    0.000
    ##     ses               0.000                           
    ##    .trauf             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.078    0.028    2.813    0.005
    ##    .owner             0.175    0.011   16.207    0.000
    ##    .sumses33          1.147    0.352    3.255    0.001
    ##    .school            0.565    0.036   15.907    0.000
    ##    .job               3.030    0.185   16.415    0.000
    ##    .aid               0.247    0.015   16.377    0.000
    ##    .ownincome         0.178    0.011   16.449    0.000
    ##    .houseincome       0.452    0.027   16.456    0.000
    ##    .ctq1              0.926    0.058   15.948    0.000
    ##    .ctq3              0.703    0.045   15.703    0.000
    ##    .ctq4              0.831    0.055   15.141    0.000
    ##    .ctq6              0.358    0.023   15.438    0.000
    ##    .ctq8              0.408    0.028   14.817    0.000
    ##    .ctq9              0.622    0.045   13.880    0.000
    ##    .ctq11             0.451    0.035   12.930    0.000
    ##    .ctq14             0.646    0.047   13.765    0.000
    ##    .ctq20             0.486    0.039   12.591    0.000
    ##    .ctq23             0.394    0.032   12.365    0.000
    ##     ses               0.168    0.031    5.456    0.000
    ##    .trauf             0.073    0.027    2.740    0.006
    ## 
    ## 
    ## Group 2 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   ses =~                                              
    ##     housing           1.000                           
    ##     owner            -0.135    0.079   -1.706    0.088
    ##     sumses33          4.808    0.418   11.499    0.000
    ##     school            0.902    0.120    7.510    0.000
    ##     job               0.459    0.198    2.314    0.021
    ##     aid               0.350    0.082    4.271    0.000
    ##     ownincome         0.298    0.078    3.805    0.000
    ##     houseincome       0.606    0.128    4.740    0.000
    ##   trauf =~                                            
    ##     ctq1              1.000                           
    ##     ctq3              1.402    0.231    6.070    0.000
    ##     ctq4              0.732    0.142    5.165    0.000
    ##     ctq6              0.528    0.104    5.071    0.000
    ##     ctq8              1.427    0.226    6.308    0.000
    ##     ctq9              0.746    0.125    5.984    0.000
    ##     ctq11             0.942    0.158    5.968    0.000
    ##     ctq14             1.622    0.257    6.311    0.000
    ##     ctq20             0.666    0.127    5.260    0.000
    ##     ctq23             0.486    0.096    5.084    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   trauf ~                                             
    ##     ses              -0.150    0.091   -1.648    0.099
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.684    0.023   30.292    0.000
    ##    .owner             0.469    0.024   19.365    0.000
    ##    .sumses33          5.317    0.075   70.945    0.000
    ##    .school            2.366    0.038   62.646    0.000
    ##    .job               3.340    0.063   53.101    0.000
    ##    .aid               0.492    0.024   20.236    0.000
    ##    .ownincome         1.276    0.025   51.886    0.000
    ##    .houseincome       1.962    0.040   49.078    0.000
    ##    .ctq1              1.836    0.064   28.838    0.000
    ##    .ctq3              1.809    0.061   29.642    0.000
    ##    .ctq4              1.347    0.047   28.912    0.000
    ##    .ctq6              1.184    0.033   35.351    0.000
    ##    .ctq8              1.506    0.056   26.850    0.000
    ##    .ctq9              1.156    0.031   37.392    0.000
    ##    .ctq11             1.283    0.038   33.352    0.000
    ##    .ctq14             2.347    0.064   36.429    0.000
    ##    .ctq20             1.284    0.038   33.691    0.000
    ##    .ctq23             1.175    0.030   39.656    0.000
    ##     ses               0.000                           
    ##    .trauf             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .housing           0.114    0.010   11.433    0.000
    ##    .owner             0.247    0.017   14.546    0.000
    ##    .sumses33          0.014    0.188    0.073    0.942
    ##    .school            0.522    0.036   14.312    0.000
    ##    .job               1.656    0.114   14.556    0.000
    ##    .aid               0.237    0.017   14.324    0.000
    ##    .ownincome         0.247    0.017   14.516    0.000
    ##    .houseincome       0.640    0.044   14.467    0.000
    ##    .ctq1              1.477    0.107   13.822    0.000
    ##    .ctq3              1.110    0.088   12.610    0.000
    ##    .ctq4              0.792    0.057   13.775    0.000
    ##    .ctq6              0.409    0.030   13.838    0.000
    ##    .ctq8              0.846    0.070   12.013    0.000
    ##    .ctq9              0.272    0.023   12.040    0.000
    ##    .ctq11             0.416    0.035   11.907    0.000
    ##    .ctq14             1.132    0.094   12.044    0.000
    ##    .ctq20             0.508    0.038   13.352    0.000
    ##    .ctq23             0.315    0.023   13.487    0.000
    ##     ses               0.102    0.014    7.266    0.000
    ##    .trauf             0.236    0.068    3.486    0.000

1-factor childhood trauma and depression
========================================

``` r
model <- 'depf1 =~ bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq10 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq17 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1
trauf =~ ctq1 + ctq3 + ctq4 + ctq6 + ctq8 + ctq9 + ctq11 + ctq14 + ctq20 + ctq23 

depf1 ~ trauf '

fit <- sem(model, drak, missing = 'fiml.x', group = "clinic")
```

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some cases are empty and will be ignored:
    ##   541

``` r
summary(fit)
```

    ## lavaan 0.6-3 ended normally after 136 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        176
    ## 
    ##                                                   Used       Total
    ##   Number of observations per group         
    ##   0                                                541         542
    ##   1                                                424         424
    ##   Number of missing patterns per group     
    ##   0                                                 15
    ##   1                                                  4
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    2633.442
    ##   Degrees of freedom                               752
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Chi-square for each group:
    ## 
    ##   0                                           1390.394
    ##   1                                           1243.048
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
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.153    0.091   12.645    0.000
    ##     bdiq4             1.489    0.109   13.643    0.000
    ##     bdiq5             0.937    0.081   11.504    0.000
    ##     bdiq6             1.346    0.100   13.510    0.000
    ##     bdiq7             1.230    0.094   13.075    0.000
    ##     bdiq8             1.577    0.120   13.109    0.000
    ##     bdiq10            1.222    0.105   11.633    0.000
    ##     bdiq12            1.237    0.094   13.194    0.000
    ##     bdiq13            1.291    0.093   13.876    0.000
    ##     bdiq14            1.232    0.088   14.014    0.000
    ##     bdiq15            1.285    0.094   13.677    0.000
    ##     bdiq17            1.017    0.088   11.541    0.000
    ##     bdiq20            1.302    0.100   13.001    0.000
    ##     bdiq21            1.254    0.123   10.175    0.000
    ##     bdiq22_1          0.058    0.020    2.870    0.004
    ##     bdiq23_1          0.041    0.013    3.190    0.001
    ##     bdiq24_1          0.018    0.008    2.301    0.021
    ##     bdiq25_1          0.014    0.006    2.176    0.030
    ##   trauf =~                                            
    ##     ctq1              1.000                           
    ##     ctq3              0.979    0.216    4.534    0.000
    ##     ctq4              1.393    0.288    4.841    0.000
    ##     ctq6              0.820    0.171    4.797    0.000
    ##     ctq8              1.099    0.221    4.965    0.000
    ##     ctq9              1.557    0.304    5.126    0.000
    ##     ctq11             1.570    0.290    5.418    0.000
    ##     ctq14             1.726    0.329    5.241    0.000
    ##     ctq20             1.459    0.291    5.017    0.000
    ##     ctq23             1.341    0.264    5.081    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf             0.319    0.091    3.529    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .bdiq2             0.359    0.030   12.022    0.000
    ##    .bdiq3             0.428    0.033   12.947    0.000
    ##    .bdiq4             0.618    0.038   16.424    0.000
    ##    .bdiq5             0.374    0.030   12.661    0.000
    ##    .bdiq6             0.405    0.035   11.733    0.000
    ##    .bdiq7             0.404    0.033   12.222    0.000
    ##    .bdiq8             0.577    0.042   13.674    0.000
    ##    .bdiq10            0.480    0.038   12.571    0.000
    ##    .bdiq12            0.482    0.033   14.663    0.000
    ##    .bdiq13            0.420    0.032   13.153    0.000
    ##    .bdiq14            0.336    0.030   11.085    0.000
    ##    .bdiq15            0.464    0.032   14.374    0.000
    ##    .bdiq17            0.444    0.032   13.844    0.000
    ##    .bdiq20            0.524    0.035   14.979    0.000
    ##    .bdiq21            0.638    0.046   13.927    0.000
    ##    .bdiq22_1          0.037    0.008    4.559    0.000
    ##    .bdiq23_1          0.015    0.005    2.852    0.004
    ##    .bdiq24_1          0.006    0.003    1.738    0.082
    ##    .bdiq25_1          0.004    0.003    1.418    0.156
    ##    .ctq1              1.590    0.043   36.978    0.000
    ##    .ctq3              1.396    0.038   36.694    0.000
    ##    .ctq4              1.408    0.043   32.775    0.000
    ##    .ctq6              1.160    0.028   41.889    0.000
    ##    .ctq8              1.201    0.031   39.052    0.000
    ##    .ctq9              1.263    0.039   32.146    0.000
    ##    .ctq11             1.240    0.035   35.426    0.000
    ##    .ctq14             1.351    0.041   33.252    0.000
    ##    .ctq20             1.205    0.035   34.394    0.000
    ##    .ctq23             1.188    0.032   37.393    0.000
    ##    .depf1             0.000                           
    ##     trauf             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .bdiq2             0.303    0.019   15.598    0.000
    ##    .bdiq3             0.351    0.023   15.481    0.000
    ##    .bdiq4             0.369    0.025   14.955    0.000
    ##    .bdiq5             0.314    0.020   15.721    0.000
    ##    .bdiq6             0.321    0.021   14.966    0.000
    ##    .bdiq7             0.318    0.021   15.203    0.000
    ##    .bdiq8             0.515    0.034   15.235    0.000
    ##    .bdiq10            0.520    0.033   15.714    0.000
    ##    .bdiq12            0.310    0.020   15.186    0.000
    ##    .bdiq13            0.251    0.017   14.753    0.000
    ##    .bdiq14            0.222    0.015   14.713    0.000
    ##    .bdiq15            0.266    0.018   14.827    0.000
    ##    .bdiq17            0.369    0.023   15.689    0.000
    ##    .bdiq20            0.357    0.024   15.190    0.000
    ##    .bdiq21            0.846    0.053   15.925    0.000
    ##    .bdiq22_1          0.035    0.002   16.362    0.000
    ##    .bdiq23_1          0.014    0.001   16.357    0.000
    ##    .bdiq24_1          0.006    0.000   16.356    0.000
    ##    .bdiq25_1          0.004    0.000   16.373    0.000
    ##    .ctq1              0.915    0.058   15.858    0.000
    ##    .ctq3              0.702    0.045   15.696    0.000
    ##    .ctq4              0.834    0.055   15.176    0.000
    ##    .ctq6              0.356    0.023   15.419    0.000
    ##    .ctq8              0.410    0.028   14.879    0.000
    ##    .ctq9              0.630    0.045   14.045    0.000
    ##    .ctq11             0.455    0.035   13.103    0.000
    ##    .ctq14             0.642    0.047   13.710    0.000
    ##    .ctq20             0.484    0.038   12.632    0.000
    ##    .ctq23             0.394    0.032   12.476    0.000
    ##    .depf1             0.170    0.023    7.539    0.000
    ##     trauf             0.084    0.029    2.925    0.003
    ## 
    ## 
    ## Group 2 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 =~                                            
    ##     bdiq2             1.000                           
    ##     bdiq3             1.064    0.100   10.657    0.000
    ##     bdiq4             1.039    0.098   10.653    0.000
    ##     bdiq5             0.894    0.085   10.534    0.000
    ##     bdiq6             1.279    0.128    9.959    0.000
    ##     bdiq7             0.936    0.092   10.218    0.000
    ##     bdiq8             1.362    0.125   10.919    0.000
    ##     bdiq10            1.225    0.141    8.670    0.000
    ##     bdiq12            1.039    0.103   10.067    0.000
    ##     bdiq13            1.066    0.109    9.818    0.000
    ##     bdiq14            0.979    0.094   10.429    0.000
    ##     bdiq15            0.712    0.092    7.713    0.000
    ##     bdiq17            0.750    0.097    7.750    0.000
    ##     bdiq20            0.576    0.093    6.212    0.000
    ##     bdiq21            0.621    0.115    5.393    0.000
    ##     bdiq22_1          0.296    0.044    6.736    0.000
    ##     bdiq23_1          0.276    0.039    7.024    0.000
    ##     bdiq24_1          0.209    0.037    5.656    0.000
    ##     bdiq25_1          0.225    0.038    5.903    0.000
    ##   trauf =~                                            
    ##     ctq1              1.000                           
    ##     ctq3              1.386    0.223    6.220    0.000
    ##     ctq4              0.721    0.137    5.254    0.000
    ##     ctq6              0.507    0.100    5.091    0.000
    ##     ctq8              1.382    0.215    6.425    0.000
    ##     ctq9              0.711    0.117    6.097    0.000
    ##     ctq11             0.902    0.148    6.097    0.000
    ##     ctq14             1.672    0.256    6.544    0.000
    ##     ctq20             0.657    0.122    5.380    0.000
    ##     ctq23             0.491    0.093    5.265    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   depf1 ~                                             
    ##     trauf             0.406    0.077    5.288    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
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
    ##    .bdiq20            0.877    0.036   24.234    0.000
    ##    .bdiq21            0.939    0.046   20.514    0.000
    ##    .bdiq22_1          0.144    0.017    8.449    0.000
    ##    .bdiq23_1          0.109    0.015    7.191    0.000
    ##    .bdiq24_1          0.099    0.015    6.834    0.000
    ##    .bdiq25_1          0.106    0.015    7.102    0.000
    ##    .ctq1              1.836    0.064   28.838    0.000
    ##    .ctq3              1.809    0.061   29.642    0.000
    ##    .ctq4              1.347    0.047   28.912    0.000
    ##    .ctq6              1.184    0.033   35.351    0.000
    ##    .ctq8              1.506    0.056   26.853    0.000
    ##    .ctq9              1.156    0.031   37.392    0.000
    ##    .ctq11             1.283    0.038   33.352    0.000
    ##    .ctq14             2.347    0.064   36.429    0.000
    ##    .ctq20             1.284    0.038   33.694    0.000
    ##    .ctq23             1.175    0.030   39.656    0.000
    ##    .depf1             0.000                           
    ##     trauf             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .bdiq2             0.321    0.024   13.361    0.000
    ##    .bdiq3             0.354    0.027   13.312    0.000
    ##    .bdiq4             0.326    0.025   13.300    0.000
    ##    .bdiq5             0.239    0.018   13.315    0.000
    ##    .bdiq6             0.617    0.046   13.566    0.000
    ##    .bdiq7             0.306    0.023   13.494    0.000
    ##    .bdiq8             0.477    0.036   13.110    0.000
    ##    .bdiq10            0.880    0.063   13.905    0.000
    ##    .bdiq12            0.370    0.027   13.454    0.000
    ##    .bdiq13            0.461    0.034   13.625    0.000
    ##    .bdiq14            0.310    0.023   13.379    0.000
    ##    .bdiq15            0.431    0.031   14.094    0.000
    ##    .bdiq17            0.462    0.033   14.081    0.000
    ##    .bdiq20            0.493    0.034   14.297    0.000
    ##    .bdiq21            0.814    0.057   14.383    0.000
    ##    .bdiq22_1          0.107    0.008   14.212    0.000
    ##    .bdiq23_1          0.082    0.006   14.177    0.000
    ##    .bdiq24_1          0.081    0.006   14.322    0.000
    ##    .bdiq25_1          0.085    0.006   14.317    0.000
    ##    .ctq1              1.469    0.106   13.852    0.000
    ##    .ctq3              1.105    0.087   12.729    0.000
    ##    .ctq4              0.792    0.057   13.847    0.000
    ##    .ctq6              0.412    0.030   13.915    0.000
    ##    .ctq8              0.861    0.070   12.295    0.000
    ##    .ctq9              0.280    0.023   12.418    0.000
    ##    .ctq11             0.427    0.035   12.311    0.000
    ##    .ctq14             1.070    0.091   11.784    0.000
    ##    .ctq20             0.508    0.038   13.442    0.000
    ##    .ctq23             0.312    0.023   13.514    0.000
    ##    .depf1             0.150    0.023    6.396    0.000
    ##     trauf             0.247    0.069    3.578    0.000

Logistic regression of severe depression on childhood trauma, adjusted for age
==============================================================================

``` r
drak_log = drak %>% 
  mutate(bdiq16 = as.numeric(str_sub(bdiq16, 0, 1)),
         bdiq18 = as.numeric(str_sub(bdiq18, 0, 1)),
         bdi = bdiq1 + bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq9 + bdiq10 + bdiq11 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq16 + bdiq17 + bdiq18 + bdiq19 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1,
         dep = bdi > 28,
         ctq = ctq3 + ctq4 + ctq6 + ctq8 + ctq9 + ctq11 + ctq14 + ctq20 + ctq23,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome)

mylogit = glm(dep ~ ctq + mother_age_at_enrolment, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ctq + mother_age_at_enrolment, family = "binomial", 
    ##     data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1661  -0.4088  -0.3623  -0.3472   2.3864  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -3.585685   0.646189  -5.549 2.87e-08 ***
    ## ctq                      0.085700   0.019519   4.391 1.13e-05 ***
    ## mother_age_at_enrolment  0.001392   0.021402   0.065    0.948    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 528.49  on 942  degrees of freedom
    ## Residual deviance: 511.71  on 940  degrees of freedom
    ##   (23 observations deleted due to missingness)
    ## AIC: 517.71
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -4.86547729 -2.32655698
    ## ctq                      0.04649509  0.12364862
    ## mother_age_at_enrolment -0.04143575  0.04266944

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ctq mother_age_at_enrolment 
    ##              0.02771768              1.08947891              1.00139277

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                 OR       2.5 %     97.5 %
    ## (Intercept)             0.02771768 0.007708148 0.09763132
    ## ctq                     1.08947891 1.047592941 1.13161818
    ## mother_age_at_enrolment 1.00139277 0.959410978 1.04359287

Logistic regression of severe depression on SES, adjusted for age
=================================================================

``` r
drak_log = drak %>% 
  mutate(bdiq16 = as.numeric(str_sub(bdiq16, 0, 1)),
         bdiq18 = as.numeric(str_sub(bdiq18, 0, 1)),
         bdi = bdiq1 + bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq9 + bdiq10 + bdiq11 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq16 + bdiq17 + bdiq18 + bdiq19 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1,
         dep = bdi > 32,
         ctq = ctq1 + ctq3 + ctq4 + ctq6 + ctq8 + ctq9 + ctq11 + ctq14 + ctq20 + ctq23,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome)

mylogit = glm(dep ~ ses + mother_age_at_enrolment, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + mother_age_at_enrolment, family = "binomial", 
    ##     data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5303  -0.3494  -0.2918  -0.2425   2.8066  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)             -0.50215    0.93853  -0.535  0.59263   
    ## ses                     -0.13700    0.04225  -3.243  0.00118 **
    ## mother_age_at_enrolment -0.02320    0.02729  -0.850  0.39536   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 367.50  on 941  degrees of freedom
    ## Residual deviance: 356.02  on 939  degrees of freedom
    ##   (24 observations deleted due to missingness)
    ## AIC: 362.02
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -2.34373248  1.34302966
    ## ses                     -0.22143202 -0.05544592
    ## mother_age_at_enrolment -0.07842291  0.02896020

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses mother_age_at_enrolment 
    ##               0.6052303               0.8719692               0.9770681

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.6052303 0.09596877 3.8306315
    ## ses                     0.8719692 0.80137040 0.9460632
    ## mother_age_at_enrolment 0.9770681 0.92457333 1.0293836

Logistic regression of severe depression on SES, adjusted for CTQ and age
=========================================================================

``` r
drak_log = drak %>% 
  mutate(bdiq16 = as.numeric(str_sub(bdiq16, 0, 1)),
         bdiq18 = as.numeric(str_sub(bdiq18, 0, 1)),
         bdi = bdiq1 + bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq9 + bdiq10 + bdiq11 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq16 + bdiq17 + bdiq18 + bdiq19 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1,
         dep = bdi > 32,
         ctq2rev = 6 - ctq2,
         ctq5rev = 6 - ctq5,
         ctq7rev = 6 - ctq7,
         ctq10rev = 6 - ctq10,
         ctq13rev = 6 - ctq13,
         ctq16rev = 6 - ctq16,
         ctq19rev = 6 - ctq19,
         ctq22rev = 6 - ctq22,
         ctq26rev = 6 - ctq26,
         ctq28rev = 6 - ctq28,
         ctq = ctq1 + ctq2rev + ctq3 + ctq4 + ctq5rev + ctq6 + ctq7rev + ctq8 + ctq9 + ctq10rev + ctq11 + ctq12 + ctq13rev + ctq14 + ctq15 + ctq16rev + ctq17 + ctq18 + ctq19rev + ctq20 + ctq21 + ctq22rev + ctq23 + ctq24 + ctq25 + ctq26rev + ctq27 + ctq28rev,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome)

mylogit = glm(dep ~ ses + ctq + mother_age_at_enrolment, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + ctq + mother_age_at_enrolment, family = "binomial", 
    ##     data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8601  -0.3330  -0.2715  -0.2182   2.8725  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -2.408786   1.051899  -2.290 0.022025 *  
    ## ses                     -0.126882   0.042971  -2.953 0.003150 ** 
    ## ctq                      0.032620   0.008563   3.809 0.000139 ***
    ## mother_age_at_enrolment -0.013226   0.027889  -0.474 0.635314    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 359.96  on 925  degrees of freedom
    ## Residual deviance: 338.35  on 922  degrees of freedom
    ##   (40 observations deleted due to missingness)
    ## AIC: 346.35
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -4.48259339 -0.34842728
    ## ses                     -0.21270519 -0.04384147
    ## ctq                      0.01518097  0.04901602
    ## mother_age_at_enrolment -0.06956812  0.04017067

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                     ctq 
    ##               0.0899244               0.8808373               1.0331579 
    ## mother_age_at_enrolment 
    ##               0.9868606

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.0899244 0.01130406 0.7057972
    ## ses                     0.8808373 0.80839442 0.9571057
    ## ctq                     1.0331579 1.01529679 1.0502372
    ## mother_age_at_enrolment 0.9868606 0.93279659 1.0409884

Logistic regression of severe depression on SES, adjusted for emotional neglect and age
=======================================================================================

not meaningful

``` r
drak_log = drak %>% 
  mutate(bdiq16 = as.numeric(str_sub(bdiq16, 0, 1)),
         bdiq18 = as.numeric(str_sub(bdiq18, 0, 1)),
         bdi = bdiq1 + bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq9 + bdiq10 + bdiq11 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq16 + bdiq17 + bdiq18 + bdiq19 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1,
         dep = bdi > 32,
         ctq2rev = 6 - ctq2,
         ctq5rev = 6 - ctq5,
         ctq7rev = 6 - ctq7,
         ctq10rev = 6 - ctq10,
         ctq13rev = 6 - ctq13,
         ctq16rev = 6 - ctq16,
         ctq19rev = 6 - ctq19,
         ctq22rev = 6 - ctq22,
         ctq26rev = 6 - ctq26,
         ctq28rev = 6 - ctq28,
         emoneg = ctq13rev + ctq7rev + ctq5rev + ctq19rev + ctq28rev,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome)

mylogit = glm(dep ~ ses + emoneg + mother_age_at_enrolment, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + emoneg + mother_age_at_enrolment, family = "binomial", 
    ##     data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8156  -0.3454  -0.2770  -0.2192   2.9179  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -1.39412    0.98581  -1.414 0.157310    
    ## ses                     -0.13930    0.04227  -3.296 0.000982 ***
    ## emoneg                   0.10244    0.03139   3.264 0.001100 ** 
    ## mother_age_at_enrolment -0.02328    0.02762  -0.843 0.399383    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 366.79  on 934  degrees of freedom
    ## Residual deviance: 345.91  on 931  degrees of freedom
    ##   (31 observations deleted due to missingness)
    ## AIC: 353.91
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -3.33289087  0.53988609
    ## ses                     -0.22375107 -0.05763635
    ## emoneg                   0.03881422  0.16263937
    ## mother_age_at_enrolment -0.07918753  0.02949451

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                  emoneg 
    ##               0.2480523               0.8699660               1.1078738 
    ## mother_age_at_enrolment 
    ##               0.9769916

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.2480523 0.03568978 1.7158114
    ## ses                     0.8699660 0.79951413 0.9439932
    ## emoneg                  1.1078738 1.03957733 1.1766123
    ## mother_age_at_enrolment 0.9769916 0.92386666 1.0299338

Logistic regression of severe depression on SES, adjusted for emotional abuse and age
=====================================================================================

ok

``` r
drak_log = drak %>% 
  mutate(bdiq16 = as.numeric(str_sub(bdiq16, 0, 1)),
         bdiq18 = as.numeric(str_sub(bdiq18, 0, 1)),
         bdi = bdiq1 + bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq9 + bdiq10 + bdiq11 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq16 + bdiq17 + bdiq18 + bdiq19 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1,
         dep = bdi > 32,
         ctq2rev = 6 - ctq2,
         ctq5rev = 6 - ctq5,
         ctq7rev = 6 - ctq7,
         ctq10rev = 6 - ctq10,
         ctq13rev = 6 - ctq13,
         ctq16rev = 6 - ctq16,
         ctq19rev = 6 - ctq19,
         ctq22rev = 6 - ctq22,
         ctq26rev = 6 - ctq26,
         ctq28rev = 6 - ctq28,
         emoabu = ctq18 + ctq14 + ctq25 + ctq3 + ctq8,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome)

mylogit = glm(dep ~ ses + emoabu + mother_age_at_enrolment, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + emoabu + mother_age_at_enrolment, family = "binomial", 
    ##     data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1507  -0.3428  -0.2690  -0.2169   2.9113  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -1.875538   1.010735  -1.856 0.063508 .  
    ## ses                     -0.136622   0.043597  -3.134 0.001726 ** 
    ## emoabu                   0.114893   0.030350   3.786 0.000153 ***
    ## mother_age_at_enrolment -0.009307   0.027983  -0.333 0.739442    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 361.14  on 937  degrees of freedom
    ## Residual deviance: 339.50  on 934  degrees of freedom
    ##   (28 observations deleted due to missingness)
    ## AIC: 347.5
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -3.87028899  0.10146643
    ## ses                     -0.22387445 -0.05253424
    ## emoabu                   0.05315423  0.17303399
    ## mother_age_at_enrolment -0.06574858  0.04436538

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                  emoabu 
    ##               0.1532725               0.8722996               1.1217539 
    ## mother_age_at_enrolment 
    ##               0.9907361

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.1532725 0.02085234 1.1067928
    ## ses                     0.8722996 0.79941550 0.9488218
    ## emoabu                  1.1217539 1.05459228 1.1889065
    ## mother_age_at_enrolment 0.9907361 0.93636626 1.0453642

Logistic regression of severe depression on SES, adjusted for sexual abuse and age
==================================================================================

ok

``` r
drak_log = drak %>% 
  mutate(bdiq16 = as.numeric(str_sub(bdiq16, 0, 1)),
         bdiq18 = as.numeric(str_sub(bdiq18, 0, 1)),
         bdi = bdiq1 + bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq9 + bdiq10 + bdiq11 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq16 + bdiq17 + bdiq18 + bdiq19 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1,
         dep = bdi > 32,
         ctq2rev = 6 - ctq2,
         ctq5rev = 6 - ctq5,
         ctq7rev = 6 - ctq7,
         ctq10rev = 6 - ctq10,
         ctq13rev = 6 - ctq13,
         ctq16rev = 6 - ctq16,
         ctq19rev = 6 - ctq19,
         ctq22rev = 6 - ctq22,
         ctq26rev = 6 - ctq26,
         ctq28rev = 6 - ctq28,
         sexabu = ctq23 + ctq24 + ctq27 + ctq20 + ctq21,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome)

mylogit = glm(dep ~ ses + sexabu + mother_age_at_enrolment, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + sexabu + mother_age_at_enrolment, family = "binomial", 
    ##     data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.6929  -0.3505  -0.2853  -0.2378   2.8357  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)             -0.94156    0.97998  -0.961  0.33665   
    ## ses                     -0.13317    0.04247  -3.135  0.00172 **
    ## sexabu                   0.06829    0.03672   1.859  0.06297 . 
    ## mother_age_at_enrolment -0.02486    0.02763  -0.900  0.36833   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 367.30  on 939  degrees of freedom
    ## Residual deviance: 353.07  on 936  degrees of freedom
    ##   (26 observations deleted due to missingness)
    ## AIC: 361.07
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -2.87221698  0.97810081
    ## ses                     -0.21800200 -0.05113783
    ## sexabu                  -0.01221233  0.13465213
    ## mother_age_at_enrolment -0.08079139  0.02792243

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                  sexabu 
    ##               0.3900199               0.8753175               1.0706711 
    ## mother_age_at_enrolment 
    ##               0.9754477

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.3900199 0.05657337 2.6594007
    ## ses                     0.8753175 0.80412384 0.9501477
    ## sexabu                  1.0706711 0.98786194 1.1441387
    ## mother_age_at_enrolment 0.9754477 0.92238609 1.0283159

Logistic regression for severe depression on SES, adjusted for physical neglect and age
=======================================================================================

good!

``` r
drak_log = drak %>% 
  mutate(bdiq16 = as.numeric(str_sub(bdiq16, 0, 1)),
         bdiq18 = as.numeric(str_sub(bdiq18, 0, 1)),
         bdi = bdiq1 + bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq9 + bdiq10 + bdiq11 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq16 + bdiq17 + bdiq18 + bdiq19 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1,
         dep = bdi > 32,
         ctq2rev = 6 - ctq2,
         ctq5rev = 6 - ctq5,
         ctq7rev = 6 - ctq7,
         ctq10rev = 6 - ctq10,
         ctq13rev = 6 - ctq13,
         ctq16rev = 6 - ctq16,
         ctq19rev = 6 - ctq19,
         ctq22rev = 6 - ctq22,
         ctq26rev = 6 - ctq26,
         ctq28rev = 6 - ctq28,
         physneg = ctq2rev + ctq26rev + ctq6 + ctq1 + ctq4,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome)

mylogit = glm(dep ~ ses + physneg + mother_age_at_enrolment, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + physneg + mother_age_at_enrolment, 
    ##     family = "binomial", data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8564  -0.3385  -0.2729  -0.2250   2.8039  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)             -2.19901    1.05616  -2.082  0.03734 * 
    ## ses                     -0.11412    0.04303  -2.652  0.00800 **
    ## physneg                  0.14101    0.04395   3.208  0.00134 **
    ## mother_age_at_enrolment -0.01286    0.02767  -0.465  0.64218   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 360.95  on 935  degrees of freedom
    ## Residual deviance: 342.46  on 932  degrees of freedom
    ##   (30 observations deleted due to missingness)
    ## AIC: 350.46
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -4.27744393 -0.12910192
    ## ses                     -0.19997991 -0.03090531
    ## physneg                  0.05210576  0.22560465
    ## mother_age_at_enrolment -0.06879460  0.04008278

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                 physneg 
    ##               0.1109133               0.8921546               1.1514326 
    ## mother_age_at_enrolment 
    ##               0.9872250

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.1109133 0.01387809 0.8788844
    ## ses                     0.8921546 0.81874720 0.9695674
    ## physneg                 1.1514326 1.05348715 1.2530802
    ## mother_age_at_enrolment 0.9872250 0.93351841 1.0408969

Logistic regression of severe depression on SES, adjusted for physical abuse and age
====================================================================================

ok

``` r
drak_log = drak %>% 
  mutate(bdiq16 = as.numeric(str_sub(bdiq16, 0, 1)),
         bdiq18 = as.numeric(str_sub(bdiq18, 0, 1)),
         bdi = bdiq1 + bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq9 + bdiq10 + bdiq11 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq16 + bdiq17 + bdiq18 + bdiq19 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1,
         dep = bdi > 32,
         ctq2rev = 6 - ctq2,
         ctq5rev = 6 - ctq5,
         ctq7rev = 6 - ctq7,
         ctq10rev = 6 - ctq10,
         ctq13rev = 6 - ctq13,
         ctq16rev = 6 - ctq16,
         ctq19rev = 6 - ctq19,
         ctq22rev = 6 - ctq22,
         ctq26rev = 6 - ctq26,
         ctq28rev = 6 - ctq28,
         physabu = ctq12 + ctq17 + ctq9 + ctq15 + ctq11,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome)

mylogit = glm(dep ~ ses + physabu + mother_age_at_enrolment, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + physabu + mother_age_at_enrolment, 
    ##     family = "binomial", data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.7657  -0.3391  -0.2808  -0.2338   2.8530  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)             -1.32116    0.99494  -1.328  0.18422   
    ## ses                     -0.12871    0.04246  -3.031  0.00243 **
    ## physabu                  0.08365    0.03347   2.499  0.01245 * 
    ## mother_age_at_enrolment -0.02008    0.02729  -0.736  0.46201   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 367.10  on 937  degrees of freedom
    ## Residual deviance: 350.62  on 934  degrees of freedom
    ##   (28 observations deleted due to missingness)
    ## AIC: 358.62
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -3.27180945  0.63695679
    ## ses                     -0.21351677 -0.04669737
    ## physabu                  0.01264577  0.14551065
    ## mother_age_at_enrolment -0.07530988  0.03208572

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                 physabu 
    ##               0.2668267               0.8792304               1.0872487 
    ## mother_age_at_enrolment 
    ##               0.9801238

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.2668267 0.03793772 1.8907183
    ## ses                     0.8792304 0.80773862 0.9543762
    ## physabu                 1.0872487 1.01272607 1.1566301
    ## mother_age_at_enrolment 0.9801238 0.92745605 1.0326060

Logistic regression of severe depression on SES, adjusted for emotional abuse, sexual abuse, physical neglect, physical abuse, and age
======================================================================================================================================

``` r
drak_log = drak %>% 
  mutate(bdiq16 = as.numeric(str_sub(bdiq16, 0, 1)),
         bdiq18 = as.numeric(str_sub(bdiq18, 0, 1)),
         bdi = bdiq1 + bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq9 + bdiq10 + bdiq11 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq16 + bdiq17 + bdiq18 + bdiq19 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1,
         dep = bdi > 32,
         ctq2rev = 6 - ctq2,
         ctq5rev = 6 - ctq5,
         ctq7rev = 6 - ctq7,
         ctq10rev = 6 - ctq10,
         ctq13rev = 6 - ctq13,
         ctq16rev = 6 - ctq16,
         ctq19rev = 6 - ctq19,
         ctq22rev = 6 - ctq22,
         ctq26rev = 6 - ctq26,
         ctq28rev = 6 - ctq28,
         emoneg = ctq13rev + ctq7rev + ctq5rev + ctq19rev + ctq28rev,
         emoabu = ctq18 + ctq14 + ctq25 + ctq3 + ctq8,
         sexabu = ctq23 + ctq24 + ctq27 + ctq20 + ctq21,
         physneg = ctq2rev + ctq26rev + ctq6 + ctq1 + ctq4,
         physabu = ctq12 + ctq17 + ctq9 + ctq15 + ctq11,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome)

mylogit = glm(dep ~ ses + emoneg + emoabu + physneg + physabu + sexabu + mother_age_at_enrolment, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + emoneg + emoabu + physneg + physabu + 
    ##     sexabu + mother_age_at_enrolment, family = "binomial", data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9654  -0.3355  -0.2647  -0.2098   2.8819  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)             -2.385455   1.071009  -2.227   0.0259 * 
    ## ses                     -0.132287   0.044575  -2.968   0.0030 **
    ## emoneg                   0.028247   0.043755   0.646   0.5185   
    ## emoabu                   0.082756   0.043513   1.902   0.0572 . 
    ## physneg                  0.063264   0.059722   1.059   0.2895   
    ## physabu                 -0.008775   0.046308  -0.189   0.8497   
    ## sexabu                   0.008043   0.044110   0.182   0.8553   
    ## mother_age_at_enrolment -0.009222   0.028201  -0.327   0.7437   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 360.15  on 927  degrees of freedom
    ## Residual deviance: 335.73  on 920  degrees of freedom
    ##   (38 observations deleted due to missingness)
    ## AIC: 351.73
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                                2.5 %      97.5 %
    ## (Intercept)             -4.494798738 -0.28642269
    ## ses                     -0.221363737 -0.04618429
    ## emoneg                  -0.059219240  0.11290653
    ## emoabu                  -0.005079403  0.16645191
    ## physneg                 -0.055964501  0.17882789
    ## physabu                 -0.104382116  0.07826075
    ## sexabu                  -0.086765663  0.08879295
    ## mother_age_at_enrolment -0.066184947  0.04479562

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                  emoneg 
    ##               0.0920471               0.8760899               1.0286502 
    ##                  emoabu                 physneg                 physabu 
    ##               1.0862772               1.0653076               0.9912633 
    ##                  sexabu mother_age_at_enrolment 
    ##               1.0080759               0.9908202

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.0920471 0.01116693 0.7509451
    ## ses                     0.8760899 0.80142512 0.9548660
    ## emoneg                  1.0286502 0.94250011 1.1195273
    ## emoabu                  1.0862772 0.99493348 1.1811067
    ## physneg                 1.0653076 0.94557270 1.1958149
    ## physabu                 0.9912633 0.90088099 1.0814046
    ## sexabu                  1.0080759 0.91689193 1.0928544
    ## mother_age_at_enrolment 0.9908202 0.93595775 1.0458141
