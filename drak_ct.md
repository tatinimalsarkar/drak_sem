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

Plot with extended CTQ
======================

``` r
drak_plot2 = drak %>% 
  mutate(ctq2rev = 6 - ctq2,
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

Socioeconomic status and 2-factor childhood trauma
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

Socioeconomic status and 1-factor childhood trauma
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

Logistic regression of severe depression on childhood trauma, adjusted for age and clinic
=========================================================================================

``` r
drak_log = drak %>% 
  mutate(bdiq16 = as.numeric(str_sub(bdiq16, 0, 1)),
         bdiq18 = as.numeric(str_sub(bdiq18, 0, 1)),
         bdi = bdiq1 + bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq9 + bdiq10 + bdiq11 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq16 + bdiq17 + bdiq18 + bdiq19 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1,
         dep = bdi > 28,
         ctq = ctq3 + ctq4 + ctq6 + ctq8 + ctq9 + ctq11 + ctq14 + ctq20 + ctq23,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome)

mylogit = glm(dep ~ ctq + mother_age_at_enrolment + clinic, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ctq + mother_age_at_enrolment + clinic, family = "binomial", 
    ##     data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.2027  -0.4027  -0.3660  -0.3546   2.4081  
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -3.5250410  0.6599229  -5.342 9.21e-08 ***
    ## ctq                      0.0872381  0.0198458   4.396 1.10e-05 ***
    ## mother_age_at_enrolment  0.0002352  0.0215603   0.011    0.991    
    ## clinic                  -0.1091374  0.2491937  -0.438    0.661    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 528.49  on 942  degrees of freedom
    ## Residual deviance: 511.52  on 939  degrees of freedom
    ##   (23 observations deleted due to missingness)
    ## AIC: 519.52
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -4.83274040 -2.24030943
    ## ctq                      0.04746143  0.12594717
    ## mother_age_at_enrolment -0.04289117  0.04183511
    ## clinic                  -0.60289836  0.37729301

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ctq mother_age_at_enrolment 
    ##               0.0294506               1.0911564               1.0002353 
    ##                  clinic 
    ##               0.8966072

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR       2.5 %    97.5 %
    ## (Intercept)             0.0294506 0.007964665 0.1064256
    ## ctq                     1.0911564 1.048605751 1.1342222
    ## mother_age_at_enrolment 1.0002353 0.958015649 1.0427225
    ## clinic                  0.8966072 0.547223283 1.4583316

Logistic regression of severe depression on SES, adjusted for age and clinic
============================================================================

``` r
drak_log = drak %>% 
  mutate(bdiq16 = as.numeric(str_sub(bdiq16, 0, 1)),
         bdiq18 = as.numeric(str_sub(bdiq18, 0, 1)),
         bdi = bdiq1 + bdiq2 + bdiq3 + bdiq4 + bdiq5 + bdiq6 + bdiq7 + bdiq8 + bdiq9 + bdiq10 + bdiq11 + bdiq12 + bdiq13 + bdiq14 + bdiq15 + bdiq16 + bdiq17 + bdiq18 + bdiq19 + bdiq20 + bdiq21 + bdiq22_1 + bdiq23_1 + bdiq24_1 + bdiq25_1,
         dep = bdi > 32,
         ctq = ctq1 + ctq3 + ctq4 + ctq6 + ctq8 + ctq9 + ctq11 + ctq14 + ctq20 + ctq23,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome)

mylogit = glm(dep ~ ses + mother_age_at_enrolment + clinic, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + mother_age_at_enrolment + clinic, family = "binomial", 
    ##     data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.6130  -0.3491  -0.2928  -0.2351   2.8893  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -0.49015    0.94471  -0.519 0.603871    
    ## ses                     -0.15554    0.04570  -3.404 0.000665 ***
    ## mother_age_at_enrolment -0.02033    0.02751  -0.739 0.459921    
    ## clinic                   0.37123    0.33412   1.111 0.266542    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 367.50  on 941  degrees of freedom
    ## Residual deviance: 354.79  on 938  degrees of freedom
    ##   (24 observations deleted due to missingness)
    ## AIC: 362.79
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                              2.5 %      97.5 %
    ## (Intercept)             -2.3438899  1.36728873
    ## ses                     -0.2468696 -0.06739188
    ## mother_age_at_enrolment -0.0759232  0.03229948
    ## clinic                  -0.2908152  1.02556015

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses mother_age_at_enrolment 
    ##               0.6125322               0.8559524               0.9798773 
    ##                  clinic 
    ##               1.4495153

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.6125322 0.09595366 3.9246954
    ## ses                     0.8559524 0.78124252 0.9348288
    ## mother_age_at_enrolment 0.9798773 0.92688739 1.0328268
    ## clinic                  1.4495153 0.74765386 2.7886571

Logistic regression of severe depression on SES, adjusted for CTQ, age, and clinic
==================================================================================

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

mylogit = glm(dep ~ ses + ctq + mother_age_at_enrolment + clinic, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + ctq + mother_age_at_enrolment + clinic, 
    ##     family = "binomial", data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8875  -0.3340  -0.2691  -0.2174   2.8933  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -2.383408   1.057950  -2.253 0.024268 *  
    ## ses                     -0.131942   0.046068  -2.864 0.004182 ** 
    ## ctq                      0.032026   0.008811   3.635 0.000278 ***
    ## mother_age_at_enrolment -0.012342   0.028058  -0.440 0.660018    
    ## clinic                   0.106747   0.346907   0.308 0.758301    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 359.96  on 925  degrees of freedom
    ## Residual deviance: 338.26  on 921  degrees of freedom
    ##   (40 observations deleted due to missingness)
    ## AIC: 348.26
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -4.46632942 -0.30786805
    ## ses                     -0.22392417 -0.04289483
    ## ctq                      0.01402879  0.04883994
    ## mother_age_at_enrolment -0.06897201  0.04143220
    ## clinic                  -0.58088175  0.78563870

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                     ctq 
    ##              0.09223572              0.87639154              1.03254442 
    ## mother_age_at_enrolment                  clinic 
    ##              0.98773351              1.11265305

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                 OR      2.5 %    97.5 %
    ## (Intercept)             0.09223572 0.01148941 0.7350123
    ## ses                     0.87639154 0.79937575 0.9580121
    ## ctq                     1.03254442 1.01412765 1.0500523
    ## mother_age_at_enrolment 0.98773351 0.93335280 1.0423025
    ## clinic                  1.11265305 0.55940490 2.1938077

Logistic regression of severe depression on SES, adjusted for emotional neglect, age, and clinic
================================================================================================

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

mylogit = glm(dep ~ ses + emoneg + mother_age_at_enrolment + clinic, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + emoneg + mother_age_at_enrolment + 
    ##     clinic, family = "binomial", data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.7972  -0.3485  -0.2748  -0.2187   2.9581  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)             -1.35989    0.99071  -1.373  0.16986   
    ## ses                     -0.14953    0.04608  -3.245  0.00117 **
    ## emoneg                   0.09947    0.03198   3.110  0.00187 **
    ## mother_age_at_enrolment -0.02162    0.02780  -0.778  0.43684   
    ## clinic                   0.19622    0.34444   0.570  0.56890   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 366.79  on 934  degrees of freedom
    ## Residual deviance: 345.58  on 930  degrees of freedom
    ##   (31 observations deleted due to missingness)
    ## AIC: 355.58
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -3.30715334  0.58493440
    ## ses                     -0.24159703 -0.06058329
    ## emoneg                   0.03451929  0.16066987
    ## mother_age_at_enrolment -0.07783752  0.03154874
    ## clinic                  -0.48702635  0.86949498

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                  emoneg 
    ##               0.2566878               0.8611102               1.1045879 
    ## mother_age_at_enrolment                  clinic 
    ##               0.9786160               1.2167909

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.2566878 0.03662027 1.7948732
    ## ses                     0.8611102 0.78537260 0.9412154
    ## emoneg                  1.1045879 1.03512199 1.1742972
    ## mother_age_at_enrolment 0.9786160 0.92511473 1.0320517
    ## clinic                  1.2167909 0.61445084 2.3857057

Logistic regression of severe depression on SES, adjusted for emotional abuse, age, and clinic
==============================================================================================

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

mylogit = glm(dep ~ ses + emoabu + mother_age_at_enrolment + clinic, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + emoabu + mother_age_at_enrolment + 
    ##     clinic, family = "binomial", data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1583  -0.3414  -0.2689  -0.2169   2.9187  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -1.864665   1.015738  -1.836 0.066391 .  
    ## ses                     -0.138492   0.046511  -2.978 0.002905 ** 
    ## emoabu                   0.113710   0.032037   3.549 0.000386 ***
    ## mother_age_at_enrolment -0.009058   0.028074  -0.323 0.746966    
    ## clinic                   0.041352   0.356760   0.116 0.907723    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 361.14  on 937  degrees of freedom
    ## Residual deviance: 339.49  on 933  degrees of freedom
    ##   (28 observations deleted due to missingness)
    ## AIC: 349.49
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -3.86722343  0.12423702
    ## ses                     -0.23152277 -0.04875782
    ## emoabu                   0.04873403  0.17516447
    ## mother_age_at_enrolment -0.06564910  0.04482027
    ## clinic                  -0.66742964  0.73758559

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                  emoabu 
    ##               0.1549481               0.8706701               1.1204269 
    ## mother_age_at_enrolment                  clinic 
    ##               0.9909831               1.0422192

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.1549481 0.02091636 1.1322842
    ## ses                     0.8706701 0.79332463 0.9524118
    ## emoabu                  1.1204269 1.04994107 1.1914422
    ## mother_age_at_enrolment 0.9909831 0.93645941 1.0458399
    ## clinic                  1.0422192 0.51302555 2.0908812

Logistic regression of severe depression on SES, adjusted for sexual abuse, age, and clinic
===========================================================================================

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

mylogit = glm(dep ~ ses + sexabu + mother_age_at_enrolment + clinic, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + sexabu + mother_age_at_enrolment + 
    ##     clinic, family = "binomial", data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.7706  -0.3507  -0.2863  -0.2314   2.9068  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)             -0.92111    0.98630  -0.934  0.35035   
    ## ses                     -0.14944    0.04574  -3.268  0.00108 **
    ## sexabu                   0.06564    0.03705   1.772  0.07642 . 
    ## mother_age_at_enrolment -0.02217    0.02783  -0.797  0.42571   
    ## clinic                   0.33308    0.33423   0.997  0.31899   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 367.30  on 939  degrees of freedom
    ## Residual deviance: 352.08  on 935  degrees of freedom
    ##   (26 observations deleted due to missingness)
    ## AIC: 362.08
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -2.86382702  1.01153868
    ## ses                     -0.24083009 -0.06117082
    ## sexabu                  -0.01550223  0.13259704
    ## mother_age_at_enrolment -0.07845320  0.03105205
    ## clinic                  -0.32916500  0.98768505

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                  sexabu 
    ##               0.3980755               0.8611876               1.0678440 
    ## mother_age_at_enrolment                  clinic 
    ##               0.9780724               1.3952541

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.3980755 0.05705001 2.7498289
    ## ses                     0.8611876 0.78597516 0.9406625
    ## sexabu                  1.0678440 0.98461731 1.1417898
    ## mother_age_at_enrolment 0.9780724 0.92454532 1.0315392
    ## clinic                  1.3952541 0.71952428 2.6850116

Logistic regression for severe depression on SES, adjusted for physical neglect, age, and clinic
================================================================================================

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

mylogit = glm(dep ~ ses + physneg + mother_age_at_enrolment + clinic, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + physneg + mother_age_at_enrolment + 
    ##     clinic, family = "binomial", data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8783  -0.3428  -0.2690  -0.2212   2.8690  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)             -2.14566    1.06203  -2.020  0.04335 * 
    ## ses                     -0.12749    0.04666  -2.732  0.00629 **
    ## physneg                  0.13656    0.04440   3.075  0.00210 **
    ## mother_age_at_enrolment -0.01104    0.02783  -0.397  0.69153   
    ## clinic                   0.25998    0.34088   0.763  0.44566   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 360.95  on 935  degrees of freedom
    ## Residual deviance: 341.88  on 931  degrees of freedom
    ##   (30 observations deleted due to missingness)
    ## AIC: 351.88
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -4.23468229 -0.06338916
    ## ses                     -0.22062927 -0.03735256
    ## physneg                  0.04679739  0.22203594
    ## mother_age_at_enrolment -0.06725728  0.04225303
    ## clinic                  -0.41506034  0.92778736

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                 physneg 
    ##               0.1169905               0.8803007               1.1463236 
    ## mother_age_at_enrolment                  clinic 
    ##               0.9890177               1.2969064

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.1169905 0.01448441 0.9385781
    ## ses                     0.8803007 0.80201395 0.9633364
    ## physneg                 1.1463236 1.04790967 1.2486163
    ## mother_age_at_enrolment 0.9890177 0.93495462 1.0431584
    ## clinic                  1.2969064 0.66030043 2.5289074

Logistic regression of severe depression on SES, adjusted for physical abuse, age, and clinic
=============================================================================================

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

mylogit = glm(dep ~ ses + physabu + mother_age_at_enrolment + clinic, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + physabu + mother_age_at_enrolment + 
    ##     clinic, family = "binomial", data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.7542  -0.3432  -0.2803  -0.2302   2.9305  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)             -1.33349    1.00518  -1.327  0.18463   
    ## ses                     -0.14622    0.04556  -3.209  0.00133 **
    ## physabu                  0.08372    0.03361   2.491  0.01276 * 
    ## mother_age_at_enrolment -0.01673    0.02759  -0.607  0.54412   
    ## clinic                   0.36795    0.33395   1.102  0.27054   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 367.10  on 937  degrees of freedom
    ## Residual deviance: 349.41  on 933  degrees of freedom
    ##   (28 observations deleted due to missingness)
    ## AIC: 359.41
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -3.30491770  0.64438152
    ## ses                     -0.23726762 -0.05825695
    ## physabu                  0.01244714  0.14583076
    ## mother_age_at_enrolment -0.07246502  0.03607389
    ## clinic                  -0.29379248  1.02196160

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                 physabu 
    ##               0.2635558               0.8639668               1.0873198 
    ## mother_age_at_enrolment                  clinic 
    ##               0.9834060               1.4447665

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.2635558 0.03670223 1.9048086
    ## ses                     0.8639668 0.78878016 0.9434075
    ## physabu                 1.0873198 1.01252493 1.1570004
    ## mother_age_at_enrolment 0.9834060 0.93009828 1.0367324
    ## clinic                  1.4447665 0.74543117 2.7786400

Logistic regression of severe depression on SES, adjusted for emotional abuse, sexual abuse, physical neglect, physical abuse, age, and clinic
==============================================================================================================================================

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

mylogit = glm(dep ~ ses + emoabu + physneg + emoneg + sexabu + physabu + mother_age_at_enrolment + clinic, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ ses + emoabu + physneg + emoneg + sexabu + 
    ##     physabu + mother_age_at_enrolment + clinic, family = "binomial", 
    ##     data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9668  -0.3354  -0.2645  -0.2095   2.8844  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)             -2.383395   1.072522  -2.222  0.02627 * 
    ## ses                     -0.132898   0.046992  -2.828  0.00468 **
    ## emoabu                   0.082100   0.046340   1.772  0.07645 . 
    ## physneg                  0.063372   0.059769   1.060  0.28902   
    ## emoneg                   0.028084   0.043939   0.639  0.52272   
    ## sexabu                   0.008140   0.044172   0.184  0.85379   
    ## physabu                 -0.008369   0.047352  -0.177  0.85971   
    ## mother_age_at_enrolment -0.009140   0.028274  -0.323  0.74650   
    ## clinic                   0.015248   0.370311   0.041  0.96715   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 360.15  on 927  degrees of freedom
    ## Residual deviance: 335.73  on 919  degrees of freedom
    ##   (38 observations deleted due to missingness)
    ## AIC: 353.73
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -4.49444440 -0.27967546
    ## ses                     -0.22681701 -0.04214665
    ## emoabu                  -0.01096932  0.17155366
    ## physneg                 -0.05599251  0.17900118
    ## emoneg                  -0.05976735  0.11308291
    ## sexabu                  -0.08676849  0.08901769
    ## physabu                 -0.10577391  0.08099563
    ## mother_age_at_enrolment -0.06622379  0.04504342
    ## clinic                  -0.72337721  0.73500147

``` r
exp(coef(mylogit))
```

    ##             (Intercept)                     ses                  emoabu 
    ##              0.09223689              0.87555442              1.08556452 
    ##                 physneg                  emoneg                  sexabu 
    ##              1.06542276              1.02848202              1.00817365 
    ##                 physabu mother_age_at_enrolment                  clinic 
    ##              0.99166598              0.99090171              1.01536513

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                 OR      2.5 %    97.5 %
    ## (Intercept)             0.09223689 0.01117089 0.7560291
    ## ses                     0.87555442 0.79706663 0.9587292
    ## emoabu                  1.08556452 0.98909062 1.1871478
    ## physneg                 1.06542276 0.94554621 1.1960222
    ## emoneg                  1.02848202 0.94198366 1.1197248
    ## sexabu                  1.00817365 0.91688934 1.0931000
    ## physabu                 0.99166598 0.89962802 1.0843662
    ## mother_age_at_enrolment 0.99090171 0.93592139 1.0460733
    ## clinic                  1.01536513 0.48511117 2.0854851

Logistic regression of severe depression on household income, adjusted for age and clinic
=========================================================================================

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
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome,
         ses1 = ses <= 5,
         ses2 = ses > 5 & ses <= 10,
         ses3 = ses > 10 & ses <= 15,
         ses4 = ses > 15 & ses <= 20,
         ses5 = ses > 20 & ses <= 25,
         ses6 = ses > 25) %>% 
  gather(ses1:ses6, key = "sescat", value = "bin") %>% 
  filter(bin == TRUE) %>% 
  select(-bin)

mylogit = glm(dep ~ houseincome + mother_age_at_enrolment + clinic, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ houseincome + mother_age_at_enrolment + clinic, 
    ##     family = "binomial", data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.3933  -0.3659  -0.2983  -0.2811   2.7382  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)             -1.97095    0.78522  -2.510   0.0121 *
    ## houseincome             -0.48784    0.23899  -2.041   0.0412 *
    ## mother_age_at_enrolment -0.00820    0.02778  -0.295   0.7678  
    ## clinic                   0.08574    0.31333   0.274   0.7843  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 367.5  on 941  degrees of freedom
    ## Residual deviance: 362.4  on 938  degrees of freedom
    ##   (17 observations deleted due to missingness)
    ## AIC: 370.4
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -3.51924015 -0.43139830
    ## houseincome             -0.97817476 -0.04012870
    ## mother_age_at_enrolment -0.06440232  0.04486432
    ## clinic                  -0.53892630  0.69661214

``` r
exp(coef(mylogit))
```

    ##             (Intercept)             houseincome mother_age_at_enrolment 
    ##               0.1393240               0.6139534               0.9918334 
    ##                  clinic 
    ##               1.0895267

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                OR      2.5 %    97.5 %
    ## (Intercept)             0.1393240 0.02962193 0.6496001
    ## houseincome             0.6139534 0.37599676 0.9606658
    ## mother_age_at_enrolment 0.9918334 0.93762770 1.0458859
    ## clinic                  1.0895267 0.58337428 2.0069419

Logistic regression of severe depression on household income, adjusted for CTQ, age, and clinic
===============================================================================================

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
         ctq = ctq1 + ctq3 + ctq4 + ctq6 + ctq8 + ctq9 + ctq11 + ctq14 + ctq20 + ctq23,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome,
         ses1 = ses <= 5,
         ses2 = ses > 5 & ses <= 10,
         ses3 = ses > 10 & ses <= 15,
         ses4 = ses > 15 & ses <= 20,
         ses5 = ses > 20 & ses <= 25,
         ses6 = ses > 25) %>% 
  gather(ses1:ses6, key = "sescat", value = "bin") %>% 
  filter(bin == TRUE) %>% 
  select(-bin)

mylogit = glm(dep ~ houseincome + ctq + mother_age_at_enrolment + clinic, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ houseincome + ctq + mother_age_at_enrolment + 
    ##     clinic, family = "binomial", data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8939  -0.3326  -0.2872  -0.2468   2.7351  
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -3.2885790  0.8712723  -3.774 0.000160 ***
    ## houseincome             -0.4656489  0.2434619  -1.913 0.055797 .  
    ## ctq                      0.0758272  0.0226516   3.348 0.000815 ***
    ## mother_age_at_enrolment -0.0004694  0.0283617  -0.017 0.986796    
    ## clinic                  -0.0561129  0.3231841  -0.174 0.862160    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 360.95  on 935  degrees of freedom
    ## Residual deviance: 347.22  on 931  degrees of freedom
    ##   (23 observations deleted due to missingness)
    ## AIC: 357.22
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %       97.5 %
    ## (Intercept)             -5.01378695 -1.586996923
    ## houseincome             -0.96405657 -0.008259603
    ## ctq                      0.02893196  0.118753597
    ## mother_age_at_enrolment -0.05780582  0.053786015
    ## clinic                  -0.70024453  0.573947389

``` r
exp(coef(mylogit))
```

    ##             (Intercept)             houseincome                     ctq 
    ##              0.03730682              0.62772767              1.07877616 
    ## mother_age_at_enrolment                  clinic 
    ##              0.99953075              0.94543235

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                 OR       2.5 %    97.5 %
    ## (Intercept)             0.03730682 0.006645689 0.2045389
    ## houseincome             0.62772767 0.381342802 0.9917744
    ## ctq                     1.07877616 1.029354552 1.1260924
    ## mother_age_at_enrolment 0.99953075 0.943833205 1.0552588
    ## clinic                  0.94543235 0.496463891 1.7752609

Logistic regression of severe depression on household income, adjusted for physical neglect, age, and clinic
============================================================================================================

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
         ctq = ctq1 + ctq3 + ctq4 + ctq6 + ctq8 + ctq9 + ctq11 + ctq14 + ctq20 + ctq23,
         ses = housing + owner + sumses33 + school + job + aid + ownincome + houseincome,
         ses1 = ses <= 5,
         ses2 = ses > 5 & ses <= 10,
         ses3 = ses > 10 & ses <= 15,
         ses4 = ses > 15 & ses <= 20,
         ses5 = ses > 20 & ses <= 25,
         ses6 = ses > 25) %>% 
  gather(ses1:ses6, key = "sescat", value = "bin") %>% 
  filter(bin == TRUE) %>% 
  select(-bin)

mylogit = glm(dep ~ houseincome + physneg + mother_age_at_enrolment + clinic, data = drak_log, family = "binomial")

summary(mylogit)
```

    ## 
    ## Call:
    ## glm(formula = dep ~ houseincome + physneg + mother_age_at_enrolment + 
    ##     clinic, family = "binomial", data = drak_log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9949  -0.3363  -0.2918  -0.2391   2.7227  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -3.414707   0.890264  -3.836 0.000125 ***
    ## houseincome             -0.418520   0.241423  -1.734 0.082997 .  
    ## physneg                  0.147236   0.043751   3.365 0.000764 ***
    ## mother_age_at_enrolment -0.001188   0.028140  -0.042 0.966333    
    ## clinic                   0.025045   0.319517   0.078 0.937522    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 360.95  on 935  degrees of freedom
    ## Residual deviance: 346.39  on 931  degrees of freedom
    ##   (23 observations deleted due to missingness)
    ## AIC: 356.39
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                               2.5 %      97.5 %
    ## (Intercept)             -5.17906117 -1.67910337
    ## houseincome             -0.91337471  0.03432158
    ## physneg                  0.05890717  0.23165162
    ## mother_age_at_enrolment -0.05807025  0.05264070
    ## clinic                  -0.61137616  0.64847552

``` r
exp(coef(mylogit))
```

    ##             (Intercept)             houseincome                 physneg 
    ##              0.03288604              0.65802008              1.15862756 
    ## mother_age_at_enrolment                  clinic 
    ##              0.99881298              1.02536159

``` r
exp(cbind(OR = coef(mylogit), confint(mylogit)))
```

    ## Waiting for profiling to be done...

    ##                                 OR       2.5 %    97.5 %
    ## (Intercept)             0.03288604 0.005633293 0.1865412
    ## houseincome             0.65802008 0.401168112 1.0349174
    ## physneg                 1.15862756 1.060676776 1.2606805
    ## mother_age_at_enrolment 0.99881298 0.943583661 1.0540509
    ## clinic                  1.02536159 0.542603645 1.9126229
