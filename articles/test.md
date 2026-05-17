# test

    #> asaur::pharmacoSmoking
    #> Relative Efficiency: 1.14
    #>                                 term family estimate stderr pvalue     method
    #> 1  Surv(time = ttr, event = relapse)  PATED   -0.538   0.20 0.0071      PATED
    #> 2  Surv(time = ttr, event = relapse)  coxph   -0.605   0.21 0.0046   Standard
    #> 3                                age    glm    2.170   2.10 0.3011 Prognostic
    #> 4                       yearsSmoking    glm    1.963   2.08 0.3442 Prognostic
    #> 5                     longestNoSmoke    glm  116.806 191.48 0.5419 Prognostic
    #> 6              I(employment == "ft")    glm   -0.149   0.36 0.6809 Prognostic
    #> 7              I(employment == "pt")    glm    0.054   0.57 0.9241 Prognostic
    #> 8                 I(race == "black")    glm   -0.543   0.40 0.1700 Prognostic
    #> 9                             gender    glm    0.074   0.37 0.8432 Prognostic
    #> 10             I(race == "hispanic")    glm    0.051   0.73 0.9441 Prognostic
    #> 11                I(race == "white")    glm    0.467   0.37 0.2090 Prognostic
    #> 12                     priorAttempts    glm   15.514  16.28 0.3406 Prognostic
    #> 13        I(levelSmoking == "heavy")    glm    0.089   0.40 0.8224 Prognostic
    #>       corr
    #> 1       NA
    #> 2   1.0000
    #> 3  -0.2144
    #> 4  -0.1494
    #> 5  -0.1463
    #> 6  -0.1217
    #> 7   0.1133
    #> 8   0.0816
    #> 9  -0.0740
    #> 10 -0.0420
    #> 11 -0.0243
    #> 12  0.0187
    #> 13 -0.0067

    #> coin::glioma
    #> Relative Efficiency: 1.66
    #>                               term family estimate stderr  pvalue     method
    #> 1 Surv(time = time, event = event)  PATED   -1.423   0.35 5.6e-05      PATED
    #> 2 Surv(time = time, event = event)  coxph   -1.829   0.46 5.9e-05   Standard
    #> 3            I(histology == "GBM")    glm   -1.012   0.68 1.4e-01 Prognostic
    #> 4                              age    glm   -3.272   4.61 4.8e-01 Prognostic
    #> 5                              sex    glm    0.095   0.66 8.9e-01 Prognostic
    #>   corr
    #> 1   NA
    #> 2 1.00
    #> 3 0.59
    #> 4 0.33
    #> 5 0.13

    #> iBST::burn
    #> Relative Efficiency: 1.15
    #>                           term family estimate stderr pvalue     method   corr
    #> 1  Surv(time = T3, event = D3)  PATED   -0.582   0.27  0.033      PATED     NA
    #> 2  Surv(time = T3, event = D3)  coxph   -0.561   0.29  0.055   Standard  1.000
    #> 3                           Z3    glm    0.088   0.49  0.858 Prognostic  0.215
    #> 4                  I(Z11 == 3)    glm    0.405   0.65  0.532 Prognostic  0.195
    #> 5                           Z2    glm   -0.083   0.39  0.831 Prognostic -0.149
    #> 6                           Z6    glm    0.442   0.40  0.263 Prognostic  0.113
    #> 7                  I(Z11 == 1)    glm    0.541   0.73  0.456 Prognostic -0.104
    #> 8                           Z4    glm   -5.483   3.17  0.084 Prognostic  0.074
    #> 9                           Z7    glm    0.821   0.46  0.073 Prognostic  0.055
    #> 10                          Z9    glm    0.294   0.35  0.407 Prognostic -0.042
    #> 11                          Z8    glm   -0.256   0.33  0.437 Prognostic -0.035
    #> 12                          Z5    glm   -0.125   0.33  0.701 Prognostic  0.029
    #> 13                 I(Z11 == 2)    glm   -0.718   0.51  0.162 Prognostic  0.025
    #> 14                         Z10    glm   -0.448   0.36  0.209 Prognostic -0.013

    #> invGauss::d.oropha.rec
    #> Relative Efficiency: 1.06
    #>                                term family estimate stderr pvalue     method
    #> 1 Surv(time = time, event = status)  PATED  0.16718  0.166   0.31      PATED
    #> 2 Surv(time = time, event = status)  coxph  0.17374  0.171   0.31   Standard
    #> 3                            tstage    glm -0.03691  0.117   0.75 Prognostic
    #> 4                            nstage    glm  0.13222  0.170   0.44 Prognostic
    #> 5                       I(sex == 1)    glm  0.00065  0.061   0.99 Prognostic
    #> 6                               age    glm -0.37169  1.568   0.81 Prognostic
    #>    corr
    #> 1    NA
    #> 2 1.000
    #> 3 0.181
    #> 4 0.118
    #> 5 0.050
    #> 6 0.019

    #> JM::aids.id
    #> Relative Efficiency: 1.25
    #>                               term family estimate stderr pvalue     method
    #> 1 Surv(time = Time, event = death)  PATED   -0.247   0.13  0.059      PATED
    #> 2 Surv(time = Time, event = death)  coxph   -0.210   0.15  0.150   Standard
    #> 3                              CD4    glm   -0.213   0.44  0.624 Prognostic
    #> 4              I(prevOI == "AIDS")    glm    0.084   0.20  0.668 Prognostic
    #> 5          I(AZT == "intolerance")    glm   -0.080   0.19  0.676 Prognostic
    #> 6                           gender    glm   -0.016   0.31  0.959 Prognostic
    #>    corr
    #> 1    NA
    #> 2  1.00
    #> 3 -0.40
    #> 4  0.35
    #> 5 -0.23
    #> 6 -0.03

    #> mlr3proba::actg
    #> Relative Efficiency: 1.09
    #>                                term family estimate stderr pvalue     method
    #> 1  Surv(time = time, event = event)  PATED  -0.6755   0.21 0.0011      PATED
    #> 2  Surv(time = time, event = event)  coxph  -0.6844   0.22 0.0015   Standard
    #> 3                               cd4    glm   4.3155   4.13 0.2956 Prognostic
    #> 4                            strat2    glm  -0.0011   0.12 0.9930 Prognostic
    #> 5                   I(karnof == 70)    glm   0.1341   0.36 0.7089 Prognostic
    #> 6                   I(karnof == 80)    glm  -0.0460   0.16 0.7758 Prognostic
    #> 7                  I(karnof == 100)    glm  -0.0537   0.12 0.6655 Prognostic
    #> 8                               age    glm   0.0503   0.52 0.9228 Prognostic
    #> 9                   I(karnof == 90)    glm   0.0587   0.12 0.6194 Prognostic
    #> 10                   I(raceth == 2)    glm  -0.0183   0.13 0.8884 Prognostic
    #> 11                   I(ivdrug == 1)    glm   0.0328   0.16 0.8388 Prognostic
    #> 12                         priorzdv    glm   0.1439   1.72 0.9334 Prognostic
    #> 13                   I(raceth == 3)    glm  -0.0774   0.15 0.6168 Prognostic
    #> 14                         hemophil    glm  -0.4126   0.35 0.2387 Prognostic
    #> 15                   I(raceth == 1)    glm   0.0665   0.12 0.5731 Prognostic
    #> 16                              sex    glm   0.1517   0.16 0.3303 Prognostic
    #>       corr
    #> 1       NA
    #> 2   1.0000
    #> 3  -0.1939
    #> 4  -0.1825
    #> 5   0.1489
    #> 6   0.1200
    #> 7  -0.0961
    #> 8   0.0609
    #> 9  -0.0467
    #> 10 -0.0435
    #> 11  0.0398
    #> 12 -0.0396
    #> 13  0.0264
    #> 14 -0.0164
    #> 15  0.0048
    #> 16  0.0011

    #> joint.Cox::dataOvarian1
    #> Relative Efficiency: 1.1
    #>                                  term family estimate stderr pvalue     method
    #> 1 Surv(time = t.event, event = event)  PATED  -0.1651  0.077  0.033      PATED
    #> 2 Surv(time = t.event, event = event)  coxph  -0.1696  0.081  0.036   Standard
    #> 3                              CXCL12    glm  -0.0341  0.061  0.576 Prognostic
    #> 4                                PDPN    glm   0.0238  0.066  0.720 Prognostic
    #> 5                               TIMP2    glm   0.0381  0.061  0.535 Prognostic
    #> 6                               TEAD1    glm   0.0086  0.067  0.897 Prognostic
    #> 7                               NCOA3    glm  -0.0583  0.060  0.331 Prognostic
    #> 8                               YWHAB    glm   0.0114  0.055  0.837 Prognostic
    #>    corr
    #> 1    NA
    #> 2 1.000
    #> 3 0.202
    #> 4 0.194
    #> 5 0.192
    #> 6 0.188
    #> 7 0.154
    #> 8 0.088

    #> pec::Pbc3
    #> Relative Efficiency: 1.58
    #>                                term family estimate stderr pvalue     method
    #> 1  Surv(time = days, event = event)  PATED   -0.193   0.17   0.25      PATED
    #> 2  Surv(time = days, event = event)  coxph   -0.059   0.21   0.78   Standard
    #> 3                              bili    glm    6.219   7.22   0.39 Prognostic
    #> 4                     I(stage == 4)    glm    0.253   0.25   0.32 Prognostic
    #> 5                     I(stage == 1)    glm   -0.107   0.31   0.73 Prognostic
    #> 6                             asptr    glm    2.641   5.68   0.64 Prognostic
    #> 7                     I(stage == 2)    glm   -0.336   0.26   0.20 Prognostic
    #> 8                            weight    glm    0.391   1.11   0.72 Prognostic
    #> 9                               sex    glm    0.026   0.30   0.93 Prognostic
    #> 10                          gibleed    glm   -0.590   0.31   0.06 Prognostic
    #> 11                             crea    glm   -1.150   1.97   0.56 Prognostic
    #> 12                            alkph    glm  -12.043  80.43   0.88 Prognostic
    #> 13                              age    glm    0.142   1.06   0.89 Prognostic
    #> 14                    I(stage == 3)    glm    0.168   0.28   0.55 Prognostic
    #>       corr
    #> 1       NA
    #> 2   1.0000
    #> 3   0.4977
    #> 4   0.3681
    #> 5  -0.2463
    #> 6   0.2231
    #> 7  -0.1745
    #> 8  -0.1465
    #> 9   0.1432
    #> 10  0.1358
    #> 11 -0.1020
    #> 12  0.0986
    #> 13  0.0618
    #> 14 -0.0013

    #> pec::cost
    #> Relative Efficiency: 1.47
    #>                                 term family estimate stderr pvalue     method
    #> 1  Surv(time = time, event = status)  PATED -1.8e-01  0.079  0.024      PATED
    #> 2  Surv(time = time, event = status)  coxph -1.4e-01  0.095  0.140   Standard
    #> 3                                age    glm  6.7e-01  0.899  0.458 Prognostic
    #> 4                        strokeScore    glm -5.1e-01  1.009  0.613 Prognostic
    #> 5                          atrialFib    glm  3.3e-01  0.246  0.181 Prognostic
    #> 6                         prevStroke    glm -8.6e-02  0.207  0.679 Prognostic
    #> 7                         othDisease    glm -2.6e-02  0.230  0.909 Prognostic
    #> 8                                ihd    glm -2.1e-16  0.217  1.000 Prognostic
    #> 9                           diabetes    glm -1.6e-01  0.234  0.485 Prognostic
    #> 10                           alcohol    glm -3.2e-01  0.175  0.068 Prognostic
    #> 11                               sex    glm -1.1e-01  0.163  0.514 Prognostic
    #> 12                             smoke    glm -2.3e-01  0.164  0.163 Prognostic
    #> 13                            hypTen    glm  1.8e-01  0.173  0.300 Prognostic
    #> 14                           cholest    glm  2.7e-02  0.118  0.819 Prognostic
    #> 15                             hemor    glm  3.0e-01  0.449  0.507 Prognostic
    #>      corr
    #> 1      NA
    #> 2   1.000
    #> 3   0.417
    #> 4  -0.268
    #> 5   0.203
    #> 6   0.145
    #> 7   0.139
    #> 8   0.133
    #> 9   0.123
    #> 10 -0.122
    #> 11  0.097
    #> 12 -0.093
    #> 13  0.081
    #> 14 -0.055
    #> 15  0.026

    #> pec::GBSG2
    #> Relative Efficiency: 1.18
    #>                              term family estimate stderr pvalue     method
    #> 1 Surv(time = time, event = cens)  PATED    -0.33   0.11 0.0039      PATED
    #> 2 Surv(time = time, event = cens)  coxph    -0.36   0.12 0.0034   Standard
    #> 3                          pnodes    glm     0.19   0.43 0.6641 Prognostic
    #> 4                         progrec    glm    22.29  17.83 0.2113 Prognostic
    #> 5                           tsize    glm    -0.82   1.13 0.4693 Prognostic
    #> 6                I(tgrade == "I")    glm     0.24   0.24 0.3302 Prognostic
    #> 7              I(tgrade == "III")    glm    -0.28   0.19 0.1469 Prognostic
    #> 8               I(tgrade == "II")    glm     0.11   0.17 0.5288 Prognostic
    #>     corr
    #> 1     NA
    #> 2  1.000
    #> 3  0.326
    #> 4 -0.187
    #> 5  0.169
    #> 6 -0.159
    #> 7  0.123
    #> 8  0.006

    #> randomForestSRC::follic
    #> Relative Efficiency: 1.11
    #>                                term family estimate stderr pvalue     method
    #> 1 Surv(time = time, event = status)  PATED    -0.15   0.14   0.28      PATED
    #> 2 Surv(time = time, event = status)  coxph    -0.23   0.15   0.12   Standard
    #> 3                               age    glm    -2.37   1.47   0.11 Prognostic
    #> 4                               hgb    glm     0.84   1.52   0.58 Prognostic
    #>     corr
    #> 1     NA
    #> 2  1.000
    #> 3  0.311
    #> 4 -0.082
