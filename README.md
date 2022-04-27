
# campsismisc

## Forest plots

Assume the following estimated 2-compartment model:

``` r
library(campsismisc)
library(campsisnca)

model <- read.campsis("tests/testthat/campsis_models/metaboliser_effect_on_cl/")
model
```

    ## [MAIN]
    ## TVMETAB_CL=0
    ## if (METAB == 1) TVMETAB_CL=THETA_METAB_CL
    ## MU_1=THETA_DUR
    ## MU_2=THETA_VC
    ## MU_3=THETA_VP
    ## MU_4=THETA_Q
    ## MU_5=THETA_CL + TVMETAB_CL
    ## TVPROP_RUV=THETA_PROP_RUV
    ## DUR=exp(ETA_DUR + MU_1)
    ## VC=(1.0/70.0)*WT*exp(ETA_VC + MU_2)
    ## VP=(1.0/70.0)*WT*exp(ETA_VP + MU_3)
    ## Q=exp(ETA_Q + MU_4)
    ## CL=exp(ETA_CL + MU_5)
    ## PROP_RUV=exp(TVPROP_RUV)
    ## D1=DUR
    ## S1=VC
    ## 
    ## [ODE]
    ## d/dt(A_1)=-A_1*CL/VC - A_1*Q/VC + A_2*Q/VP
    ## d/dt(A_2)=A_1*Q/VC - A_2*Q/VP
    ## 
    ## [DURATION]
    ## A_1=D1
    ## 
    ## [ERROR]
    ## CONC=A_1/VC
    ## IPRED=-6.9077552789821368
    ## if (CONC > 0) IPRED=log(CONC)
    ## W=PROP_RUV
    ## Y=EPS_RSV_FIX*W + IPRED
    ## 
    ## 
    ## THETA's:
    ##       name index     value   fix
    ## 1      DUR     1  0.414183 FALSE
    ## 2       VC     2  4.432130 FALSE
    ## 3       VP     3  5.195360 FALSE
    ## 4        Q     4  1.404670 FALSE
    ## 5       CL     5  1.659260 FALSE
    ## 6 PROP_RUV     6 -1.954050 FALSE
    ## 7 METAB_CL     7  0.243992 FALSE
    ## OMEGA's:
    ##   name index index2       value   fix type same
    ## 1  DUR     1      1 1.54000e-02 FALSE  var   NA
    ## 2   VC     2      2 3.64502e-02 FALSE  var   NA
    ## 3   VP     3      3 3.35092e-15 FALSE  var   NA
    ## 4    Q     4      4 9.28255e-13 FALSE  var   NA
    ## 5   CL     5      5 2.97127e-02 FALSE  var   NA
    ## SIGMA's:
    ##      name index index2 value  fix type
    ## 1 RSV_FIX     1      1     1 TRUE  var
    ## Variance-covariance matrix:
    ##                   THETA_DUR     THETA_VC     THETA_VP      THETA_Q     THETA_CL
    ## THETA_DUR       1.15533e-03  4.39562e-05  2.00213e-09  2.55695e-08 -3.71284e-05
    ## THETA_VC        4.39562e-05  1.58049e-03  1.25744e-10 -2.12204e-08 -9.45167e-05
    ## THETA_VP        2.00213e-09  1.25744e-10  5.98280e-14  3.08209e-13  9.52439e-10
    ## THETA_Q         2.55695e-08 -2.12204e-08  3.08209e-13  2.09537e-11 -2.37621e-10
    ## THETA_CL       -3.71284e-05 -9.45167e-05  9.52439e-10 -2.37621e-10  1.41147e-03
    ## THETA_PROP_RUV -5.29804e-06 -3.06956e-04 -1.41398e-09  2.95451e-09 -1.77338e-04
    ## THETA_METAB_CL -7.13244e-04 -3.28291e-04 -2.80954e-09 -6.01550e-08 -1.30986e-03
    ## OMEGA_DUR      -7.08500e-05  6.60053e-05 -3.05415e-10 -3.00269e-09  3.51509e-05
    ## OMEGA_VC        1.37273e-04 -1.48562e-04  5.15600e-10  3.35919e-09  3.62697e-05
    ## OMEGA_VP       -3.63665e-16 -1.90104e-16 -1.07701e-21  2.67788e-20 -8.51198e-17
    ## OMEGA_Q        -1.44348e-14  5.69714e-15  1.15957e-19  1.11679e-18  5.13919e-14
    ## OMEGA_CL       -4.60593e-05 -4.24932e-05 -6.93743e-11  5.02385e-09  2.83741e-05
    ##                THETA_PROP_RUV THETA_METAB_CL    OMEGA_DUR     OMEGA_VC
    ## THETA_DUR        -5.29804e-06   -7.13244e-04 -7.08500e-05  1.37273e-04
    ## THETA_VC         -3.06956e-04   -3.28291e-04  6.60053e-05 -1.48562e-04
    ## THETA_VP         -1.41398e-09   -2.80954e-09 -3.05415e-10  5.15600e-10
    ## THETA_Q           2.95451e-09   -6.01550e-08 -3.00269e-09  3.35919e-09
    ## THETA_CL         -1.77338e-04   -1.30986e-03  3.51509e-05  3.62697e-05
    ## THETA_PROP_RUV    1.80850e-03   -3.43405e-04 -1.52959e-04 -5.14237e-05
    ## THETA_METAB_CL   -3.43405e-04    5.27898e-03  6.64558e-05 -6.60521e-05
    ## OMEGA_DUR        -1.52959e-04    6.64558e-05  6.72394e-05 -8.48541e-06
    ## OMEGA_VC         -5.14237e-05   -6.60521e-05 -8.48541e-06  1.06403e-04
    ## OMEGA_VP          1.43246e-16    4.06874e-16  6.81008e-18 -5.77054e-17
    ## OMEGA_Q          -2.99290e-14   -5.30180e-14  1.37679e-14  1.25898e-15
    ## OMEGA_CL         -8.78519e-05    2.55358e-04  1.36557e-05  4.18007e-06
    ##                    OMEGA_VP      OMEGA_Q     OMEGA_CL
    ## THETA_DUR      -3.63665e-16 -1.44348e-14 -4.60593e-05
    ## THETA_VC       -1.90104e-16  5.69714e-15 -4.24932e-05
    ## THETA_VP       -1.07701e-21  1.15957e-19 -6.93743e-11
    ## THETA_Q         2.67788e-20  1.11679e-18  5.02385e-09
    ## THETA_CL       -8.51198e-17  5.13919e-14  2.83741e-05
    ## THETA_PROP_RUV  1.43246e-16 -2.99290e-14 -8.78519e-05
    ## THETA_METAB_CL  4.06874e-16 -5.30180e-14  2.55358e-04
    ## OMEGA_DUR       6.81008e-18  1.37679e-14  1.36557e-05
    ## OMEGA_VC       -5.77054e-17  1.25898e-15  4.18007e-06
    ## OMEGA_VP        6.55985e-28  9.05611e-27  8.30384e-17
    ## OMEGA_Q         9.05611e-27  2.95552e-23  1.15034e-14
    ## OMEGA_CL        8.30384e-17  1.15034e-14  7.76144e-05
    ## 
    ## Compartments:
    ## A_1 (CMT=1)
    ## A_2 (CMT=2)

This model contains:

-   a metabolism effect on the clearance: `THETA_METAB_CL`
-   allometric scaling on central and peripheral volumes with a fixed
    exponent of 1
-   a variance-covariance matrix

### Effect of metabolism effect and weight on a model parameter

Let’s show how the metabolism effect and the weight influence the
clearance. This can be achieved as follows:

``` r
fp <- ForestPlot(model=model, output=ModelParameterOutput("CL"), replicates=100) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))
  
fp <- fp %>% prepare()
fp %>% getForestPlot + 
  ggplot2::scale_y_continuous(breaks=c(0.7,0.8,1,1.25,1.4), limits=c(0.5, 1.5))
```

<img src="README_files/figure-gfm/fp_metabolism_effect_on_cl -1.png" style="display: block; margin: auto;" />

Now, let’s see what happens with the central volume. The configuration
is identical.

``` r
fp <- ForestPlot(model=model, output=ModelParameterOutput("VC"), replicates=100) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))
  
fp <- fp %>% prepare()
fp %>% getForestPlot + 
  ggplot2::scale_y_continuous(breaks=c(0.7,0.8,1,1.25,1.4), limits=c(0.5, 1.5))
```

<img src="README_files/figure-gfm/fp_metabolism_effect_on_vc -1.png" style="display: block; margin: auto;" />

### Effect of metabolism and weight on a PK metric

Let’s show how the metabolism effect and the weight influence AUC. For
that, we need to create first a dataset. Assume we are interested to
compute AUC on day 1 and day 7. We give the drug every day and we
observe on day 1 and day 7.

``` r
dataset <- Dataset(1) %>%
    add(Infusion(time=0, amount=1000, compartment=1, ii=24, addl=6)) %>%
    add(Observations(times=c(0:24, 144:168)))
```

Instead of creating a `ModelParameterOutput` object, we create a
`NcaMetricOutput` and we use any metric from `campsisnca`. To compute
AUC on day 1, we need to provide the appropriate filter function.

``` r
fp <- ForestPlot(model=model, dataset=dataset, 
                       output=NcaMetricOutput(Auc(variable="CONC"), filter=~timerange(.x, min=0, max=24)), replicates=100) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))
  
fp <- fp %>% prepare()
fp %>% getForestPlot() + 
  ggplot2::scale_y_continuous(breaks=c(0.7,0.8,1,1.25,1.4), limits=c(0.5, 1.5))
```

<img src="README_files/figure-gfm/fp_metabolism_effect_on_auc_d1 -1.png" style="display: block; margin: auto;" />

If we are interested to see the absolute change in AUC, argument
relative can be set to FALSE as follows.

``` r
fp %>% getForestPlot(relative=FALSE)
```

<img src="README_files/figure-gfm/fp_metabolism_effect_on_auc_d1_absolute -1.png" style="display: block; margin: auto;" />

To look at AUC on day 7, we proceed as follows:

``` r
fp <- ForestPlot(model=model, dataset=dataset, 
                       output=NcaMetricOutput(Auc(variable="CONC"), filter=~timerange(.x, min=144, max=168)), replicates=100) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))
  
fp <- fp %>% prepare()
fp %>% getForestPlot() + 
  ggplot2::scale_y_continuous(breaks=c(0.7,0.8,1,1.25,1.4), limits=c(0.5, 1.5))
```

<img src="README_files/figure-gfm/fp_metabolism_effect_on_auc_d7 -1.png" style="display: block; margin: auto;" />

Again, we can look at the absolute values.

``` r
fp %>% getForestPlot(relative=FALSE)
```

<img src="README_files/figure-gfm/fp_metabolism_effect_on_auc_d7_absolute -1.png" style="display: block; margin: auto;" />

Let’s produce one more plot with Cmax on day 1.

``` r
fp <- ForestPlot(model=model, dataset=dataset, 
                       output=NcaMetricOutput(campsisnca::Cmax(variable="CONC"), filter=~timerange(.x, min=0, max=24)), replicates=100) %>%
    add(CategoricalLabeledCovariate(name="METAB", default_value=0, label="Metaboliser", categories=c(Slow=0, Fast=1))) %>%
    add(LabeledCovariate(name="WT", default_value=70, label="Weight", unit="kg")) %>%
    add(ForestPlotItem(Covariate("METAB", 0))) %>%
    add(ForestPlotItem(Covariate("METAB", 1))) %>%
    add(ForestPlotItem(Covariate("WT", 60))) %>%
    add(ForestPlotItem(Covariate("WT", 80)))
  
fp <- fp %>% prepare()
fp %>% getForestPlot() + 
  ggplot2::scale_y_continuous(breaks=c(0.7,0.8,1,1.25,1.4), limits=c(0.5, 1.5))
```

<img src="README_files/figure-gfm/fp_metabolism_effect_on_cmax -1.png" style="display: block; margin: auto;" />

## OAT-method based sensitivity analysis

### Effect of model parameters on a PK metric

Assume the same 2-compartment model. Let’s see how the model parameters
influence AUC if they are multiplied by two. This can be achieved with a
one-at-a-time (OAT)-method-based sensitivity analysis. Most of the time,
these analyses are done without parameter uncertainty (replicates=1, by
default) and represented with tornado plots, as shown below. However if
parameter uncertainty is used, you can still call `getForestPlot()` on
this new type of object.

``` r
dataset <- Dataset(1) %>%
  add(Infusion(time=0, amount=1000, compartment=1)) %>%
  add(Observations(times=seq(0, 24, by=0.1))) %>%
  add(Covariate("METAB", 0)) %>%
  add(Covariate("WT", 70))

object <- SensitivityAnalysis(model=model, dataset=dataset,
                     output=NcaMetricOutput(campsisnca::Auc(variable="CONC"))) %>%
  add(SensitivityAnalysisItem(Change("DUR", up=2, down=2))) %>%
  add(SensitivityAnalysisItem(Change("VC", up=2, down=2))) %>%
  add(SensitivityAnalysisItem(Change("VP", up=2, down=2))) %>%
  add(SensitivityAnalysisItem(Change("Q", up=2, down=2))) %>%
  add(SensitivityAnalysisItem(Change("CL", up=2, down=2)))

object <- object %>% prepare()
object %>% getTornadoPlot()
```

<img src="README_files/figure-gfm/sensibility_analysis_auc_example -1.png" style="display: block; margin: auto;" />

Absolute values may also be shown:

``` r
object %>% getTornadoPlot(relative=FALSE)
```

<img src="README_files/figure-gfm/sensibility_analysis_auc_example_absolute -1.png" style="display: block; margin: auto;" />

Let’s do the same exercise on Cmax. Let’s also show how parameters can
be labelled.

``` r
dataset <- Dataset(1) %>%
  add(Infusion(time=0, amount=1000, compartment=1)) %>%
  add(Observations(times=seq(0, 24, by=0.1))) %>%
  add(Covariate("METAB", 0)) %>%
  add(Covariate("WT", 70))

object <- SensitivityAnalysis(model=model, dataset=dataset,
                     output=NcaMetricOutput(campsisnca::Cmax(variable="CONC"))) %>%
  add(LabeledParameters(c(DUR="Duration", VC="Central volume", VP="Peripheral volume", Q="Inter-compartmental\nclearance", CL="Clearance"))) %>%
  add(SensitivityAnalysisItem(Change("DUR", up=2, down=2))) %>%
  add(SensitivityAnalysisItem(Change("VC", up=2, down=2))) %>%
  add(SensitivityAnalysisItem(Change("VP", up=2, down=2))) %>%
  add(SensitivityAnalysisItem(Change("Q", up=2, down=2))) %>%
  add(SensitivityAnalysisItem(Change("CL", up=2.5, down=10, upDownAsFactor=FALSE))) # Test a clearance of 2.5 and 10

object <- object %>% prepare()
object %>% getTornadoPlot()
```

<img src="README_files/figure-gfm/sensibility_analysis_cmax_example -1.png" style="display: block; margin: auto;" />
