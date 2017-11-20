---
output:
  pdf_document: default
  html_document: default
---
## AOV table for trait responses to competition

ANOVAs were run as `trait ~ species+plot_type+species*plot_type`, where plot type was either `comp` for competition plots or `lambda` for lambda plots. All three traits were logged to normalize.



### SLA

```
Analysis of Variance Table

Response: log_sla_cm2_g
Df  Sum Sq  Mean Sq F value    Pr(>F)    
species            12 1.40600 0.117166 23.6190 < 2.2e-16 ***
  plot_type           1 0.11712 0.117121 23.6098 2.689e-06 ***
  species:plot_type  12 0.14504 0.012086  2.4364  0.006037 ** 
  Residuals         168 0.83340 0.004961                      
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

### LDMC
```
Analysis of Variance Table

Response: log_ldmc_mg_g
Df  Sum Sq  Mean Sq F value    Pr(>F)    
species            12 1.40891 0.117409 21.8165 < 2.2e-16 ***
  plot_type           1 0.00211 0.002105  0.3912  0.532522    
species:plot_type  12 0.17680 0.014734  2.7378  0.002054 ** 
  Residuals         168 0.90412 0.005382                      
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```


### Leaf Size

```
Analysis of Variance Table

Response: log_leaf_area_cm2
Df  Sum Sq Mean Sq F value    Pr(>F)    
species            12 20.0379 1.66982 42.7008 < 2.2e-16 ***
  plot_type           1  0.2459 0.24587  6.2874   0.01311 *  
  species:plot_type  12  3.2068 0.26724  6.8338 5.624e-10 ***
  Residuals         168  6.5697 0.03911                      
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```