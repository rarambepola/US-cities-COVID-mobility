# US-cities-COVID-mobility

Thi repository contains code and mock data for the analysis in "TITLE" (link). This data is provided to demonstrate how the models can be run and produces similar results to those presented in the paper. The four R scripts included can be run individually or run in the order suggested to first fit each model to the data and then plot the results. 






Note: In the model of mobility over summer the direction of the fitted covariate effects should be flipped to interpret these as described in the paper (large values meaning less travel). In more detail, the effect of covariates is implemented in the code as

![equation](https://latex.codecogs.com/svg.image?\log&space;q_{ijat}&space;=&space;\exp{\beta^TX_{ijt}&space;&plus;&space;\beta_a}&space;\sum_{k=1}^tc_k)

where positive values of beta represent high values of the covariate being assocaited with more travel. To be more consistent with the model of decreasing travel at the beginning of the pandemic, the fitted covariate values were multiplied by -1 (so a positive value means that a covariate is associated with less travel). This is reflected in the model as described in the paper, where  


![equation](https://latex.codecogs.com/svg.image?\log&space;q_{ijat}&space;=&space;-&space;\exp{\beta^TX_{ijt}&space;&plus;&space;\beta_a}&space;\sum_{k=1}^tc_k)
