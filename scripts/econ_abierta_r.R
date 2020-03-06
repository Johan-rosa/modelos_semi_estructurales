# ###################################################
# Modelo Semi-Estructural para una Economía Abierta
# Marzo, 2020
# f.ramirez@bancentral.gov.do
#####################################################

# Librerías para modelos DSGE
library(gEcon)
library(gEcon.estimation)

## Configurando el modelo
ea <- make_model("econ_abierta.gcn")

## Solucionando el modelo

### Encontrando el estado estacionario
ea <- steady_state(ea,calibration = FALSE)
get_ss_values(ea)

### Solucinando el modelo
ea <- solve_pert(model = ea, loglin=FALSE)
get_pert_solution(ea)



## Calibrando la distribución de los choques 
ea <- set_shock_distr_par(ea,
                          distr_par = list("sd(eta_ygap)" = 1,
                                           "sd(eta_pic)" = 1,
                                           "sd(eta_tpm)" = 1,
                                           "sd(eta_ybar)" = 1,
                                           "sd(eta_risk)" = 1,
                                           "sd(eta_pistar)" = 1,
                                           "sd(eta_istar)" = 1,
                                           "sd(eta_ystar)" = 1,
                                           "sd(eta_UIP)" = 1))

shock_info(model=ea, all=TRUE)



## Computando e imprimiendo las correlaciones
ea <- compute_model_stats(ea, ref_var = "ygap", n_leadlags = 5)
get_model_stats(model = ea, 
                basic_stats = TRUE, 
                corr = TRUE, 
                autocorr = TRUE, 
                var_dec = FALSE,
                to_tex = TRUE)



## Computando e imprimiendo las FIR
ea_irf <- compute_irf(ea, 
                      variables = c("ygap", "pic", "tpm" ),
                      sim_length = 12,
                      shock = "eta_istar")
plot_simulation(ea_irf)








