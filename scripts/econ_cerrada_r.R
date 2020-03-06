# ###################################################
# Modelo Semi-Estructural para una Economía Cerrada
# Marzo, 2020
# f.ramirez@bancentral.gov.do
#####################################################

# Librerías para modelos DSGE
library(gEcon)
library(gEcon.estimation)

## Configurando el modelo
ec <- make_model("gcn_file/econ_cerrada.gcn")

## Solucionando el modelo

### Encontrando el estado estacionario
ec <- steady_state(ec)
#get_ss_values(ec)

### Solucinando el modelo
ec <- solve_pert(model = ec, loglin=FALSE)
#get_pert_solution(ec)



## Calibrando la distribución de los choques 
ec <- set_shock_distr_par(
  ec,
  distr_par = list("sd(eta_ygap)" = 1,
                   "sd(eta_pic)" = 1,
                  "sd(eta_tpm)" = 1,
                  "sd(eta_ybar)" = 1)
  )

shock_info(model=ec, all=TRUE)



## Computando e imprimiendo las correlaciones
ec <- compute_model_stats(ec, ref_var = "ygap", n_leadlags = 5)

get_model_stats(model = ec, 
                basic_stats = TRUE, 
                corr = TRUE, 
                autocorr = TRUE, 
                var_dec = FALSE,
                to_tex = TRUE)



## Computando e imprimiendo las FIR
ec_irf <- compute_irf(ec, 
                       variables = c("ygap", "pic", "tpm"),
                      sim_length = 12,
                      shock = "eta_pic")

plot_simulation(ec_irf)

ggplot_gecon_simulation(ec_irf, facets = F)

