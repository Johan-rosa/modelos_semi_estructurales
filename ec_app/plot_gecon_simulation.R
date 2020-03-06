# Función para graficar las funciones impulso respuesta
ggplot_gecon_simulation <- function(gecon_simulation, facets = FALSE) {

  p <- gecon_simulation@sim %>% 
    as.data.frame() %>%
    rownames_to_column("variable") %>% 
    gather("periodo", "desviaciones", -variable) %>% 
    mutate(periodo = parse_number(periodo)) %>% 
    ggplot(aes(x = periodo, y = desviaciones, color = variable)) +
    geom_line() +
    theme_light() +
    theme(
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = seq(2, 12, 2)) +
    labs(
      y = "Desviación respecto al estado estacionario"
    ) +
    geom_hline(yintercept = 0, linetype = 2)
  
  if(facets) {
    p <- p + facet_wrap(~variable, ncol = 3, scales = "free")  
    
    
  }
    
  
  return(p)
  
}

# Especificación del modelo para economía cerrada
ec <- make_model("econ_cerrada.gcn") %>% 
  # Encontrando el estado estacionario
  steady_state() %>% 
  # Solucinando el modelo
  solve_pert(loglin=FALSE) %>% 
  ## Calibrando la distribución de los choques 
  set_shock_distr_par(
    distr_par = list(
      "sd(eta_ygap)" = 1,
      "sd(eta_pic)" = 1,
      "sd(eta_tpm)" = 1,
      "sd(eta_ybar)" = 1
    )
  )


# Modelo para economía abierta
ea <- make_model("econ_abierta.gcn") %>% 
  steady_state() %>% 
  solve_pert(loglin = FALSE) %>% 
  set_shock_distr_par(
    distr_par = list(
      "sd(eta_ygap)" = 1,
      "sd(eta_pic)" = 1,
      "sd(eta_tpm)" = 1,
      "sd(eta_ybar)" = 1,
      "sd(eta_risk)" = 1,
      "sd(eta_pistar)" = 1,
      "sd(eta_istar)" = 1,
      "sd(eta_ystar)" = 1,
      "sd(eta_UIP)" = 1) 
  )

choices_shocks_widget <- list(
  ec = c("Inflación" = "eta_pic", "Demanda agregada" = "eta_ygap", "TPM" = "eta_tpm"),
  
  ea = c("Inflación" = "eta_pic", "Demanda agregada" = "eta_ygap", "TPM" = "eta_tpm",
         "Premio por riesgo" = "eta_risk", "Inflación externa" = "eta_pistar",
         "Tasa de interés externa" = "eta_istar", "Demanda externa" = "eta_ystar")
)


# Data for simulation
#library(readxl)

#data_sim <- read_excel("datos.xlsx")




