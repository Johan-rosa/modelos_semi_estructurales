# Paquetes ----------------------------------------------
library(tidyverse)
library(gEcon)
library(gEcon.estimation)
library(readxl)
#library(timetk)
library(forecast)

# Creando el modelo 
ec <- make_model("gcn_file/econ_cerrada.gcn") %>% 
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

# Funciones impulso respuesta del modelo
ec_fir <- compute_irf(
  ec, 
  variables = c("ygap", "pic", "tpm"),
  sim_length = 12,
  shock = "eta_pic"
  )


# Función personal para hacer los gráficos
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

# data para estimación
data_sim <- read_excel("datos.xlsx") %>% 
  mutate(
    fecha = zoo::as.yearqtr(fecha)
  ) %>%
  mutate_at(
    vars(-fecha),
    ~.-mean(., na.rm = TRUE)
  )

# Objeto ts para estimacion
ts_sim <- data_sim %>%
  slice(-1) %>% 
  select(-fecha) %>% 
  as.matrix() %>% 
  ts( start = c(1996, 1), frequency = 4)

# distribution <- "inv_gamma"

# Distribuciones a priori 
ec_prior <- gecon_prior(
  prior_list = list(
    # brecha de producto
    list(
      par = "sd(eta_ygap)",
      type = "inv_gamma",
      mean = 1,
      sd = 1,
      lower_bound = 0.0001,
      upper_bound = Inf,
      initial = 1
    ),
    
    # Inflacion 
    list(
      par = "sd(eta_pic)",
      type = "inv_gamma",
      mean = 1,
      sd = 1,
      lower_bound = 0.0001,
      upper_bound = Inf,
      initial = 1
    ),
    
    # tpm
    list(
      par = "sd(eta_tpm)",
      type = "inv_gamma",
      mean = 1,
      sd = 1,
      lower_bound = 0.0001,
      upper_bound = Inf,
      initial = 1
    ),
    
    # ybar
    list(
      par = "sd(eta_ybar)",
      type = "inv_gamma",
      mean = 1,
      sd = 1,
      lower_bound = 0.0001,
      upper_bound = Inf,
      initial = 1
    )
  ),
  
  model = ec
)


plot_prior(ec_prior)

# Estimación bayesiana
ec_bm <-  bayesian_estimation(
  data_set = ts_sim,
  optim_options_list = list(solver = "csminwel"),
  mcmc_options_list = list(
    chain_length = 1000,
    burn = 200,
    cores = 2, chains = 2,
    scale = rep(0.5, 4)
    ),
  observables = c("ygap", "pic", "tpm", "gy"),
  model = ec,
  prior = ec_prior
  )

# distribuciones posteriores
plot_posterior(ec_bm)

get_estimated_par(ec_bm)

forecast_ec <-  forecast_posterior(
  ec_bm, ts_sim,
  c("ygap", "pic", "tpm", "gy"),
  c("ygap", "pic", "tpm", "gy"),
  h = 12)

plot_forecast(forecast_ec)






#

forecast_ec$data_set 

forecast_ec$data_set_var

forecast_ec$forecasts$pic %>%
  as.data.frame() %>% 
  summarise_all(
    .funs = list(
      mean = ~mean(.),
      up95 = ~quantile(., .95),
      up80 = ~quantile(., .80),
      low95 = ~quantile(., .05),
      low80 = ~quantile(., .2)
    )
  ) %>% 
  gather(key, value) %>% 
  separate(key, into = c("h", "stats")) %>% 
  spread(stats, value) %>% 
  mutate(h = parse_number(h)) %>%
  arrange(h)


# Forecast tidy data
tidy_forecast <- function(forecast_ec) {
  
  # Variables del set de datos
  variables <- forecast_ec$data_set_var
  
  # set de datos origianl
  ts_original <- forecast_ec$data_set 
  
  # Tidy data original
  tidy_data_original <- forecast_ec$data_set %>% 
    as.data.frame() %>%
    #rowid_to_column("h") %>% 
    mutate(h = zoo::index(ts_original),
           key = "data") %>% 
    gather(variable, mean, -h, -key) 
  
  # primer periodo del forecast
  start_h <-  max(tidy_data_original$h) + 0.25
  
  # Tidy forecast
  tidy_forecast <- forecast_ec$forecasts %>% 
    map(~ .x %>%
          as.data.frame() %>% 
          summarise_all(
            .funs = list(
              mean = ~mean(.),
              up95 = ~quantile(., .95),
              up80 = ~quantile(., .80),
              low95 = ~quantile(., .05),
              low80 = ~quantile(., .2)
            )
          ) %>% 
          gather(key, value) %>% 
          separate(key, into = c("h", "stats")) %>% 
          spread(stats, value) %>% 
          mutate(h = parse_number(h)) %>% 
          arrange(h) %>%
          mutate(h = seq(start_h, by = 0.25, length.out = length(h)))
        ) %>% 
    bind_rows(.id = "variable") %>%
    mutate(key = "forecast")
  
  # bind data y forecast
  full_data <- bind_rows(
    tidy_data_original,
    tidy_forecast
  ) %>% 
    mutate(
      h = zoo::as.yearqtr(h)
    )

  return(full_data)   
}


# tidy forecast modelo ec
tidy_forecast_ec <-  tidy_forecast(forecast_ec)



# Fan chart en ggplot2

tidy_forecast_ec %>%
  ggplot(aes(x = h, y = mean, linetype = key)) +
  geom_ribbon(aes(ymin = low95, ymax = up95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = low80, ymax = up80), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line(size = 1) +
  facet_wrap(~variable, scales = "free") +
  theme_light()


+
  labs(title = "Gasoline Price Forecasts",
       subtitle = "Forecasting multiple models with sweep: ARIMA, BATS, ETS",
       x = "", y = "Price") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_tq() +
  scale_color_tq()


# Funcion Posterios plot

posterior_desnity <- function(ec_bm) {
  
}


sim_parm <-  data.frame(
  parametros = ec_bm@est_parameters,
  n = ec_bm@total_n,
  posterior_mean = ec_bm@estimates,
  posterior_sd = ec_bm@stddev,
  prior_mean = ec_bm@prior@moments[,1],
  prior_sd = ec_bm@prior@moments[,2]
  
)

sim_parm$parametros %>% 
  map(~filter(sim_parm, parametros == .x)) %>% 
  setNames(sim_parm$parametros) %>% 
  map(~rnorm(n = .x$n, sd = .x$posterior_mean))








