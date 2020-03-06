# plot gecon geom
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

