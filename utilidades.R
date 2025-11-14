# FUNCIONES PARA UTILIZAR


MapaCoroplético <- function(geoj,value,region_labels,legend_title){
  pal <- colorQuantile("YlOrRd", value, n = 9)
  p <-  geoj %>%
    leaflet() %>%  
    setView(lng = 25, lat = 22, zoom = 2)  %>% 
    addPolygons(
      fillColor = ~pal(value), 
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions( 
        weight = 2,
        color = rgb(0.2,0.2,0.2),
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = region_labels 
    ) %>% 
    addLegend("bottomleft", 
              pal = pal, 
              values = value,
              title = legend_title,
              labFormat = function(type, cuts, p) {
                n = length(cuts) 
                x = (cuts[-n] + cuts[-1])/2
                x=prettyNum(round(x,
                                  digits=max(5-nchar(as.character(round(max(na.omit(value))))),0)), 
                            big.mark = ","
                )
                as.character(x)
              },
              opacity = 1
    )
  return(p)
}



MapaCoroplético2 <- function(geoj,value,region_labels,legend_title){
  pal <- colorQuantile("YlOrRd", value, n = 9)
  p <-  geoj %>%
    leaflet() %>%  
    setView(lng = 10.7038, lat = 44, zoom = 4) %>% 
    addPolygons(
      fillColor = ~pal(value), 
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions( 
        weight = 2,
        color = rgb(0.2,0.2,0.2),
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = region_labels 
    ) %>% 
    addLegend("bottomleft", 
              pal = pal, 
              values = value,
              title = legend_title,
              labFormat = function(type, cuts, p) {
                n = length(cuts) 
                x = (cuts[-n] + cuts[-1])/2
                x=prettyNum(round(x,
                                  digits=max(5-nchar(as.character(round(max(na.omit(value))))),0)), 
                            big.mark = ","
                )
                as.character(x)
              },
              opacity = 1
    )
  return(p)
}



GraficoDinamicoArima95CI <- function(
    data, # tssible con con los datos originales
    date, # string con el nombre se la variable temporal en data
    value, # string con el nombre se la variable numérica en data
    prediccion, # objeto con la predicción usando ARIMA u otro modelo
    TITLE # título del gráfico 
){
  
  fc <- prediccion %>%
    mutate(lower = NA) %>% 
    mutate(upper = NA) 
  
  for(k in 1:nrow(fc)){
    fc$lower[k] <- fc$.mean[k] - 1.96*unlist(fc[[value]][k])[2]
    fc$upper[k] <- fc$.mean[k] + 1.96*unlist(fc[[value]][k])[2]
  }
  
  data2 <- data 
  x <- data2[[date]][1]
  if(!(is.numeric(x) && !inherits(x, "Date") && !inherits(x, "POSIXt"))){
    data2[[date]] <- as.Date(data2[[date]])
    fc[[date]] <- as.Date(fc[[date]])
  }
  
  
  
  p <- plot_ly() %>%
    add_lines(x=data2[[date]], y = data2[[value]], name = "Observación", line = list(color = "black")) %>%
    add_lines(x=fc[[date]], y = fc$.mean, name = "Predicción", line = list(color = "blue")) %>%
    add_ribbons(x = fc[[date]], ymin = fc$lower, ymax = fc$upper, 
                name = "95% CI", fillcolor = "rgba(0,0,150,0.2)", line = list(width = 0)) %>%
    layout(title = TITLE)
  
  return(p) 
}

