# plot box and whiskers ---------------------------------------------------

plt_data <- function(data){
  data[, 7:28] <- scale(data[, 7:28], center = TRUE, scale = TRUE)
  fmt_long_data <- pivot_longer(data, 
                                7:28, 
                                names_to = "Biomarkers",
                                values_to = "Observations")
  
  print(ggplot(fmt_long_data, aes(x=TB_status, y=log10(Observations))) +
          geom_boxplot() +
          facet_wrap("Biomarkers", ncol = 6) + 
          ylab("log10"))  
  return(data)
}


