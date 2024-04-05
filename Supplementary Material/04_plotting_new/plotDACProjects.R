plotDACProjects <- function(data.dac, regions, fill.by, title, leg.label) {
  data.plot <- data.dac %>%
    filter(region %in% names(regions)) %>%
    revalue.levels(region = regions) %>%
    order.levels(region = regions) %>%
    group_by(across(all_of(c("year", fill.by)))) %>%
    summarise(cumcap.sum = sum(cumcap.sum))
  
  data.plot.1 <- data.plot %>%
    filter(year %in% seq(t.min, t.split))
  
 data.plot.2 <- data.plot %>%
  filter(year %in% seq(t.split, t.max))
  
 if (fill.by %in% c("region", "country")) {
       colors <- c("#4D7DBF", "#3395AB","#818F42", "orange", "pink", "magenta", "#8C8C8C")
 
    } else {
        colors <- c("#4D7DBF", "#3395AB","#818F42", "orange", "pink", "magenta", "#8C8C8C")
      
    }

 
  p1 <- ggplot() +
    geom_bar(
      data = data.plot.1,
      mapping = aes_string(x = "year", y = "cumcap.sum", fill = fill.by),
      stat = "identity",
      position = "stack",
      width = 0.9
    ) +
    scale_fill_manual(name = leg.label, values = colors) +
    #scale_fill_jama(name = leg.label) +
    scale_x_continuous(name = "Year", breaks = c(seq(t.min, t.split, 5), t.split)) +
    scale_y_continuous(
     trans = custom_trans(), 
      labels = scales::number_format(scale = 1e-6, accuracy = 1)
    ) +
    #scale_y_log10() +
    #scale_y_break(c(0, 1e6)) +
    ylab(expression("DAC Capacity [MtCO"[2]*"]")) + 
    theme(legend.position = c(0.06, 0.73), plot.margin = unit(c(5, 0, 12, 5), "pt"))
  
  p2 <- ggplot() +
    geom_bar(
      data = data.plot.2,
      mapping = aes_string(x = "year", y = "cumcap.sum", fill = fill.by),
      stat = "identity",
      position = "stack"
    ) +
    scale_fill_manual(name = NULL, values = colors) +
    scale_x_continuous(name = "Year", breaks = c(seq(t.split, t.max, 5), t.max)) +
    #scale_y_log10( 
    scale_y_continuous(
      trans = custom_trans(),  
      labels = scales::number_format(scale = 1e-6, accuracy = 1)
    ) +
    ylab(expression("DAC Capacity [MtCO"[2]*"]")) +
    theme(legend.position = "none", plot.margin = unit(c(5, 5, 12, 0), "pt"))
  
  p.title <- ggdraw() +
    draw_label(title, size = font.size, fontface = "bold", hjust = 1)
  
  p.row <- plot_grid(p1, 
                     p2, 
                     ncol = 2, 
                     rel_widths = c(1, 1))
                       
                       #c((t.split - t.min + 1)
                        #          , (t.max - t.split + 1)
                        #          ))
                                   
  
  p <- plot_grid(p.title, p.row, nrow = 2, rel_heights = c(0.05, 1))
  
  return(p)
}
