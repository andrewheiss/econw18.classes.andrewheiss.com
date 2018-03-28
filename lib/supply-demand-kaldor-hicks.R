surplus_graph <- function(demand_fun, supply_fun, supply_new) {
  equilibrium <- uniroot(function(x) supply_fun(x) - demand_fun(x), c(0, 45))$root
  equilibrium_new <- uniroot(function(x) supply_new(x) - demand_fun(x), c(0, 45))$root
  
  x_q_original <- seq(0, equilibrium, 0.1)
  x_q_new <- seq(0, equilibrium_new, 0.1)

  base_plot <- ggplot(mapping = aes(x = 0:45)) +
    geom_ribbon(aes(x = x_q_new, 
                    ymin = demand_fun(equilibrium_new), ymax = demand_fun(x_q_new)),
                alpha = 0.3, fill = nord_green) +
    geom_ribbon(aes(x = x_q_new, 
                    ymin = supply_new(x_q_new), ymax = supply_new(equilibrium_new)),
                alpha = 0.3, fill = nord_lt_blue) +
    geom_ribbon(aes(x = x_q_original, 
                    ymin = demand_fun(equilibrium), ymax = demand_fun(x_q_original)),
                alpha = 0.3, fill = nord_green) +
    geom_ribbon(aes(x = x_q_original, 
                    ymin = supply(x_q_original), ymax = supply(equilibrium)),
                alpha = 0.3, fill = nord_lt_blue)
  
  full_plot <- base_plot +
    geom_segment(aes(x = equilibrium, xend = equilibrium, 
                     y = -Inf, yend = supply_fun(equilibrium)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    geom_segment(aes(x = -Inf, xend = equilibrium,
                     y = supply_fun(equilibrium), yend = supply_fun(equilibrium)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    geom_segment(aes(x = equilibrium_new, xend = equilibrium_new, 
                     y = -Inf, yend = supply_new(equilibrium_new)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    geom_segment(aes(x = -Inf, xend = equilibrium_new,
                     y = supply_new(equilibrium_new), yend = supply_new(equilibrium_new)),
                 color = "grey50", size = 0.5, linetype = "dashed") +
    stat_function(fun = supply_fun, size = 1.5, color = nord_red) +
    stat_function(fun = supply_new, size = 1.5, color = nord_orange) +
    stat_function(fun = demand_fun, size = 1.5, color = nord_dk_blue) +
    annotate(geom = "label", x = 38, y = supply_fun(38), label = "S", 
             size = 4, fill = nord_red, color = "white") +
    annotate(geom = "label", x = 38, y = supply_new(38), label = "S[new]", 
             size = 4, fill = nord_orange, color = "white", parse = TRUE) +
    annotate(geom = "label", x = 38, y = demand_fun(38), label = "D", 
             size = 4, fill = nord_dk_blue, color = "white") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
    coord_cartesian(xlim = c(0, 45), ylim = c(0, 20)) +
    labs(x = "Product (Q)", y = "Price (P)") +
    theme_econ(13, axis_line = TRUE) +
    theme(panel.grid = element_blank())
  
  full_plot
}
