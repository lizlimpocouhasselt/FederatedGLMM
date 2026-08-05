#-------------------------------------------------------------------
# FIGURE 3: 95% CONFIDENCE INTERVALS
#-------------------------------------------------------------------

# Load packages
library(ggplot2)
library(cowplot)

# Call function
source(file.path(getwd(), "Figures", "scripts_and_functions", "fn_figure_confint.R"))

# Tabulate settings
settings <- data.frame(setting = 1:3,
                       m = c(30, 50, 100),
                       uniform_cluster_size = c(100, 60, 30))
nsim = 500
betas <- c('b0', 'b1', 'b2', 'b32', 'b33')


#---------------
# POISSON MODELS
#---------------

# Plot confidence intervals
family = "poisson"
betas <- if(family == "logit") betas else c(betas, 'b34', 'b35')
lookup_df <- data.frame(num = 1:8,
                        name = I(list(expression(sigma[u]),
                                      expression(beta[0]),
                                      expression(beta[1]),
                                      expression(beta[2]),
                                      expression(beta[32]),
                                      expression(beta[33]),
                                      expression(beta[34]),
                                      expression(beta[35]))),
                        true = c(0.4811, 2.285513, -0.302612, 0.087566, -0.959036, -0.812642, -0.809044, -0.794925))
ci_allpars <- lapply(1:nrow(lookup_df), function(parnum){
  ci_plots <- lapply(1:nrow(settings), function(setting){
    m <- settings$m[setting]
    uniform_cluster_size <- settings$uniform_cluster_size[setting]
    ci.df <- fn_confint(nsim, m, uniform_cluster_size, parnum, family)
    ci.df$iter <- rep(1:nsim, 4)
    parname <- lookup_df$name[match(parnum, lookup_df$num)]
    partrue <- lookup_df$true[match(parnum, lookup_df$num)]
    ggplot(ci.df, aes(x = iter, y = point)) + 
      ylab(parname[[1]]) +
      geom_line(aes(color = dat)) +
      ggtitle(paste0("m = ", m, "; n = ", uniform_cluster_size)) +
      theme(plot.title = element_text(hjust = 0.5, size = 16), # Title size
            
            # --- AXES TEXT ---
            axis.title.y = element_text(size = 14),            # Y-axis label size (e.g., "bias")
            axis.text.x = element_text(size = 12),             # X-axis tick labels (your beta/sigma expressions)
            axis.text.y = element_text(size = 12),             # Y-axis tick labels
            
            # --- LEGEND TEXT ---
            legend.title = element_text(size = 14),            # Legend title size (e.g., "dat")
            legend.text = element_text(size = 12)) +
      geom_ribbon(aes(ymin = LL, ymax = UL, fill = dat, color = dat), alpha = 0.1) +
      geom_hline(yintercept = partrue, col = "red") 
  })
  lims <- unlist(lapply(ci_plots, function(plot){
    layer_scales(plot)$y$range$range
  }))
  legend <- get_legend(ci_plots[[1]] + theme(legend.position = "right"))
  ci_plots <- lapply(ci_plots, function(plot) plot + ylim(range(lims)) +
                       theme(legend.position = "none"))
  pg.par <- plot_grid(plotlist = ci_plots, ncol = nrow(settings))
  plot_grid(pg.par, legend, rel_widths = c(1, 0.075))
})

# For clearer plots in the paper, we produced the plots in three (3) parts
postscript(file.path("Figures", "outputs", family, "fig_ci_all_1.eps"), onefile = F)
plot_grid(plotlist = ci_allpars[1:4], ncol = 1)
postscript(file.path("Figures", "outputs", family, "fig_ci_all_2.eps"), onefile = F)
plot_grid(plotlist = ci_allpars[5:8], ncol = 1)
dev.off()

