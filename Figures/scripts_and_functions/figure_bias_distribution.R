#-------------------------------------------------------------------
# FIGURE 2: BIAS DISTRIBUTION OF ESTIMATES
#-------------------------------------------------------------------

# Load packages
library(reshape2)
library(ggplot2)
library(cowplot)

# Call function/s
source(file.path(getwd(), "Figures", "scripts_and_functions", "fn_figure_bias_distribution.R"))
source(file.path(getwd(), "Figures", "scripts_and_functions", "fn_figure_relbias_distribution.R"))

# Tabulate settings
settings <- data.frame(setting = 1:3,
                       m = c(30, 50, 100),
                       uniform_cluster_size = c(100, 60, 30))
nsim <- 500
betas <- c('b0', 'b1', 'b2', 'b32', 'b33')

#---------------
# POISSON MODELS
#---------------
# Plot biases
family <- "poisson"
betas <- if(family == "logit") betas else c(betas, 'b34', 'b35')
bias_plots <- lapply(1:nrow(settings), function(setting){
  m <- settings$m[setting]
  uniform_cluster_size <- settings$uniform_cluster_size[setting]
  bias.df <- fn_bias(nsim, m, uniform_cluster_size, family)
  ggplot(bias.df, aes(x = factor(pars, levels = c(betas,
                                                  "sig.u")), 
                      y = bias, fill = dat)) + 
    geom_boxplot() +
    geom_hline(yintercept = 0, col = "red") +
    scale_x_discrete(labels = c(sapply(betas, function(name){
      subsc <- as.numeric(gsub("\\D", "", name))
      name = bquote(beta[.(subsc)])
    }),
                                "sig.u" = expression(sigma[u]))) +
    xlab("") +
    ggtitle(paste0("m = ", m, "; n = ", uniform_cluster_size)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16), # Title size
          
          # --- AXES TEXT ---
          axis.title.y = element_text(size = 14),            # Y-axis label size (e.g., "bias")
          axis.text.x = element_text(size = 12),             # X-axis tick labels (your beta/sigma expressions)
          axis.text.y = element_text(size = 12),             # Y-axis tick labels
          
          # --- LEGEND TEXT ---
          legend.title = element_text(size = 14),            # Legend title size (e.g., "dat")
          legend.text = element_text(size = 12)              # Legend item labels size)
    )
})
postscript(file.path("Figures", "outputs", family, "fig_bias.eps"), onefile = F)
plot_grid(plotlist = bias_plots, ncol = 1)
dev.off()

relbias_plots <- lapply(1:nrow(settings), function(setting){
  m <- settings$m[setting]
  uniform_cluster_size <- settings$uniform_cluster_size[setting]
  bias.df <- fn_relbias(nsim, m, uniform_cluster_size, family)
  ggplot(bias.df, aes(x = factor(pars, levels = c(betas,
                                                  "sig.u")), 
                      y = bias, fill = dat)) + 
    geom_boxplot() +
    geom_hline(yintercept = 0, col = "red") +
    scale_x_discrete(labels = c(sapply(betas, function(name){
      subsc <- as.numeric(gsub("\\D", "", name))
      name = bquote(beta[.(subsc)])
    }),
    "sig.u" = expression(sigma[u]))) +
    xlab("") + ylab("rel bias (%)") +
    ggtitle(paste0("m = ", m, "; n = ", uniform_cluster_size)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16), # Title size
          
          # --- AXES TEXT ---
          axis.title.y = element_text(size = 14),            # Y-axis label size (e.g., "bias")
          axis.text.x = element_text(size = 12),             # X-axis tick labels (your beta/sigma expressions)
          axis.text.y = element_text(size = 12),             # Y-axis tick labels
          
          # --- LEGEND TEXT ---
          legend.title = element_text(size = 14),            # Legend title size (e.g., "dat")
          legend.text = element_text(size = 12)              # Legend item labels size)
    )
})
postscript(file.path("Figures", "outputs", family, "fig_relbias.eps"), onefile = F)
plot_grid(plotlist = relbias_plots, ncol = 1)
dev.off()
