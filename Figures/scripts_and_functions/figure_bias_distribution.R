#-------------------------------------------------------------------
# FIGURE 2: BIAS DISTRIBUTION OF ESTIMATES
#-------------------------------------------------------------------

# Load packages
library(reshape2)
library(ggplot2)
library(cowplot)

# Call function/s
source(file.path(getwd(), "Figures", "scripts_and_functions", "fn_figure_io.R"))
source(file.path(getwd(), "Figures", "scripts_and_functions", "fn_figure_bias_distribution.R"))
source(file.path(getwd(), "Figures", "scripts_and_functions", "fn_figure_relbias_distribution.R"))
fig_log("starting figure_bias_distribution.R")

# Tabulate settings
settings <- data.frame(setting = 1:3,
                       m = c(30, 50, 100),
                       uniform_cluster_size = c(100, 60, 30))
nsim <- 500
betas <- c('b0', 'b1', 'b2', 'b32', 'b33')

# PDF is drawn at its printed size (full text width), so sizes are final pt
bias_panel_theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 11),
  axis.title.y = element_text(size = 10),
  axis.text.x = element_text(size = 11),
  axis.text.y = element_text(size = 9),
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 9),
  legend.position = "none"
)

write_bias_pdf <- function(plots, file) {
  legend <- get_legend(plots[[1]] + theme(legend.position = "bottom"))
  pdf(file, width = 6.3, height = 7, paper = "special", onefile = F)
  print(plot_grid(plot_grid(plotlist = plots, ncol = 1, align = "v"),
                  legend, ncol = 1, rel_heights = c(1, 0.06)))
  dev.off()
}

#---------------
# POISSON MODELS
#---------------
# Plot biases
family <- "poisson"
betas <- if(family == "logit") betas else c(betas, 'b34', 'b35')
fig_log("building bias plots for family=%s (settings=%d, nsim=%d)", family, nrow(settings), nsim)
local_root <- fig_pull_inputs(c(point_estimates = "point_estimate"), family, settings, nsim)
bias_plots <- lapply(1:nrow(settings), function(setting){
  m <- settings$m[setting]
  uniform_cluster_size <- settings$uniform_cluster_size[setting]
  fig_log("bias plot %d/%d: m=%d n=%d", setting, nrow(settings), m, uniform_cluster_size)
  bias.df <- fn_bias(nsim, m, uniform_cluster_size, family, local_root)
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
    bias_panel_theme
})
fig_log("writing fig_bias.pdf")
write_bias_pdf(bias_plots, fig_output_file("fig_bias.pdf", family))
fig_upload(fig_output_file("fig_bias.pdf", family), family)

fig_log("building relative bias plots for family=%s", family)
relbias_plots <- lapply(1:nrow(settings), function(setting){
  m <- settings$m[setting]
  uniform_cluster_size <- settings$uniform_cluster_size[setting]
  fig_log("relative bias plot %d/%d: m=%d n=%d", setting, nrow(settings), m, uniform_cluster_size)
  bias.df <- fn_relbias(nsim, m, uniform_cluster_size, family, local_root)
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
    bias_panel_theme
})
fig_log("writing fig_relbias.pdf")
write_bias_pdf(relbias_plots, fig_output_file("fig_relbias.pdf", family))
fig_upload(fig_output_file("fig_relbias.pdf", family), family)
unlink(local_root, recursive = TRUE, force = TRUE)
fig_log("completed figure_bias_distribution.R")
