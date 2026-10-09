#-------------------------------------------------------------------
# FIGURE 3: 95% CONFIDENCE INTERVALS
#-------------------------------------------------------------------

# Load packages
library(ggplot2)
library(cowplot)

# Call function
source(file.path(getwd(), "Figures", "scripts_and_functions", "fn_figure_io.R"))
source(file.path(getwd(), "Figures", "scripts_and_functions", "fn_figure_confint.R"))
fig_log("starting figure_confint.R")

# Tabulate settings
settings <- data.frame(setting = 1:3,
                       m = c(30, 50, 100),
                       uniform_cluster_size = c(100, 60, 30))
nsim = 500
betas <- c('b0', 'b1', 'b2', 'b32', 'b33')

# PDF is drawn at its printed size (full text width), so sizes are final pt
ci_panel_theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 9),
  axis.title.x = element_text(size = 9),
  axis.title.y = element_text(size = 11, margin = margin(r = 3)),
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8),
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 9),
  plot.margin = margin(t = 3, r = 4, b = 3, l = 3)
)


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
local_root <- fig_pull_inputs(c(point_estimates = "point_estimate",
                                interval_estimates = "interval_estimate"),
                              family, settings, nsim)
ci_allpars <- lapply(1:nrow(lookup_df), function(parnum){
  fig_log("confidence intervals for parameter %d/%d", parnum, nrow(lookup_df))
  ci_plots <- lapply(1:nrow(settings), function(setting){
    m <- settings$m[setting]
    uniform_cluster_size <- settings$uniform_cluster_size[setting]
    fig_log("  setting %d/%d: m=%d n=%d", setting, nrow(settings), m, uniform_cluster_size)
    ci.df <- fn_confint(nsim, m, uniform_cluster_size, parnum, family, local_root)
    ci.df$iter <- rep(1:nsim, 4)
    parname <- lookup_df$name[match(parnum, lookup_df$num)]
    partrue <- lookup_df$true[match(parnum, lookup_df$num)]
    ggplot(ci.df, aes(x = iter, y = point)) + 
      ylab(parname[[1]]) +
      geom_line(aes(color = dat)) +
      ggtitle(paste0("m = ", m, "; n = ", uniform_cluster_size)) +
      ci_panel_theme +
      geom_ribbon(aes(ymin = LL, ymax = UL, fill = dat, color = dat), alpha = 0.1) +
      geom_hline(yintercept = partrue, col = "red") 
  })
  lims <- unlist(lapply(ci_plots, function(plot){
    layer_scales(plot)$y$range$range
  }))
  if (parnum == 1) ci_legend <<- get_legend(ci_plots[[1]] + theme(legend.position = "bottom"))
  ci_plots <- lapply(ci_plots, function(plot) plot + ylim(range(lims)) +
                       theme(legend.position = "none"))
  plot_grid(plotlist = ci_plots, ncol = nrow(settings), align = "hv", axis = "tblr")
})
write_ci_pdf <- function(plots, file) {
  pdf(file, width = 6.3, height = 7.5, paper = "special", onefile = F)
  print(plot_grid(plot_grid(plotlist = plots, ncol = 1, align = "v", axis = "lr"),
                  ci_legend, ncol = 1, rel_heights = c(1, 0.04)))
  dev.off()
}

# For clearer plots in the paper, we produced the plots in two (2) parts
fig_log("writing fig_ci_all_1.pdf")
write_ci_pdf(ci_allpars[1:4], fig_output_file("fig_ci_all_1.pdf", family))
fig_upload(fig_output_file("fig_ci_all_1.pdf", family), family)
fig_log("writing fig_ci_all_2.pdf")
write_ci_pdf(ci_allpars[5:8], fig_output_file("fig_ci_all_2.pdf", family))
fig_upload(fig_output_file("fig_ci_all_2.pdf", family), family)
unlink(local_root, recursive = TRUE, force = TRUE)
fig_log("completed figure_confint.R")

