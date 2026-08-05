#-----------------------------------------------------------------------
# FIGURE COMPARING PREDICTIONS BETWEEN MODELS BASED ON PSEUDO- AND ACTUAL DATA
#-----------------------------------------------------------------------

# Load libraries
library(ggplot2)
library(cowplot)
library(reshape2)
library(ggrastr)

# Tabulate settings
settings <- data.frame(setting = 1:3,
                       m = c(30, 50, 100),
                       uniform_cluster_size = c(100, 60, 30))
nsim <- 500

#---------------
# POISSON MODELS
#---------------
# Plot predictions
family = "poisson"
preds_plots <- lapply(1:nrow(settings), function(setting) {
  m <- settings$m[setting]
  uniform_cluster_size <- settings$uniform_cluster_size[setting]
  preds.ls <- lapply(1:nsim, function(iter) {
    load(sprintf(file.path("SIMULATION", "intermediate_results", family,
                           "preds", "preds_%04d_%04d_%04d.RData"), iter, m, uniform_cluster_size))
    poi.predictions
  })
  preds.df <- as.data.frame(do.call(rbind, preds.ls))
  df <- data.frame(
    true = rep(preds.df$true, 4),
    preds = c(preds.df$ps2, preds.df$ps3, preds.df$ps4, preds.df$sim),
    type = rep(c("ps2", "ps3", "ps4", "sim"), each = m * uniform_cluster_size * nsim)
  )
  ggplot(df, aes(x = true, y = preds, color = type)) +
    rasterise(geom_point(shape = 1, alpha = 1), dpi = 300) +
    geom_abline(intercept = 0, color = "red") +
    ggtitle(paste0("m = ", m, ", n = ", uniform_cluster_size)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16), # Title size
          
          # --- AXES TEXT ---
          axis.title.y = element_text(size = 14),            # Y-axis label size (e.g., "bias")
          axis.text.x = element_text(size = 12),             # X-axis tick labels (your beta/sigma expressions)
          axis.text.y = element_text(size = 12),             # Y-axis tick labels
          
          # --- LEGEND TEXT ---
          legend.title = element_text(size = 14),            # Legend title size (e.g., "dat")
          legend.text = element_text(size = 12))
})

# postscript(file.path("Figures", "outputs", "poisson", "fig_preds.eps"), onefile = F)
# plot_grid(plotlist = preds_plots, ncol = 1, byrow = F)
# dev.off()

pdf(file.path("Figures", "outputs", "poisson", "fig_preds.pdf"), onefile = F)
plot_grid(plotlist = preds_plots, ncol = 1, byrow = F)
dev.off()
