#-----------------------------------------------------------------------
# FIGURE COMPARING PREDICTIONS BETWEEN MODELS BASED ON PSEUDO- AND ACTUAL DATA
#-----------------------------------------------------------------------

# Load libraries
library(ggplot2)
library(cowplot)
library(reshape2)
library(ggrastr)
source(file.path(getwd(), "Figures", "scripts_and_functions", "fn_figure_io.R"))
fig_log("starting figure_preds.R")

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
fig_log("building prediction plots for family=%s (settings=%d, nsim=%d)", family, nrow(settings), nsim)
local_root <- fig_pull_inputs(c(preds = "preds"), family, settings, nsim)
preds_plots <- lapply(1:nrow(settings), function(setting) {
  m <- settings$m[setting]
  uniform_cluster_size <- settings$uniform_cluster_size[setting]
  fig_log("prediction plot %d/%d: m=%d n=%d", setting, nrow(settings), m, uniform_cluster_size)
  preds.ls <- lapply(1:nsim, function(iter) {
    if (iter %% 100 == 0 || iter == nsim) {
      fig_log("  loading preds iter %d/%d for m=%d n=%d", iter, nsim, m, uniform_cluster_size)
    }
    load(file.path(local_root, "preds",
                   sprintf("preds_%04d_%04d_%04d.RData", iter, m, uniform_cluster_size)))
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
    # PDF is drawn at its printed size (full text width), so sizes are final pt
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9),
          legend.position = "none")
})
preds_legend <- get_legend(preds_plots[[1]] +
                             guides(color = guide_legend(override.aes = list(size = 2))) +
                             theme(legend.position = "bottom"))

# pdf(fig_output_file("fig_preds.pdf"), onefile = F)
# print(plot_grid(plotlist = preds_plots, ncol = 1, byrow = F))
# dev.off()
# fig_upload(fig_output_file("fig_preds.pdf"), family)

fig_log("writing fig_preds.pdf")
pdf(fig_output_file("fig_preds.pdf", family), width = 6.3, height = 7,
    paper = "special", onefile = F)
print(plot_grid(plot_grid(plotlist = preds_plots, ncol = 1, align = "v"),
                preds_legend, ncol = 1, rel_heights = c(1, 0.05)))
dev.off()
fig_upload(fig_output_file("fig_preds.pdf", family), family)
unlink(local_root, recursive = TRUE, force = TRUE)
fig_log("completed figure_preds.R")
