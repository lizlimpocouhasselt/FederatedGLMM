#-------------------------------------------------------------------
# FIGURE 4: 95% CONFIDENCE INTERVAL COVERAGE
#-------------------------------------------------------------------
# Load packages
library(ggplot2)
library(cowplot)

# Call function
source(file.path(getwd(), "Figures", "scripts_and_functions", "fn_figure_io.R"))
source(file.path(getwd(), "Figures", "scripts_and_functions", "fn_figure_coverage.R"))
fig_log("starting figure_coverage.R")

# Tabulate settings
settings <- data.frame(setting = 1:3,
                       m = c(30, 50, 100),
                       uniform_cluster_size = c(100, 60, 30))
settings$name <- factor(paste("m =", settings$m, 
                              ", n =", settings$uniform_cluster_size),
                        levels = paste("m =", settings$m, 
                                       ", n =", settings$uniform_cluster_size))
nsim = 500
betas <- c('b0', 'b1', 'b2', 'b32', 'b33')

# PDF is drawn at its printed size (full text width), so sizes are final pt
coverage_panel_theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 11),
  axis.title.y = element_text(size = 9, margin = margin(r = 3)),
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8),
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 9),
  plot.margin = margin(t = 3, r = 4, b = 3, l = 3)
)


#---------------
# POISSON MODELS
#---------------
# Tabulate true parameter values
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

# Compute coverage and plot them
local_root <- fig_pull_inputs(c(interval_estimates = "interval_estimate"), family, settings, nsim)
coverage.all <- lapply(1:nrow(lookup_df), function(parnum){
  fig_log("coverage for parameter %d/%d", parnum, nrow(lookup_df))
  coverage.ls <- lapply(1:nrow(settings), function(setting){
    m <- settings$m[setting]
    uniform_cluster_size <- settings$uniform_cluster_size[setting]
    fig_log("  setting %d/%d: m=%d n=%d", setting, nrow(settings), m, uniform_cluster_size)
    df <- data.frame(
      coverage = 100 *
        fn_coverage(nsim, m, uniform_cluster_size, parnum, lookup_df, family, local_root),
      setting = rep(paste("m =", m,", n =", uniform_cluster_size), 4))
    df$type = row.names(df); row.names(df) <- NULL
    print(df)
    df
  })
  coverage.df <- do.call(rbind, coverage.ls)
  parname <- lookup_df$name[match(parnum, lookup_df$num)]
  coverage.df$par <- rep(parname, nrow(coverage.df))
  ggplot(coverage.df, aes(x = factor(setting, 
                                     levels = settings$name), 
                          y = coverage, group = type)) +
    geom_line(aes(color = type)) + geom_point(aes(color = type)) + 
    ylab("coverage (%)") + xlab("") + ylim(85, 100) +
    scale_x_discrete(labels = function(x) sub(" , ", "\n", x)) +
    geom_hline(yintercept = 95, color = "red") +
    geom_hline(yintercept = c(93.05,96.95), color = "red", lty = 2) +
    ggtitle(parname[[1]]) + 
    coverage_panel_theme
})
legend <- get_legend(coverage.all[[1]] + theme(legend.position = "bottom"))
coverage.all <- lapply(coverage.all, function(plot) plot +
                         theme(legend.position = "none"))
  pg.par <- plot_grid(plotlist = coverage.all, ncol = 2, align = "hv", axis = "tblr")
fig_log("writing fig_coverage.pdf")
  pdf(fig_output_file("fig_coverage.pdf", family), width = 6.3, height = 7.5,
    paper = "special", onefile = F)
  print(plot_grid(pg.par, legend, ncol = 1, rel_heights = c(1, 0.04)))
dev.off()
fig_upload(fig_output_file("fig_coverage.pdf", family), family)
unlink(local_root, recursive = TRUE, force = TRUE)
fig_log("completed figure_coverage.R")
