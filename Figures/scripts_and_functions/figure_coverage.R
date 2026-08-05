#-------------------------------------------------------------------
# FIGURE 4: 95% CONFIDENCE INTERVAL COVERAGE
#-------------------------------------------------------------------
# Load packages
library(ggplot2)
library(cowplot)

# Call function
source(file.path(getwd(), "Figures", "scripts_and_functions", "fn_figure_coverage.R"))

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
coverage.all <- lapply(1:nrow(lookup_df), function(parnum){
  coverage.ls <- lapply(1:nrow(settings), function(setting){
    m <- settings$m[setting]
    uniform_cluster_size <- settings$uniform_cluster_size[setting]
    df <- data.frame(
      coverage = 100 *
        fn_coverage(nsim, m, uniform_cluster_size, parnum, lookup_df, family),
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
    geom_hline(yintercept = 95, color = "red") +
    geom_hline(yintercept = c(93.05,96.95), color = "red", lty = 2) +
    ggtitle(parname[[1]]) + 
    theme(plot.title = element_text(hjust = 0.5, size = 16), # Title size
          
          # --- AXES TEXT ---
          axis.title.y = element_text(size = 14),            # Y-axis label size (e.g., "bias")
          axis.text.x = element_text(size = 12),             # X-axis tick labels (your beta/sigma expressions)
          axis.text.y = element_text(size = 12),             # Y-axis tick labels
          
          # --- LEGEND TEXT ---
          legend.title = element_text(size = 14),            # Legend title size (e.g., "dat")
          legend.text = element_text(size = 12))
})
legend <- get_legend(coverage.all[[1]] + theme(legend.position = "right"))
coverage.all <- lapply(coverage.all, function(plot) plot +
                         theme(legend.position = "none"))
pg.par <- plot_grid(plotlist = coverage.all, ncol = 2)
postscript(file.path("Figures", "outputs", family, "fig_coverage.eps"))
plot_grid(pg.par, legend, rel_widths = c(1, 0.075))
dev.off()
