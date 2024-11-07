library(ggplot2)
library(cowplot)

morpho_plots <- plot_grid(plot_score, plot_mass, plot_headLen, plot_headWid, morph_legend,
                          nrow = 1, ncol = 5,
                          labels = c('A.', 'B.', 'C.', 'D.', ''),
                          label_size = 10)



cor_plots <- plot_grid(corplot_Age_Mass, corplot_Age_HLen, corplot_Age_HWid,
                       nrow = 1, ncol = 3,
                       labels = c('E.', 'F.', 'G.'),
                       label_size = 10)

Age_plots <- plot_grid(Age_plot,
                           nrow = 1, ncol = 1,
                           labels = 'H.',
                           label_size = 10)

AgeTree_plots <- plot_grid(Age_plot, Tree_plot,
                       nrow = 1, ncol = 2,
                       labels = c('H.', 'I.'),
                       label_size = 10)

morpho_plots_bodyWeight <- plot_grid(plot_score, plot_mass, morph_legend,
                          nrow = 1, ncol = 3,
                          labels = c('A.', 'B.', ''),
                          label_size = 10)



All_plot <- plot_grid(morpho_plots, cor_plots, Age_plots, 
          nrow = 3, ncol = 1,
          rel_heights = c(1,1,1.5),
          rel_widths = c(1,1,1.5))

?plot_grid

# Export plot
ggsave(filename = "./Output/All_plots.jpg", plot = All_plot, 
       height = 9, width = 7.5, dpi = 300)

ggsave(filename = "./Output/Score_weight.jpg", plot = morpho_plots_bodyWeight, 
       height = 3, width = 7.5, dpi = 300)
