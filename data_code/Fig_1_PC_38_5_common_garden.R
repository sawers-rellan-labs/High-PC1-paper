
stat_sign <- function(x){ 
  x %>%
    group_by(LOCATION) %>%
    rstatix::t_test(., PC_38_5_A ~ elevation) %>%
    rstatix::add_xy_position(x = "elevation") %>%
    rstatix::adjust_pvalue(method = "BH") %>%
    rstatix::add_significance()  %>%
    dplyr::mutate( p_raw = p, p = p.adj,
                   pval_fmt = format.pval(p.adj, digits = 4)) %>%
    dplyr::arrange(p) 
}

PC_38_5_A <-  "PC38:5"
stat_sign(hilo_lipids)
field <- c("Highland Field", "Lowland Field")
names(field) <-  levels(as.factor(hilo_lipids$LOCATION)) 

home <- c("Lowland", "Highland")
names(home) <-  levels(as.factor(hilo_lipids$elevation)) 


plot_common_garden <- function(x){
  ggplot(data = x, aes(x = elevation, y = PC_38_5_A, color = elevation) )  +
    ggbeeswarm::geom_quasirandom() +
    stat_summary(
      fun.data=mean_sdl,
      fun.args = list(mult = 2), 
      geom="pointrange", color = "black") +
    ggpubr::stat_pvalue_manual(
      stat_sign(x), label = "{pval_fmt}",bracket.nudge.y = 0.1,
    ) +
    scale_y_continuous(name = PC_38_5_A, 
                       # limits = c(-1, 2.5), 
                       # breaks = c(0:2),
                       expand = expansion(mult = c(0, .1))
    ) +
    scale_x_discrete( labels = home) +
    #  xlab("Landrace Home") +
    #ylab("log10(PCs/LPCs)") +
    facet_grid(LOCATION ~ continent,
               labeller = labeller(LOCATION = field)) +
    ggpubr::theme_classic2(base_size = 13) +
    theme( 
      axis.text.x = element_text(color = "black"),
      # axis.title.y = element_text(size= 14),
      axis.title.x=element_blank(),
      panel.border = element_blank(),
      #panel.spacing.y=unit(0, "lines"),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.text.y = element_text(angle=90, vjust = 3, hjust = 0.5), # <<<
      legend.position ="none"
    ) 
}  



add_continent <- function(x,y=0){
  origin = data.frame(
    LOCATION = c("PV","MT"),
    elevation = "Low",
    label = c(x,"")
  )
  geom_text(
    data = origin,
    mapping = aes( x = 1.5, y = y, label = label),
    color = "black",
    hjust = 0.5, vjust = -0.7, size = 13*0.35)
}  

fig  <- NULL
fig1 <- NULL
fig1A <- NULL

fig <- ggpubr::ggarrange( 
  
  hilo_lipids %>%
    dplyr::filter(continent == "Mex") %>%
    plot_common_garden() +
    add_continent("Mesoamerica", y = -Inf) +
    scale_color_manual(values=meso_pal) +
    theme(
      plot.margin=unit(c(0.01,0,0.01,0.1), "cm")
    ), 
  
  hilo_lipids %>%
    dplyr::filter(continent == "SA") %>%
    plot_common_garden() +
    add_continent("South America", y = -Inf) +
    scale_color_manual(values=south_pal) +
    theme(
      plot.margin=unit(c(0.01,0,0.01,0.1), "cm")
    ),
  nrow = 2
)

hilo_lipids %>%
  dplyr::filter(continent == "Mex") %>%
  dplyr::pull(PC_38_5_A) %>% hist()


fig1 <- ggpubr::ggarrange(fig,NULL, nrow = 2, heights =  c(10,1))

fig1A <- cowplot::ggdraw(fig1) +
  cowplot::draw_label("Landrace Home", x = 0.55, y = 0.075, size = 10)

pdf(file= "fig1A.pdf", width = 2.2,height =7)
fig1A
dev.off() 

