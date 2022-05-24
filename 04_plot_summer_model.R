setwd(paste0(Sys.getenv("HOME"), "/US-cities-COVID-mobility/"))
rm(list=ls())

library(ggplot2)
library(patchwork)
load("city_names_df.RData")


####combine results from all cities in data frame####
df_comb_list <- list()
for(i in setdiff(1:27, 4)){
  
  city_name <- city_names_df$city_names[city_names_df$city_no == i]
  load(paste0("summer_model_fits/pars_city_",
              i, ".RData"))
  df_comb_list[[i]] <- data.frame(beta=c(pars[names(pars) == "beta_route"],
                                         pars[names(pars) == "beta_loc"],
                                         pars[names(pars) == "beta_age"]),
                                  city=city_name,
                                  city_no=i,
                                  type=c("<LQ", 
                                         "LQ-median",
                                         "median-UQ",
                                         ">UQ",
                                         "Subscriber",
                                         "ACS",
                                         "18-34",
                                         "35-54",
                                         "55+"),
                                  type_no=1:9)
}

city_groups <- list(c("Atlanta_GA", "Charlotte_NC", "Nashville_TN",
                      "Jacksonville_FL", "Tampa_FL", "Miami_FL"),
                    c("Baltimore_MD", "Philadelphia_PA", "NYC_NY",
                      "Boston_MA"),
                    c("Detroit_MI", "Columbus_OH", "Chicago_IL"),
                    c("SanJose_CA", "SanDiego_CA", "SanFrancisco_CA",
                      "Los_Angles_CA"),
                    c("Phoenix_AZ", "ElPaso_TX", "Dallas_TX",
                      "Austin_TX", "Houston_TX", "SanAntonio_TX"),
                    c("Fargo_ND", "SiouxFalls_SD", "Omaha_NE", "Lincoln_NE"))

city_group_names <- c("Southeast",
                      "Northeast",
                      "Great lakes",
                      "West coast",
                      "South",
                      "Midwest")

city_groups <- city_groups[order(city_group_names)]
city_group_names <- city_group_names[order(city_group_names)]


df_comb <- do.call("rbind", df_comb_list)
df_comb$beta <- - df_comb$beta

df_comb$city_group_no <- sapply(df_comb$city, 
                                function(city) which(sapply(city_groups, function(g) city %in% g)))
df_comb$city_group <- city_group_names[df_comb$city_group_no]


df_all <- df_comb

ylim1 <- -0.25
ylim2 <- 0.25

####make plots####
df_dist <- df_all[df_all$type_no < 5, ]
df_dist$type <- factor(df_dist$type, levels = rev(c(">UQ", "median-UQ", "LQ-median", "<LQ")),
                       ordered = TRUE)
p_dist <- ggplot(df_dist,
                  aes(x=city, y=beta, col=type)) +
  # scale_color_discrete(NULL) +
  scale_color_viridis_d(NULL) + 
  geom_hline(yintercept=0, linetype="dashed") + 
  geom_point() +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle=90),
        legend.position = "right") +
  xlab(NULL) +
  # ylab("Beta") +
  ylab(NULL) + 
  facet_grid(.~city_group, scales="free_x", space="free") +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(color="black")) +
  theme(axis.text.x = element_blank()) +
  ylim(ylim1, ylim2) +
  ggtitle(NULL, subtitle = "Distance") + 
  NULL 


p_income <- ggplot(df_all[df_all$type_no %in% 5, ],
                    aes(x=city, y=beta, col=type)) +
  scale_color_discrete(NULL) +
  geom_hline(yintercept=0, linetype="dashed") + 
  geom_point() +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle=90),
        legend.position = "right") +
  xlab(NULL) + 
  ylab(NULL) +
  facet_grid(.~city_group, scales="free_x", space="free") +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank()
  ) +
  theme(axis.text.x = element_blank()) + 
  ylim(ylim1, ylim2) +
  ggtitle(NULL, subtitle = "Subscriber income") + 
  theme(legend.position = "none") + 
  NULL

p_age <- ggplot(df_all[df_all$type_no %in% 7:9, ],
                 aes(x=city, y=beta, col=type)) +
  scale_color_discrete(NULL) +
  geom_hline(yintercept=0, linetype="dashed") + 
  geom_point() +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle=40, hjust=1),
        legend.position = "right") +
  xlab(NULL) + 
  ylab(NULL) +
  facet_grid(.~city_group, scales="free_x", space="free") +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank()
  ) +
  ylim(ylim1, ylim2) + 
  ggtitle(NULL, subtitle = "Age") + 
  NULL

p_arrow <- ggplot() +
  geom_segment(aes(x=0, xend = 0 , y=1.5, yend = 6),
               size=0.2,
               arrow = arrow(length = unit(0.1,"cm"))) +
  geom_text(aes(x=-0.75, y=4, label="less travel", angle=90), size=3) +
  xlim(-1.4, 0.05) +
  ylim(1.5, 6) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

p_empty <- ggplot() +
  xlim(-0.5, 0.05) +
  ylim(0, 1) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


p_all <- wrap_plots(wrap_plots(p_arrow, p_empty, ncol=1,
                                heights=c(0.2, 1)),
                     wrap_plots(p_dist, p_income, p_age, ncol=1),
                     widths=c(0.05, 1))


ggsave(p_all, filename = "summer_model.png",
       units="in", width=9, height=7)

