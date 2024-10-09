library(tidyverse)
theme_dendrogram <- function(){theme_bw() + 
    theme(
      legend.background = element_blank(),
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"), 
      axis.line.y = element_line(color = "black"), 
      axis.ticks.y = element_line(color = "black"), 
      axis.text.y = element_text(color = "black")
    )
}

pi_A <- read_tsv("pi-g1-L.windowed.pi")
pi_B <- read_tsv("pi-g2-L.windowed.pi")
fst <- read_tsv('fst-all.windowed.weir.fst')
pi <- pi_A %>% 
  left_join(pi_B, by=c("CHROM", "BIN_START", "BIN_END")) %>% 
  dplyr::rename(pi_A=PI.x, pi_B=PI.y) %>%
  left_join(fst, by=c("CHROM", "BIN_START", "BIN_END")) %>%
  select(-c("N_VARIANTS.x", "N_VARIANTS.y", "N_VARIANTS", "WEIGHTED_FST")) %>%
  mutate(AVSB=pi_A/pi_B,
         BVSA=pi_B/pi_A)
pi <- na.omit(pi)

fst90 <- quantile(pi$MEAN_FST, 0.95)
pi90 <-  quantile(pi$AVSB, 0.95, na.rm = TRUE)
pi10 <-  quantile(pi$AVSB, 0.05, na.rm = TRUE)

limit_x <- c(min(log(pi$AVSB), na.rm=TRUE)*0.95, max(log(pi$AVSB), na.rm=TRUE)*1.05)
limit_y <- c(min(pi$MEAN_FST, na.rm=TRUE)*0.95, max(pi$MEAN_FST, na.rm=TRUE)*1.05)

p_main <- ggplot(pi, aes(x=log(AVSB), y=MEAN_FST)) +
  geom_point(
    aes(
      color=case_when(
        log(AVSB) > log(pi90)  & MEAN_FST > fst90 ~ 'Selected region (B region)', 
        log(AVSB) < log(pi10)  & MEAN_FST > fst90 ~ 'Selected region (A region)',
        TRUE ~ 'Whole genome'
      )
    )
  ) +
  geom_vline(xintercept=log(pi90), color='grey66', linetype="dashed", size=1) +
  geom_vline(xintercept=log(pi10), color='grey66', linetype="dashed", size=1) +
  geom_hline(yintercept=fst90, color='grey66', linetype="dashed", size=1) +
  lims(x=limit_x, y=limit_y) +
  labs(x=expr(theta[pi]*"ratio("*pi[italic('A')]/pi[italic('B')]*")"),
       y=expr(italic("F"["ST"]))) +
  scale_color_manual('', values = c('Selected region (A region)'='#39539d',
                                    'Selected region (B region)'='#55ad57',
                                    'Whole genome'='grey')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.75, 0.25),
        legend.background = element_blank())


get_midpoint <- function(cut_label) {
  tmp_lst <- strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ",")
  sapply(tmp_lst, function(x)mean(as.numeric(x)))
}

data_right <-pi %>%
  mutate(cut_group = cut_width(MEAN_FST, width = 0.01)) %>% 
  group_by(cut_group) %>%
  summarize(counts=n()) %>%
  mutate(prop=counts/sum(counts),
         mid_pos=get_midpoint(as.character(cut_group))) %>%
  mutate(plot_prop = 1/max(prop) * prop) 

max1<-max(data_right$prop)
fst90_str <- as.character(round(fst90, 3))

p_right <- ggplot(data_right) + 
  stat_ecdf(data=pi, 
            aes(y=MEAN_FST), 
            geom="smooth", 
            se=F, 
            size=0.5,
            color="black") +
  geom_col(aes(y = mid_pos, 
               x=plot_prop,
               fill=ifelse(mid_pos>fst90,
                           'selected',
                           'other')
  ), 
  orientation = 'y') + 
  geom_hline(yintercept=fst90, color='grey66', linetype="dashed", size=1) +
  lims(y=limit_y) +
  scale_x_continuous("Cumulative (%)", 
                     labels = scales::percent_format(suffix = ''),
                     position = "top",
                     sec.axis = sec_axis(~.*max1,
                                         name="Frequency (%)",
                                         labels = scales::percent_format(suffix = ''))
  ) +
  scale_fill_manual('', 
                    values = c('selected'= '#e15e37','other'='grey66'),
                    labels = c('selected'= expr(italic("F"["ST"])>!!fst90_str),
                               'other'=expr(italic("F"["ST"])<=!!fst90_str)
                    )
  ) +
  theme_dendrogram() +
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.line.y = element_blank(),
        legend.position = c(0.75, 0.25))




data_top <-pi %>%
  mutate(cut_group = cut_width(log(AVSB), width = 0.01)) %>% 
  group_by(cut_group) %>%
  summarize(counts=n()) %>%
  mutate(prop=counts/sum(counts),
         mid_pos=get_midpoint(as.character(cut_group))) %>%
  mutate(plot_prop = 1/max(prop) * prop)

max2<-max(data_top$prop)
pi90_str <- as.character(round(log(pi90), 3))
pi10_str <- as.character(round(log(pi10), 3))

p_top <- ggplot(data_top) + 
  stat_ecdf(data=pi, 
            aes(x=log(AVSB)), 
            geom="smooth", 
            se=F, 
            size=0.5,
            color="black") +
  geom_col(aes(x= mid_pos, 
               y=plot_prop,
               fill=case_when(
                 mid_pos > log(pi90) ~ 'selectedB', 
                 mid_pos < log(pi10) ~ 'selectedA',
                 TRUE ~ 'other'
               )
  )
  ) + 
  geom_vline(xintercept=log(pi90), color='grey66', linetype="dashed", size=1) +
  geom_vline(xintercept=log(pi10), color='grey66', linetype="dashed", size=1) + 
  lims(x=limit_x) +
  scale_y_continuous("Cumulative (%)", 
                     labels = scales::percent_format(suffix = ''),
                     position = "right",
                     sec.axis = sec_axis(~.*max2,
                                         name="Frequency (%)",
                                         labels = scales::percent_format(suffix = ''))
  ) +
  scale_fill_manual('', 
                    values = c('selectedA'='#39539d',
                               'selectedB'='#55ad57',
                               'other'='grey'),
                    labels = c('selectedA'= expr(theta[pi]*"ratio"<=!!pi10_str),
                               'selectedB'= expr(theta[pi]*"ratio">=!!pi90_str),
                               'other'=expr(!!pi10_str<theta[pi]*"ratio<"*!!pi90_str))
  ) +
  theme_dendrogram() +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.line.x=element_blank(),
        legend.position = c(0.75, 0.25)
  )



library(patchwork)
p_top + plot_spacer() + p_main + p_right + plot_layout(ncol=2, heights = c(1,4), widths=c(4,1))
ggsave("pi-fst.pdf", width=18, height=16, units="cm")

write.table(pi,file='pi-fst.xls',sep='\t')
