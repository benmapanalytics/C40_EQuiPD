library(tidyverse)
library(ggnewscale)
library(stringr)
############################################################
##  CATEGORIES
############################################################
cats <- data.frame(abbr = c("DS", "CB", "PS", "IM", "AP", "SE"),
                   full = c("Data-sharing partnerships & management tools",
                            "City/institutional buy-in, capacity building & training",
                            "Public support & engagement",
                            "Health and economic impact modelling",
                            "Estimating air pollution",
                            "Source & emission inventories"))

############################################################
##  DOMAINS and EQUITY THEMES
############################################################
dims <- data.frame(abbr = c("TC", "DI", "IP", "PE", "DE", "SO", "AI"),
                   type = c("Domain", "Domain", "Domain", "Domain",
                            "Theme", "Theme", "Theme"),
                   full = c("Technical Capacity",
                            "Data Integration",
                            "Internal Processes",
                            "Participation and Empowerment",
                            "Distributional Equity",
                            "Socioeconomic Resilience and Opportunity",
                            "Access and Inclusivity"))
############################################################
##  C40 COLOR PALETTE
############################################################
dom_col <- c("#EC6C54",  # R236 G108 B84 - Coral/Orange-Red
             "#2C631C",  # R44 G99 B28 - Forest Green
             "#5AB9E8",  # R90 G185 B232 - Sky Blue
             "#193C68")  # R25 G60 B104 - Dark Blue

theme_col <- c("#7A66BB",  # R122 G102 B187 - Purple
               "#F9DA5B",  # R249 G218 B91 - Golden Yellow
               "#58BF56")  # R88 G191 B86 - Green

############################################################
##  helper that turns the matrix into polygons + arcs
############################################################
build_geoms <- function(radar_df,
                        spoke_gap = 6 * pi/180) {
  if (nrow(radar_df) == 0)
    return(list())
  
  cats <- cats$full
  doms <- dims$full[which(dims$type == "Domain")]
  thms <- dims$full[which(dims$type == "Theme")]
  
  n_cat <- length(cats) 
  n_dom <- length(doms)
  n_thm <- length(thms)
  a_mid <- seq(0, 2*pi - 2*pi/n_cat, length.out = n_cat)
  
  # --- Wedges ---
  poly_list <- vector("list", n_cat * (n_dom + n_thm)); 
  idx <- 0
  
  # Loop over categories
  for (k in seq_len(n_cat)) {
    
    offset <- ((seq_len(n_dom + n_thm) - ((n_dom + n_thm) + 1)/2) * spoke_gap)
    
    # Category/Domain
    for (j in seq_len(n_dom)) {
      idx <- idx + 1
      q_idx <- which(radar_df$category == cats[k] & radar_df$domain == doms[j])
      v <- mean(radar_df$score[q_idx], na.rm = TRUE)
      a_cen <- a_mid[k] + offset[j]
      poly_list[[idx]] <-
        tibble(cat = cats[k], dom = doms[j], thm = "", group = paste0(k, "_", j),
               x = c(0, 0, v * cos(a_cen + spoke_gap/2), v * cos(a_cen - spoke_gap/2)),
               y = c(0, 0, v * sin(a_cen + spoke_gap/2), v * sin(a_cen - spoke_gap/2)))
    }
    
    # Category/Theme
    for (l in seq_len(n_thm)) {
      idx <- idx + 1
      q_idx <- which(radar_df$category == cats[k] & radar_df$domain == thms[l])
      v <- mean(radar_df$score[q_idx], na.rm = TRUE)
      a_cen <- a_mid[k] + offset[l + j]
      poly_list[[idx]] <-
        tibble(cat = cats[k], dom = "", thm = thms[l], group = paste0(k, "_", l + j),
               x = c(0, 0, v * cos(a_cen + spoke_gap/2), v * cos(a_cen - spoke_gap/2)),
               y = c(0, 0, v * sin(a_cen + spoke_gap/2), v * sin(a_cen - spoke_gap/2)))
    }
  } # end loop over categories
  
  wedge_df <- bind_rows(poly_list)
  
  # --- Category labels ---
  avg_scores <- sapply(cats, function(c) {
    mean(radar_df$score[which(radar_df$category == c & is.na(radar_df$domain))], na.rm = TRUE)
  })
  lab_df <- tibble(cat       = cats,
                   angle_rad = a_mid,
                   cat_score = avg_scores,
                   x         = 3.4 * cos(a_mid),
                   y         = 3.4 * sin(a_mid),
                   hjust     = ifelse(a_mid > pi/2 & a_mid < 3*pi/2, 1, 0),
                   rot       = 0)
  
  # --- Circle labels ---
  circle_labels <- tibble(
    x = c(1, 2, 3),
    y = 0,
    label = c("1", "2", "3")
  )
  
  # --- Category average arcs (FIXED: centered on category, small size) ---
  arc_span <- spoke_gap * 3.5  # Back to original smaller size
  
  avg_arcs <- tibble(
    cat = cats,
    x0 = 0,
    y0 = 0,
    r = avg_scores,
    start = -a_mid + pi/2 - arc_span,  # Centered on main category angle
    end = -a_mid + pi/2 + arc_span   # Small arc centered on category
  )
  
  list(wedge = wedge_df, 
       label = lab_df, 
       circle_labels = circle_labels, 
       avg_arcs = avg_arcs)
}

build_radar <- function(g, g2) {
  if (length(g2) == 0) {
    plot <-
      ggplot() +
      geom_polygon(data = g$wedge[which(g$wedge$thm == ""),],
                   aes(x, y, group = group, fill = dom)) +
      scale_fill_manual(name = "Domains", values = dom_col, drop = FALSE) +
      guides(fill = guide_legend(ncol = 1, title.position = "top")) +
      new_scale_fill() +
      geom_polygon(data = g$wedge[which(g$wedge$dom == ""),],
                   aes(x, y, group = group, fill = thm)) +
      scale_fill_manual(name = "Equity Themes", values = theme_col, drop = FALSE) +
      guides(fill = guide_legend(ncol = 1, title.position = "top")) +
      geom_circle(aes(x0 = 0, y0 = 0, r = 1:3),
                  colour = "grey60", linetype = 3, linewidth = .3) +
      # Arcs
      geom_arc(data = g$avg_arcs,
               aes(x0 = x0, y0 = y0, r = r, start = start, end = end),
               linewidth = 1, linetype = 1,
               color = "#000000")
    
    
  } else { 
    # Plot with reassessment
    plot <-
      ggplot() +
      # Domains
      geom_polygon(data = g$wedge[which(g$wedge$thm == ""),],
                   aes(x, y, group = group, fill = dom)) +
      geom_polygon(data = g2$wedge[which(g2$wedge$thm == ""),],
                   aes(x, y, group = group, fill = dom), alpha = 0.5) +
      scale_fill_manual(name = "Domains", values = dom_col, drop = FALSE) +
      guides(fill = guide_legend(ncol = 1, title.position = "top")) +
      
      # Themes
      new_scale_fill() +
      geom_polygon(data = g$wedge[which(g$wedge$dom == ""),],
                   aes(x, y, group = group, fill = thm)) +
      geom_polygon(data = g2$wedge[which(g2$wedge$dom == ""),],
                   aes(x, y, group = group, fill = thm), alpha = 0.5) +
      scale_fill_manual(name = "Equity Themes", values = theme_col, drop = FALSE) +
      guides(fill = guide_legend(ncol = 1, title.position = "top")) +
      
      # Measurement circles
      geom_circle(aes(x0 = 0, y0 = 0, r = 1:3),
                  colour = "grey60", linetype = 3, linewidth = .3) +
      # Arcs
      geom_arc(data = g$avg_arcs,
               aes(x0 = x0, y0 = y0, r = r, start = start, end = end),
               linewidth = 1, linetype = 1,
               color = "#000000") +
      geom_arc(data = g2$avg_arcs,
               aes(x0 = x0, y0 = y0, r = r, start = start, end = end),
               linewidth = 1, linetype = 1,
               color = "#AAAAAA")
    
  }
  
  plot <- plot +
    geom_text(data = g$label,
              aes(x = x, y = y, label = str_wrap(cat, 24), 
                  angle = rot, hjust = hjust),
              size = 16 / .pt, lineheight = 2.5 / .pt,#14
              family = "Figtree") +
    coord_equal(clip = "off", expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.margin = margin(t = 30),
      plot.margin = margin(20, 80, 100, 80),
      legend.text = element_text(family = "Figtree",
        size = 30 / .pt),#30
      legend.title = element_text(family = "Figtree",
        size = 40 / .pt, face = "bold")#40
    ) + 
    labs(fill = "")
  
  return(plot)
}
