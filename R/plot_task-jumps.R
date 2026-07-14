gen_jumps_plot <- function(data,
                           plot_formula,
                            exp_strs,
                            cols,
                            p_wdth, p_hgt,
                            plt_sv_nm,
                            fig_labs,
                            ylabel,
                            ylims,
                            xlabel,
                            title,
                            title_strs,
                            figfont){
  
#  fig_labs = c("C", "D")
  ###########################################################
  # plot task jumps box plots
  ##########################################################
  # for manuscripts
  pdf(paste(plt_sv_nm, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(family=figfont, mfrow = c(1,2), mar = c(4, 4, 2, 1), las=2, cex=3/4)
  for (i in 1:length(exp_strs)){
    if (i == 1){
      leg = TRUE
      ylab = ylabel
      xlab = xlabel
    } else {
      leg = FALSE
      ylab = ''
      xlab = ''
    }
    plot_task_jumps(data, plot_formula, exp_strs[i], cols, leg, ylab, ylims, 
                    xlab, title = TRUE, title_str = title_strs[i])
    fig_label(fig_labs[i])
  }
  dev.off()
  
  svg(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54) 
  par(family=figfont, mfrow = c(1,2), mar = c(4, 4, 2, 1), las=2, cex=3/4)
  for (i in 1:length(exp_strs)){
    if (i == 1){
      leg = TRUE
      ylab = ylabel
      xlab = xlabel
    } else {
      leg = FALSE
      ylab = ''
      xlab = ''
    }
    plot_task_jumps(data, plot_formula, exp_strs[i], cols, leg, ylab, ylims, 
                    xlab, title = TRUE, title_str = title_strs[i])
    fig_label(fig_labs[i])
  }
  dev.off()
  
  ##########################################################
  # for talks
  pdf(paste(plt_sv_nm, '_4tlks.pdf', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(family=figfont, mfrow = c(1,2), mar = c(4, 4, 2, 1), las=2, cex=1.5)
  for (i in 1:length(exp_strs)){
    if (i == 1){
      leg = TRUE
      ylab = ylabel
      xlab = xlabel
    } else {
      leg = FALSE
      ylab = ''
      xlab = ''
    }
    plot_task_jumps(data, plot_formula, exp_strs[i], cols, leg, ylab, ylims, 
                    xlab, title = TRUE, title_str = title_strs[i])
    fig_label(fig_labs[i])
  }
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*2.5, height = p_hgt/2.54*2.5)
  par(family=figfont, mfrow = c(1,2), mar = c(4, 4, 2, 1), las=2, cex=1.5)
  for (i in 1:length(exp_strs)){
    if (i == 1){
      leg = TRUE
      ylab = ylabel
      xlab = xlabel
    } else {
      leg = FALSE
      ylab = ''
      xlab = ''
    }
    plot_task_jumps(data, plot_formula, exp_strs[i], cols, leg, ylab, ylims, 
                    xlab, title = TRUE, title_str = title_strs[i])
    fig_label(fig_labs[i])
  }
  dev.off()
}


plot_task_jumps <- function(data,
                            plot_formula,
                            exp_str,
                            cols,
                            leg = TRUE,
                            ylab,
                            ylims,
                            xlab,
                            title = FALSE,
                            title_str = NA){
  # use this function to create the appropriate box plot
  data$train_type <- factor(data$train_type,
                            levels = c(1, 2),
                            labels = c("stable", "variable"))
  data$switch <- factor(data$switch,
                        levels = c(0, 1),
                        labels = c("stay", "switch"))
  plot_data <- data %>% 
    filter(exp == exp_str)
  
  with(data %>% filter(exp == exp_str),
       boxplot(as.formula(plot_formula),
               at=c(1:2, 4:5),
               frame=F,
               yaxt = 'n',
               xaxt='n',
               col=rep(cols,2),
               outline=FALSE,
               ylab=ylab,
               ylim=ylims,
               xlab=xlab))
  axis(1, at = c(1.5, 4.5), labels=c('Stable','Variable'), las=1)
  axis(2, at = ylims)
  
  
  for(tt in levels(plot_data$train_type)) {
    
    tmp <- plot_data %>%
      filter(train_type == tt)
    
    xpos <- if(tt == "stable") {c(stay = 1, switch = 2)
    } else {c(stay = 4, switch = 5)}
    
    tmp_wide <- tmp %>%
      tidyr::pivot_wider(
        id_cols = sub,
        names_from = switch,
        values_from = all.vars(as.formula(plot_formula))[1]
      )
    
    segments(
      x0 = xpos["stay"],
      y0 = tmp_wide$stay,
      x1 = xpos["switch"],
      y1 = tmp_wide$switch,
      col = adjustcolor("grey70", alpha.f = 0.3)
    )
    
    
    points(rep(xpos["stay"], nrow(tmp_wide)),
           tmp_wide$stay,
           pch = 16,
           cex = 0.6,
           col = adjustcolor("grey70", alpha.f = 0.5))
    
    points(rep(xpos["switch"], nrow(tmp_wide)),
           tmp_wide$switch,
           pch = 16,
           cex = 0.6,
           col = adjustcolor("grey70", alpha.f = 0.5))
  }
    
  if (leg){
    legend('topleft', c('Stay','Switch'), fill=col_scheme, bty='n')
  }
  if (title){
    title(title_str)
  }
  
}

