plt_ts_bp_4paper_andtlks <- function(plt_sv_nm,
                                       p_wdth, p_hgt,
                                       dat,
                                       this_form,
                                       col_scheme,
                                       ylabel,
                                       ylim,
                                       fig_lab,
                                       leg_on){
  
  
  pdf(paste(plt_sv_nm, '.pdf', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=3/4)
  ts_grp_bp(dat, this_form, col_scheme, ylabel, ylim, xlab_cex=3/4, leg_on)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '.svg', sep=''), 
      width = p_wdth/2.54, height = p_hgt/2.54)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=3/4)
  ts_grp_bp(dat, this_form, col_scheme, ylabel, ylim, xlab_cex=3/4, leg_on)
  fig_label(fig_lab)
  dev.off()
  
  # for talks
  tlk_scl = 2
  tlk_scl = 2
  pdf(paste(plt_sv_nm, '_4tlks.pdf', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=1.5)
  ts_grp_bp(dat, this_form, col_scheme, ylabel, ylim, xlab_cex=1.5, leg_on)
  fig_label(fig_lab)
  dev.off()
  
  svg(paste(plt_sv_nm, '_4tlks.svg', sep=''), # for talks
      width = p_wdth/2.54*tlk_scl, height = p_hgt/2.54*tlk_scl)
  par(family="Source Sans Pro", mar=c(4,4,2,1), las=2, cex=1.5)
  ts_grp_bp(dat, this_form, col_scheme, ylabel, ylim, xlab_cex=1.5, leg_on)
  fig_label(fig_lab)
  dev.off()
}

ts_grp_bp <- function(dat, this_form, col_scheme, ylabel, ylim, xlab_cex, leg_on){
  
  with(dat, 
       boxplot(as.formula(this_form),
               frame=F,
               at=c(1:2, 3.5:4.5),
               col=col_scheme,
               ylab=ylabel,
               ylim=ylim,
               yaxt='n',
               xaxt='n',
               xlab='',
               notch=FALSE))
  axis(1, at=c(1.5, 4), labels=c('Stable', 'Variable'), las=1)
  axis(2, at=seq(min(ylim), max(ylim), by=max(ylim)/4), labels=paste(seq(min(ylim), max(ylim), by=max(ylim)/4)))
  mtext('Group', side=1, line=2, las=1, cex=xlab_cex)
  if (leg_on) legend(2, 0.8, c('Stay','Switch'), fill=col_scheme, bty='n')
}
