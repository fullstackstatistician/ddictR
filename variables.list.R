# Demographics
demo.var <- Cs(
  age,
  sex.factor,
  education,
  raceethnicity.factor,
  diagnosis.factor    
)

# APOE Genotype and Polygenetic Risk Scores
apoe.var <- Cs(
  alleles.factor,
  apoe4count,  
  apoe4pos.factor,
  apoe2count,  
  apoe2pos.factor
  #pgc.score, # Poly genetic risk score
  #pgc.noapoe.score # Poly genetic risk score removing apoe
)

# Medical History
med.hx.var <- Cs(
  height,
  weight,
  bmi,
  bsa,
  sbp, 
  dbp,
  pp, 
  cvd.factor,
  afib.factor,
  cvdafib.factor,
  diabetes.factor,
  htnrx.factor,  
  hypertensive.factor,   
  htn.factor,  
  currentsmoking.factor,
  dyslipidemia.factor
)

# Neuropsychological Assessment 
neuropsych.var <- Cs(
  np.moca,
  np.bnt,
  np.anim,
  np.tmta,
  np.tmta.trun,
  np.digsymb,
  np.executive.composite,
  np.tower,
  np.inhibit,
  np.inhibit.trun,
  np.fas,
  np.tmtb,
  np.tmtb.trun,
  np.hvot,
  np.memory.composite,
  np.biber.t1to5,
  # biber interference
  np.biber.ld,
  np.biber.discrim,
  np.cvlt1to5,
  # cvlt interference
  np.cvlt.ldfr,
  np.cvltrecog.discrim
)

# Cognitive Complaint
scd.var <- Cs(
  tot.complaint,
  tot.complaint.short,
  tot.complaint.gifford.45,
  tot.complaint.gifford.45.ef,
  tot.complaint.gifford.45.lang,
  tot.complaint.gifford.45.mem
)

# Cerebrospinal Fluid (CSF) Research Biomarkers
csf.var <- Cs(
  csf.abx42,
  csf.abx40,
  csf.abx42.abx40.ratio,
  csf.ab1.42,
  amyloidPos.factor,    
  csf.tau,
  tauPos.factor,
  csf.snap.factor,
  csf.ptau,
  ptauPos.factor,
  csf.nfl,
  csf.neurogranin,
  csf.ykl40,
  csf.gap43,
  csf.albumin,
  csf.plasma.albumin.ratio,
  csf.strem2,
  csf.brevican,
  csf.timp1,
  csf.timp2,
  csf.mmp2,
  csf.mmp3,
  csf.mmp9,
  csf.upenn.ab1.42,
  csf.upenn.tau,
  csf.upenn.ptau,
  csf.upenn.tau.ab1.42.ratio,
  csf.upenn.ptau.ab1.42.ratio,
  csf.abeta42.40.ratio
)

# Blood-based Research Biomarkers
blood.var <- Cs(
  biomarkers.leptin,
  biomarkers.il6,
  biomarkers.tnfalpha,
  biomarkers.vegf,
  biomarkers.vegf.serum,
  biomarkers.nfl.plasma,
  biomarkers.tau.plasma,
  biomarkers.ab1.42.plasma,
  biomarkers.abx40.plasma,
  biomarkers.albumin.plasma
)

# Cardiac Structure and Function -- Echocardiogram
cardiac.echo.var <- Cs(
  echo.hrate,
  echo.lv.stroke.volume,
  echo.lvot,
  echo.co.calc, 
  echo.cardiac.index,
  echo.edv,
  echo.esv,
  echo.ef.calc,
  echo.myocardial.contraction.fraction,
  echo.lvh.factor,
  echo.lvmass,
  echo.lvh.scaledlvmass,
  echo.lvh.scaledlvmass.factor
)

# Cardiac Structure and Function -- MRI
cardiac.mri.var <- Cs(
  qmass.usable.factor,
  qmass.lv.absolute.ed.mass,
  qmass.lv.absolute.sv,
  qmass.lv.bsa.indexed.ed.mass,
  qmass.lv.bsa.indexed.co, 
  qmass.lv.absolute.ef,
  pwv.usable.factor,
  pwv
)

# Cardiac Strain -- MRI
cardiac.strain.mri.var <- Cs(
  qstrain.usable.factor,
  qstrain.2ch.myogls,
  qstrain.2ch.myogcs,
  qstrain.2ch.sd.ts.peak,
  qstrain.2ch.sd.ls.peak,
  qstrain.mid.fac,
  qstrain.mid.myorot,
  qstrain.mid.myogcs,
  qstrain.mid.grs,
  qstrain.mid.delta.rot,
  qstrain.mid.sd.rs.peak,
  qstrain.mid.sd.cs.peak,
  qstrain.avg.grs
)

# FSRP and Related Points
fsrp.points.var <- Cs(
  fsrp.age.pts,
  fsrp.sbp.pts,
  fsrp.diabetes.pts,
  fsrp.cigs.pts,
  fsrp.cvd.pts,
  fsrp.afib.pts,
  fsrp.lvh.pts,
  fsrp,  
  fsrp.minus.age.points
)

# Brain MRI: White Matter Volume
lobe <- Cs(
  hemisphere,
  frontal.lobe,
  temporal.lobe,
  parietal.lobe,
  occipital.lobe
)

my.lobe <- function(prefix = NULL, lob, suffix = NULL){
  if (length(prefix) > 0) {
    if (length(suffix) > 0) paste(prefix, lob, suffix, sep = '.') else paste(prefix, lob, sep = '.')
  } else{
    if (length(suffix) > 0) paste(lob, suffix, sep = '.') else lob
  }
}

wml.roi <- c(
  rbind(
    my.lobe('left', lobe), 
    my.lobe('right', lobe)
  )
)

wml.var <- c(
  'wml.usable.factor',
  'wml.volume',
  paste0('wml.volume.', wml.roi)
)


# Brain MRI: Grey Matter Volume
ma.comp <- Cs(
  ma.lh.hippocampus.vol,
  ma.rh.hippocampus.vol,
  ma.lh.precuneus.vol,
  ma.rh.precuneus.vol,
  ma.lh.g.parahippocampal.vol,
  ma.rh.g.parahippocampal.vol,
  ma.lh.entorhinal.area,
  ma.rh.entorhinal.area,
  ma.lh.inf.lat.vent.vol,
  ma.rh.inf.lat.vent.vol
)

ma.roi <- c(
  'ma.grey.matter',
  'ma.left.hemisphere',
  'ma.right.hemisphere',
  paste0('ma.', c(rbind(my.lobe('left', lobe[-1], 'vol'),
                        my.lobe('right', lobe[-1], 'vol')
  )))
)

ma.var <- c(
  'ma.usable.factor', 
  ma.roi,
  ma.comp
)

# Brain MRI:: Cerebrol Blood Flow (CBF) ROI assessed by ASL
asl.roi <- c(
  'grey.matter',
  rbind(
    my.lobe('left', lobe, 'hct'), 
    my.lobe('right', lobe, 'hct')
  )
)

asl.rest  <- c('asl.rest.usable.hct.factor', paste0('asl.rest.', asl.roi))
asl.chall <- c('asl.chall.usable.hct.factor', paste0('asl.chall.', asl.roi))
asl.reac  <- c('asl.reac.usable.hct.factor', paste0('asl.reac.', asl.roi))

asl.var <- c(
  asl.rest, 
  asl.chall, 
  asl.reac
)

# Brain MRI: Vessel Wall Imaging
vwi.ica <- Cs(
  vwi.ica,
  vwi.right.ica,
  vwi.left.ica
  #    vwi.right.ica.id.adj,
  #    vwi.left.ica.id.adj
)

vwi.aca <- Cs(
  vwi.aca,
  vwi.right.aca,
  vwi.left.aca
  #    vwi.right.aca.id.adj,
  #    vwi.left.aca.id.adj
)

vwi.mca <- Cs(
  vwi.mca,
  vwi.right.mca,
  vwi.left.mca
  # vwi.right.mca.id.adj,
  # vwi.left.mca.id.adj
)

vwi.vb <- Cs(
  vwi.vb
  #vwi.vb.id.adj
)

vwi.pre <- c(vwi.ica, vwi.aca, vwi.mca, vwi.vb)
vwi.id <- paste0(vwi.pre, '.id')
vwi.thick <- paste0(vwi.pre, '.thick')
vwi.var <- c(
  #'vwi.scan.quality', #vessel wall index scan quality: missing means VWI scan not acquired; 1 means poor quality and should be excluded
  'vwi.usable.factor', #vessel wall index usable variable; has not been used
  vwi.id, 
  vwi.thick
)


# Brain MRI: Cerebral Small Vessel Disease Markers 
lac <- Cs(
  lacunar.infarcts.usable.factor,
  lacunar.infarcts.number,
  lacunar.infarcts.number.factor
)

pvs.han <- Cs(
  pvs.han.usable.factor,
  pvs.han.basal.ganglia,
  pvs.han.basal.ganglia.factor,
  pvs.han.centrum,
  pvs.han.centrum.factor,
  pvs.han.total,
  pvs.han.total.factor
)

pvs.pat <- Cs(
  pvs.pat.usable.factor,
  pvs.pat.centrum.factor,
  pvs.pat.mesencephalon.factor,
  pvs.pat.subinsular.factor,
  pvs.pat.basal.ganglia,
  pvs.pat.basal.ganglia.factor
)

swi <- Cs(
  swi.usable.factor,
  swi.microbleeds.number,
  swi.microbleeds.number.factor
)

svd.var <- c(
  lac, 
  pvs.han, 
  pvs.pat, 
  swi
)


####

variables.list <- list(
  demo.var = list(
    title = "Demographics",
    variables = demo.var
  ),
  
  apoe.var = list(
    title = "APOE Genotype and Polygenetic Risk Scores",
    variables = apoe.var
  ),
  
  med.hx.var = list(
    title = "Medical History",
    variables = med.hx.var
  ),
  
  fsrp.points.var = list(
    title = "FSRP",
    variables = fsrp.points.var
  ),
  
  scd.var = list(
    title = "Cognitive Complaint Module",
    variables = scd.var
  ),
  
  csf.var = list(
    title = "Cerebrospinal Fluid (CSF) Research Biomarkers",
    variables = csf.var
  ),
  
  blood.var = list(
    title = "Blood-based Research Biomarkers",
    variables = blood.var
  ),
  
  cardiac.echo.var = list(
    title = "Cardiac Structure and Function -- Echocardiogram",
    variables = cardiac.echo.var
  ),
  
  cardiac.mri.var = list(
    title = "Cardiac Structure and Function -- MRI",
    variables = cardiac.mri.var
  ),
  
  cardiac.strain.mri.var = list(
    title = "Cardiac Strain -- MRI",
    variables = cardiac.strain.mri.var
  )
  
  neuropsych.var = list(
    title = "Neuropsychological Assessment",
    variables = neuropsych.var
  ),
  
  wml.var = list(
    title = "Brain MRI: White Matter Hyperintensities (FLAIR)",
    variables = wml.var
  ),
  
  ma.var = list(
    title = "Brain MRI: Grey Matter Volume (T1)",
    variables = ma.var
  ),
  
  asl.var = list(
    title = "Brain MRI: Cerebral Blood Flow (CBF) ROI -- PCASL",
    variables = asl.var
  ),
  
  vwi.var = list(
    title = "Brain MRI: Vessel Wall Imaging",
    variables = vwi.var
  ),
  
  svd.var = list(
    title = "Brain MRI: Cerebral Small Vessel Disease Markers",
    variables = svd.var
  )
)

