# Demographics
demo <- Cs(
  age,
  sex.factor,    
  education,
  raceethnicity.factor,
  apoe4pos.factor,    
  diagnosis.factor    
)

# APOE Genotype and Polygenetic Risk Scores
apoe<-Cs(
  apoe4pos.factor,
  apoe2pos.factor,
  apoe4count,
  apoe2count,
  alleles #corrected name "alleles" 
  #pgc.score, # Poly genetic risk score
  #pgc.noapoe.score # Poly genetic risk score removing apoe
)

# Medical History
med.hx<-Cs(
  bmi,
  bsa,
  sbp, 
  dbp,
  cvd.factor,
  afib.factor,
  cvdafib.factor,
  echo.lvh.factor,
  diabetes.factor,
  htnrx.factor,  
  hypertensive.factor,   
  htn.factor,  
  currentsmoking.factor,
  dyslipidemia.factor
)

# FSRP and Related Points
fsrp.points <- Cs(
  fsrp,
  fsrp.minus.age.points,
  fsrp.age.pts,
  fsrp.sbp.pts,
  fsrp.diabetes.pts,
  fsrp.cigs.pts,
  fsrp.cvd.pts,
  fsrp.afib.pts,
  fsrp.lvh.pts    
)

# Neuropsychological Assessment 
neuropsych <- Cs(
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
  np.biber.ld,
  np.biber.discrim,
  np.cvlt1to5,
  np.cvlt.ldfr,
  np.cvltrecog.discrim
)

# Cognitive Complaint
scd<-Cs(
  tot.complaint,
  tot.complaint.short,
  tot.complaint.gifford.45,
  tot.complaint.gifford.45.ef,
  tot.complaint.gifford.45.lang,
  tot.complaint.gifford.45.mem
)

# Cerebrospinal Fluid (CSF) Biomarkers
csf<- Cs(
  csf.abx40 ,              
  csf.ab1.42 ,        
  amyloidPos.factor,    
  csf.abeta42.40.ratio,
  csf.tau,            
  tauPos.factor,    
  csf.ptau,
  ptauPos.factor,    
  csf.snap.factor,    
  csf.nfl,
  csf.neurogranin,
  csf.ykl40,
  csf.plasma.albumin.ratio,
  csf.strem2,
  csf.gap43,
  csf.brevican, 
  csf.timp1, 
  csf.timp2, 
  csf.mmp2, 
  csf.mmp3, 
  csf.mmp9    
)

# Blood-based Biomarkers
blood<-Cs(
  biomarkers.abx40.plasma,
  biomarkers.ab1.42.plasma,
  biomarkers.tau.plasma,
  biomarkers.nfl.plasma,
  biomarkers.albumin.plasma,
  biomarkers.vegf,
  biomarkers.il6
  )

# Cardiac Structure and Function Based on Echo
cardiac.echo<-Cs(
  echo.co.calc, 
  echo.cardiac.index,
  echo.ef.calc,
  echo.lv.stroke.volume,
  echo.hrate,
  echo.edv,
  echo.esv,
  echo.lvot,
  echo.myocardial.contraction.fraction
  )

# Cardiac Structure and Function Based on MRI
cardiac.mri <- Cs(
  qmass.usable,
  qmass.lv.absolute.ed.mass,
  qmass.lv.absolute.sv,
  qmass.lv.bsa.indexed.ed.mass,
  qmass.lv.bsa.indexed.co, 
  qmass.lv.absolute.ef,
  qstrain.usable,
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
  qstrain.avg.grs,
  pwv.usable,
  pwv)

# Brain MRI: White Matter Volume
lobe<-Cs(
  hemisphere,
  frontal.lobe,
  occipital.lobe,
  temporal.lobe,
  parietal.lobe
)
my.lobe<-function(prefix=NULL, lob, suffix=NULL){
  if(length(prefix)>0) {
    if(length(suffix)>0) paste(prefix, lob, suffix, sep='.') else paste(prefix, lob, sep='.')
  }else{
    if(length(suffix)>0) paste(lob, suffix, sep='.') else lob
  }
}

wml.roi<-c(rbind(my.lobe('left', lobe), 
             my.lobe('right', lobe)))

wml<-c(
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

ma.roi<-c(
  'ma.grey.matter',
  'ma.left.hemisphere',
  'ma.right.hemisphere',
   paste0('ma.', c(rbind(my.lobe('left', lobe[-1], 'vol'),
                         my.lobe('right', lobe[-1], 'vol')
                  )))
)
ma <- c('ma.usable.factor', ma.comp, ma.roi)

# Brain MRI:: Cerebrol Blood Flow (CBF) ROI assessed by ASL
asl.roi<-c('grey.matter',
       rbind(my.lobe('left', lobe, 'hct'), 
             my.lobe('right', lobe, 'hct')))
       
asl.rest  <- c('asl.rest.usable.hct.factor', paste0('asl.rest.', asl.roi))
asl.chall <- c('asl.chall.usable.hct.factor', paste0('asl.chall.', asl.roi))
asl.reac  <- c('asl.reac.usable.hct.factor', paste0('asl.reac.', asl.roi))
asl <- c(asl.rest, asl.chall, asl.reac)

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
vwi.vb<-Cs(vwi.vb
           #vwi.vb.id.adj
           )

vwi.pre <- c(vwi.ica, vwi.aca, vwi.mca, vwi.vb)
vwi.id<-paste0(vwi.pre, '.id')
vwi.thick<-paste0(vwi.pre, '.thick')
vwi<-c(
  #'vwi.scan.quality', #vessel wall index scan quality: missing means VWI scan not acquired; 1 means poor quality and should be excluded
  'vwi.usable.factor', #vessel wall index usable variable; has not been used
   vwi.id, 
   vwi.thick)


# Brain MRI: Cerebral Small Vessel Disease Markers 

lac<-Cs(
  lacunar.infarcts.usable.factor,
  lacunar.infarcts.number,
  lacunar.infarcts.number.factor)

pvs.han<-Cs(
  pvs.han.usable.factor,
  pvs.han.basal.ganglia,
  pvs.han.basal.ganglia.factor,
  pvs.han.centrum,
  pvs.han.centrum.factor,
  pvs.han.total,
  pvs.han.total.factor
)
pvs.pat<-Cs(
  pvs.pat.usable.factor,
  pvs.pat.centrum.factor,
  pvs.pat.mesencephalon.factor,
  pvs.pat.subinsular.factor,
  pvs.pat.basal.ganglia,
  pvs.pat.basal.ganglia.factor
)
swi<-Cs(
  swi.usable.factor,
  swi.microbleeds.number,
  swi.microbleeds.number.factor
)
svd<-c(lac, pvs.han, pvs.pat, swi)

####

variables.list <- list(
  demo = list(
    title = "Demographics",
    variables = demo
  ),
  
  apoe = list(
    title = "APOE Genotype and Polygenetic Risk Scores",
    variables = apoe
  ),
  
  med.hx = list(
    title = "Medical History",
    variables = med.hx
  ),
  
  fsrp.points = list(
    title = "FSRP and Related Points",
    variables = fsrp.points
  ),
  
  neuropsych = list(
    title = "Neuropsychological Assessment",
    variables = neuropsych
  ),
  
  scd = list(
    title = "Cognitive Complaint",
    variables = scd
  ),
  
  csf = list(
    title = "Cerebrospinal Fluid (CSF) Biomarkers",
    variables = csf
  ),
  
  blood = list(
    title = "Blood-based Biomarkers",
    variables = blood
  ),
  
  cardiac.echo = list(
    title = "Cardiac Structure and Function Based on Echocardiogram",
    variables = cardiac.echo
  ),
  
  cardiac.mri = list(
    title = "Cardiac Structure and Function Based on MRI",
    variables = cardiac.mri
  ),
  
  wml = list(
    title = "Brain MRI: White Matter Volume",
    variables = wml
  ),
  
  ma = list(
    title = "Brain MRI: Grey Matter Volume",
    variables = ma
  ),
  
  asl = list(
    title = "Brain MRI: Cerebral Blood Flow (CBF) ROI assessed by ASL",
    variables = asl
  ),
  
  vwi = list(
    title = "Brain MRI: Vessel Wall Imaging",
    variables = vwi
  ),
  
  svd = list(
    title = "Brain MRI: Cerebral Small Vessel Disease Markers",
    variables = svd
  )
)

