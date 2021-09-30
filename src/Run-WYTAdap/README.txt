The following files were modified to point to the water year type calculated according to the adaptive (re-calibrated) water year typing scheme

.\Wytypes\wytypes.wresl
add wyt_SAC_adap definitions

.\SanJoaquin\Vernalis\vernalis_min.wresl
define X2EastOfChipps {select VernMinReqEast from VernMin where wyt_SJR=wyt_SJR_adap }
define X2WestOfChipps {select VernMinReqWest from VernMin where wyt_SJR=wyt_SJR_adap }

.\SanJoaquin\Various\definitions\previous_wyt_sjr.wresl
wyt_SJRave5_adap 

.\SanJoaquin\Mokulumne\deliver_mok.wresl
wyt_SJR_adap

.\cvp_dellogic\CVP_delivery_logic_south\delcar_cvp_south_B2BO.wresl
wyt_SJR_adap

.\Export_Ops\OMR\OMR_constraint.wresl
wyt_SAC_adap

.\Export_Ops\NMFS_SALMON_BO_SJR.wresl
define SJR_indicator  {value 6. - wyt_SJR_adap}
define SJR_indicator1 {value 6. - wyt_SJR_prv1_adap}
define SJR_indicator2 {value 6. - wyt_SJR_prv2_adap}

.\Export_Ops\NMFS_SALMON_BO_DCC.wresl
wyt_SAC_adap

.\Export_Ops\FWS_SMELT_BO_FallX2check.wresl
wyt_SAC_adap 

.\Delta\riovista.wresl
wyt_SAC_adap

.\Delta\delta-outflow.wresl
wyt_SAC_adap

.\Delta\ANN\X2days_FWS_WQ.wre
wyt_SAC_adap

.\Delta\ANN\RockSlough_data.wresl
wyt_SAC_adap

.\Delta\ANN\JerseyPoint_data.wresl
wyt_SAC_adap

.\Delta\ANN\Emmaton_data.wresl
wyt_SAC_adap

