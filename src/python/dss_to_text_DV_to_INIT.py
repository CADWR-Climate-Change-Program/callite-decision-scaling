#=====================================================================================
# example: dss export to txt
#
#several pathnames have been commented out because they are not part of the DV.dss records
#=====================================================================================

import sys
import vutils as vu
from scripts.tool import DssVista

# parts are exact except for the blank parts
def getDataRef(group, path, timewindow):

    p = getExactPartsFromPath(path)
    data = vu.findparts(group, a=p[0] , b=p[1] , c=p[2] , d=p[3] , e=p[4], f=p[5])[0]
    dataReference = vu.DataReference.create(data,timewindow)
    return dataReference

# parts are exact except for the blank parts
def getExactPartsFromPath(path):

    p_in = path.split('/')[1:-1]
    #print p_in
    p = []
    for part in p_in:
        if part:
            part = "^"+part+"$"
        p.append(part)
    #print p
    return p


text_file = open(sys.argv[1],'w')

dss_file = sys.argv[2]
vu.DSSUtil.generateCatalog(dss_file)

g = vu.opendss(dss_file)

begin_time = "30SEP1971 2400"
end_time = "30SEP1971 2400"
tw = vu.timewindow(begin_time + " - " + end_time)

pathnames = []

#pathnames.append("/CALLITE/A18/SURFACE-AREA//1MON/2020D09E/")
#pathnames.append("/CALLITE/AD_SJR/FLOW-ACCRDEPL//1MON/2020D09E/")
#pathnames.append("/CALLITE/ANN_CCWD_DEL_/ANNUAL-SUM//1MON/2020D09E/")
#pathnames.append("/CALLITE/ANN_CVP_DIV_/ANNUAL-DIVERSION//1MON/2020D09E/")
#pathnames.append("/CALLITE/ANN_DS_DIV_/ANNUAL-DIVERSION//1MON/2020D09E/")
#pathnames.append("/CALLITE/ANN_MOK_DIV_/ANNUAL-DIVERSION//1MON/2020D09E/")
pathnames.append("/CALLITE/C_DELTA/FLOW-CHANNEL//1MON/2020D09E/")
pathnames.append("/CALLITE/C_HOOD/FLOW-CHANNEL//1MON/2020D09E/")
pathnames.append("/CALLITE/C_HOOD_ANN/FLOW-ANN//1MON/2020D09E/")
pathnames.append("/CALLITE/C_MLRTNF/FLOW-FLOOD//1MON/2020D09E/")
pathnames.append("/CALLITE/C_SACRV/FLOW-CHANNEL//1MON/2020D09E/")
pathnames.append("/CALLITE/D18F/FLOW-DELIVERY//1MON/2020D09E/")
#pathnames.append("/CALLITE/D408_CVP/FLOW-DELIVERY//1MON/2020D09E/")
#pathnames.append("/CALLITE/D408_DS/FLOW-DELIVERY//1MON/2020D09E/")
#pathnames.append("/CALLITE/D420/FLOW-DELIVERY//1MON/2020D09E/")
pathnames.append("/CALLITE/DAYSINDV/DAYS//1MON/2020D09E/")
#pathnames.append("/CALLITE/DAYSMETFLOW60_/DAYS-MET//1MON/2020D09E/")
#pathnames.append("/CALLITE/DEL_SWP_CODV/DEL-TARGET//1MON/2020D09E/")
pathnames.append("/CALLITE/DXC_DAYSOPEN/GATE-DAYS-OPEN//1MON/2020D09E/")
pathnames.append("/CALLITE/D_EXPTD/FLOW-DELIVERY//1MON/2020D09E/")
pathnames.append("/CALLITE/D_FKCNL_16B/FLOW-DELIVERY//1MON/2020D09E/")
pathnames.append("/CALLITE/D_FKCNL_215/FLOW-DELIVERY//1MON/2020D09E/")
pathnames.append("/CALLITE/D_MDRCNL_16B/FLOW-DELIVERY//1MON/2020D09E/")
pathnames.append("/CALLITE/D_MDRCNL_215/FLOW-DELIVERY//1MON/2020D09E/")
pathnames.append("/CALLITE/EXP_OTH/FLOW-EXPORTS//1MON/2020D09E/")
pathnames.append("/CALLITE/FR_SNOW_REL_VOL/FRIANT-FC//1MON/2020D09E/")
#pathnames.append("/CALLITE/I424/FLOW-INFLOW//1MON/2020D09E/")
pathnames.append("/CALLITE/NDO/FLOW-NDO//1MON/2020D09E/")
pathnames.append("/CALLITE/NDO_SWRCB_DV/DELTAFLOWCRITERIA//1MON/2020D09E/")
pathnames.append("/CALLITE/NET_DICU/DICU_FLOW//1MON/2020D09E/")
pathnames.append("/CALLITE/REMWQRELCAP/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/RE_UND_D18B_C1/REALLOCATED//1MON/2020D09E/")
pathnames.append("/CALLITE/RE_UND_D18B_C2/REALLOCATED//1MON/2020D09E/")
pathnames.append("/CALLITE/RIOVISTA_SWRCB_DV/DELTAFLOWCRITERIA//1MON/2020D09E/")
pathnames.append("/CALLITE/S17/STORAGE//1MON/2020D09E/")
#pathnames.append("/CALLITE/S422/STORAGE//1MON/2020D09E/")
#pathnames.append("/CALLITE/S422_CC/STORAGE//1MON/2020D09E/")
#pathnames.append("/CALLITE/S422_P/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S75/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S76/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S78/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S79/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/SAC_OTH/FLOW-INFLOW//1MON/2020D09E/")
pathnames.append("/CALLITE/SIO/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/SIO_CVP/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/SIO_FWUA/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/SIO_SWP/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/SJR_ANN/FLOW-CHANNEL//1MON/2020D09E/")
pathnames.append("/CALLITE/SJR_SWRCB_DV/DELTAFLOWCRITERIA//1MON/2020D09E/")
pathnames.append("/CALLITE/SUM_MAD_C1_PCT/PERCENTAGE-DELIVERED//1MON/2020D09E/")
pathnames.append("/CALLITE/SUM_MAD_TOT_PCT/PERCENTAGE-DELIVERED//1MON/2020D09E/")
pathnames.append("/CALLITE/S_ESTMN/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_FOLSM/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_HNSLY/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_MCLRE/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_MELON/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_MLRTN/STORAGE//1MON/2020D09E/")
#pathnames.append("/CALLITE/S_MLRTNF/FLOW-FLOOD//1MON/2020D09E/")
pathnames.append("/CALLITE/S_NHGAN/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_OROVL/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_PEDRO/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_SHSTA/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_SHSTAE/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_SLCVP/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_SLSWP/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_TRNTY/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/S_WKYTN/STORAGE//1MON/2020D09E/")
pathnames.append("/CALLITE/TOT_UND_D18B_C1/UNDER-DELIVERY//1MON/2020D09E/")
pathnames.append("/CALLITE/TOT_UND_D18B_C2/UNDER-DELIVERY//1MON/2020D09E/")
pathnames.append("/CALLITE/UND_D18B_C1/UNDER-DELIVERY//1MON/2020D09E/")
pathnames.append("/CALLITE/UND_D18B_C2/UNDER-DELIVERY//1MON/2020D09E/")
pathnames.append("/CALLITE/VAMP_DEBT/FLOW-RELEASE//1MON/2020D09E/")
pathnames.append("/CALLITE/VERNWQFINAL/SALINITY-EC//1MON/2020D09E/")
#pathnames.append("/CALLITE/WQ422/SALINITY//1MON/2020D09E/")
pathnames.append("/CALLITE/WQRELCAPDV/WQ-REL-CAP//1MON/2020D09E/")
pathnames.append("/CALLITE/X2_PRV/X2-POSITION-PREV//1MON/2020D09E/")


y_arr = {}

text_file.write('pathname')
for pathname in pathnames:

    da = getDataRef(g, pathname, tw)

    ds = da.getData()

    y_arr[pathname] = ds.getYArray()

    text_file.write(','+pathname)

text_file.write('\n')

for i in range(0,len(y_arr[pathnames[0]])):

    text_file.write('value')
    for pathname in pathnames:

        text_file.write(','+str(y_arr[pathname][i]))
    text_file.write('\n')

text_file.close()

# print 'completed.'

exit()
