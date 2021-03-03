#=====================================================================================
# example: read txt into dss
#=====================================================================================
import sys
import vutils as vu
from scripts.tool import DssVista


text_file = open(sys.argv[1],"r" )
dss_file = sys.argv[2]

per_row = []
for line in text_file:
    per_row.append(line.strip().split(','))

per_column = zip(*per_row)

for col in per_column:

    begin_time = col[0].strip()
    pathname  = col[1].strip()
    units = col[2].strip()

    # print pathname, begin_time, units


    array = map(float, col[3:])

    DssVista.array2dss(dss_file, array, begin_time, pathname, units)


text_file.close()

#vu.DSSUtil.generateCatalog(dss_file)

# print 'completed.'

exit()











