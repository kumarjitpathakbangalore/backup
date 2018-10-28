from collections import defaultdict
import csv
d = defaultdict(list)
#open the log file for preprocessing
f_input = open("C:/Users/Public/Documents/Edw_logs_800Mb/alert_edw01t1.log.old.20170703","r")
i = 0
key = ''
#for Redw log files prepare the documents as per delimiters
for line in f_input:
    i = i + 1
    if line.lower().startswith(("mon","tue","wed","thu","fri","sat","sun")):
        key = line.strip() + '_%d'%(i)
    else:
        if len(line.strip()) == 0:
            continue
        
        d[key].append(line.strip())
		
f_input.close()
#clean the documents by eliminating trailing spaces and empty documents
s =   {k: [elem.strip() for elem in v ] for k,v in d.items()}
s =   {k: list(filter(None,v)) for k,v in s.items()}
f = {k: ".".join(v) for k,v in s.items()}
fi = dict((k, v) for k, v in f.items() if v)
#save the preprocessed documents into a csv file		
with open("alert_edw01t1.log.old.20170703.csv",'w+') as output1:
    writer = csv.writer(output1)
    for key,value in fi.items():
        writer.writerow([key,value])