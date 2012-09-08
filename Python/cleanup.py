import csv
import re

loadReader = csv.reader(open('../input/Load_history.csv', 'rb'), delimiter=',')

for row in loadReader:
    print ','.join([re.sub('[",]', '', x) for x in row])
