import csv

data = []
text = []
i = 0
with open('data.csv', newline='') as csvfile:
    spamreader = csv.reader(csvfile, delimiter=',', quotechar='|')
    for row in spamreader:
        if i >= 2 and row[3]:
            data.append((row[1], row[3]))
            text.append("attk[\"{}\"]={};".format(row[3], row[1]))
        i += 1

print("let attk = {};")
print("".join(text))