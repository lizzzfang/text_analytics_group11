import csv

total = 0
correct = 0
isFirst = True

with open("imdb_master.csv", "r") as csvFile:
    reader = csv.reader(csvFile)
    for row in reader:
        if isFirst:
            isFirst = False
            continue

        currentRating = row[3]
        posCounter = row[5]
        negCounter = row[5]

        actualRating = False
        if currentRating == 'pos':
            actualRating = True
        predictedPositive = False
        if(int(posCounter) > int(negCounter)):
            predictedPositive = True

        if actualRating == predictedPositive:
            correct += 1

        total += 1

print("Predicted :"+ str(correct*100/total))
print("Total :"+ str(total))