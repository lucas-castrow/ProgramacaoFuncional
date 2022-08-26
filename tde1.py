import matplotlib.pyplot as plt
vet = []
with open("file.txt") as file:
    for line in file:
        vet.append(line.rstrip())

vet.remove('')
plt.plot(vet)
plt.show
#print(vet)
