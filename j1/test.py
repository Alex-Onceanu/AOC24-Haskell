l1 = []
l2 = []

with open("input1.txt") as fichier:
    try:
        while True:
            ligne = fichier.readline()
            c1, c2 = ligne.split('    ')
            l1.append(int(c1))
            l2.append(int(c2))
    except:
        pass