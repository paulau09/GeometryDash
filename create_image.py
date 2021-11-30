import numpy as np
import matplotlib.pyplot as plt
from PIL import Image

fname = 'buisson_agarml2.png'
image = Image.open(fname).convert("RGB")
arr = np.asarray(image)
print(arr.shape)

fichier=open('test.txt', "a")
for i in range (196):
    for j in range(196):
        
        fichier.write(str(arr[i][j][0]))
        fichier.write(' ')
        fichier.write(str(arr[i][j][0]))
        fichier.write(' ')
        fichier.write(str(arr[i][j][0]))
        fichier.write('\n')
    fichier.write('\n')
    fichier.write('\n')


fichier.close()
