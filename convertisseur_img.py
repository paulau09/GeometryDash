from PIL import Image
from numpy import asarray

# Chargement de l'image
image = Image.open(f"Background-geometry-dash.jpg")
image = image.convert("RGB")
print(image.mode)
print(image.size)

# resizing the image
new_width = image.size[0] # output width
w_percent = (new_width/float(image.size[0]))
new_height = int((float(image.size[1])*float(w_percent)))
new_img = image.resize((new_width,new_height), Image.ANTIALIAS)

print(new_img.mode)
print(new_img.size)

# convert image to numpy array
data = asarray(new_img)

# converting to txt file
with open(f"background.txt", "w") as fichier: # name of output file (can be ID dependant)
    height, width = new_img.size
    fichier.write(f"{width}\n{height}\n")
    for k in range(width):
        for j in range(height):
            somme = 0
            for info in data[k][j]:
                somme *= 256
                somme += info
            fichier.write(str(somme))
            fichier.write(f"\n")