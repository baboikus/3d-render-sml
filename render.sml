use "files.sml";

val screenWidth = 1024;
val screenHeight = 1024;

val testImage : Image.image = Image.newImage(screenWidth, screenHeight, Image.black);
val testObj = OBJ.loadOBJFile "./african_head.obj";  (*тут можно указать, откуда брать obj файл*)
val texture = TGA.loadTGA "./no_compresion_texture.tga"; (*а тут, откуда брать tga текстуру*)
val camera = {
    from = {x = ~1.0, y = 0.0, z = ~1.0}, (*откуда смотрит камера*)
    to = {x = 0.0, y = 0.0, z = 0.0},  (*куда смотрит камера*)
    top = {x = 0.0, y = 1.0, z = 0.0}  (*направление "верх" для камеры*)
};

   

Draw.drawObj testImage testObj texture camera;
TGA.saveTGA testImage "./8.tga";
