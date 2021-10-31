structure TGA = 
struct

(*сохраняем картинку в tga файл*)
fun saveTGA image filePath = let
    fun tgaHeader w h = map Word8.fromInt [
	    1, (*Image ID length*)
	    0, (*color map type *)
	    2, (*Image type*)
	    0, 0, 0, 0, 0, (*color map specification*)
	    0, 0, 0, 0, w, w div 256, h, h div 256, 24, 0, (*Image specification*)
	    0 (*Image ID*)];
    val out = BinIO.openOut filePath;
    val header = tgaHeader (Image.width image) (Image.height image)
    val body = rev(Image.fold (fn({c = {r = r, g = g, b = b}, ...} : Image.pixel, ls) => b :: g :: r :: ls) [] image)
in
    BinIO.output(out, Word8Vector.fromList(header @ body)) before BinIO.closeOut out
end;

(*грузи картинку из !!!несжатого!!! tga файла*)
fun loadTGA filePath = let	
    val input = BinIO.openIn filePath
    val data : Word8Vector.vector = BinIO.inputAll(input)	 
    val imageIDLenght = Word8.toInt(Word8Vector.sub (data, 0))
    val w = Word8.toInt (Word8Vector.sub (data, 12)) + Word8.toInt (Word8Vector.sub (data, 13)) * 256
    val h = Word8.toInt (Word8Vector.sub (data, 14)) + Word8.toInt (Word8Vector.sub (data, 15)) * 256
    val image = Image.newImage(w, h, Image.black);
    val l = Word8Vector.length data
    fun parsePixel i x y = let		
	val pos = i * 3 + 18 + imageIDLenght		
    in
	if pos + 2 < l
	then let
	    val color : Image.color = {r = Word8Vector.sub (data, pos), g = Word8Vector.sub (data, pos + 1), b = Word8Vector.sub (data, pos + 2)}
	    val (nextX, nextY) = if x = w - 1 then (0, y + 1) else (x + 1, y)
	in
	    Image.setPixel (image, x, y, {c = color, z = ~1.0}) before parsePixel (i + 1) nextX nextY
	end
	else ()
    end
			       
    val _ = (parsePixel 0 0 0) before BinIO.closeIn input
in
    image
end

end
