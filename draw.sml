structure Draw = 
struct

(*это просто служебная функция для уменьшения дублей в коде*)
fun app3 f (a, b, c) = (f a, f b, f c)

(*для краткости и "математичности" кода будем использовать операторы для матриц и векторов*)
infix 7 <-> fun u <-> v = Vector3d.sub u v
infix 7 <+> fun u <+> v = Vector3d.add u v
infix 8 <* fun v <* k = Vector3d.scale v k
infix 8 *> fun k *> v = v <*k
infix 8 <*> fun u <*> v = Vector3d.scalar u v
infix 8 |*|  fun m1 |*| m2 = Matrix.product m1 m2

(*набор функций для вращения модели, сдвига модели и переспективы*)
structure Transform = struct

fun perspectiveDistortion ({x = x, y = y, z = z} : Vector3d.real) (c : Real.real) : Vector3d.real = let
	val dm = [[1.0, 0.0, 0.0, 0.0],
		  [0.0, 1.0, 0.0, 0.0],
		  [0.0, 0.0, 1.0, 0.0],
		  [0.0, 0.0, ~1.0 / c, 1.0]]
	val vm = [[x, y, z, 1.0]]
	val [[x'], [y'], [z'], [k]] = dm |*| vm
    in
	(*{x = x' / k, y = y' / k, z = z' / k}*)
	{x = x, y = y, z = z}
    end

fun moveCamera 
	({x = x, y = y, z = z} : Vector3d.real) 
	{from = {x = ex, y = ey, z = ez} : Vector3d.real, 
	 to ={x = cx, y = cy, z = cz} : Vector3d.real,
	 top = {x = ux, y = uy, z = uz} : Vector3d.real } = let
    val {x = a31, y = a32, z = a33} : Vector3d.real = Vector3d.normalize {x = ex - cx, y = ey - cy, z = ez - cz}
    val {x = a11, y = a12, z = a13} : Vector3d.real = Vector3d.normalize {x = uy * a33 - uz * a32, y = uz * a31 - ux * a33, z = ux * a32 - uy * a31}
    val {x = a21, y = a22, z = a23} = Vector3d.normalize {x = a32 * a13 - a33 * a12, y = a33 * a11 - a31 * a13, z = a31 * a12 - a32 * a11}
    val m = [[a11, a12, a13, 0.0],
	     [a21, a22, a23, 0.0],
	     [a31, a32, a33, 0.0],
	     [0.0, 0.0, 0.0, 1.0]]
    val tr = [[1.0, 0.0, 0.0, ~cx],
	      [0.0, 1.0, 0.0, ~cy],
	      [0.0, 0.0, 1.0, ~cz],
	      [0.0, 0.0, 0.0, 1.0]]
    val [[x'], [y'], [z'], _] = (m |*| tr) |*| [[x, y, z, 1.0]]
in
    {x = x', y = y', z = z'}	
end

end

(*описываем предметную область*)
(*вершина в модели состоит из координат в пространстве, текстурных координат и вектора нормали*)
type vertex = {geometric : Vector3d.real, texture : Vector3d.real, normal : Vector3d.real}
(*полигон состоит из трёх вершин в модели*)
type face = {vertex1 : vertex, vertex2 : vertex, vertex3 : vertex, normal : Vector3d.real}
(*модель состоит из списка полигонов*)
type model = {faces : face list}

(*рисуем один полигон*)
fun drawFace 
	(image : Image.image) (*картинка, на которую всё рисуем*)
	({vertex1 = v1, vertex2 = v2, vertex3 = v3, normal = n} : face) (*сам полигон*)
	(texture : Image.image)  (*текстура для модели*)
	{from = from, to = to, top = top} = (*откуда смотрим на модель + источник света*)
    let 
	(*готовим данные *)
	val vs3 = (v1, v2, v3)
	val (gv1, gv2, gv3) = app3 #geometric vs3 (*координаты вершин*)
	val (tv1, tv2, tv3) = app3 #texture vs3 (*координаты тексту*)
	val (nv1, nv2, nv3) = app3 #normal vs3 (*векторы нормалей*)
	val (l1, l2, l3) = app3 (fn v => #normal v <*> from) vs3 (*находим степень освещённости вершин*)
	val (y1, y2, y3) = app3 (fn v => Real.round (#y v)) (gv1, gv2, gv3)	(*получаем y координаты вершин для отрисовки треугольника*)
	val height = y3 - y1 (*общая высота треугольника*)
	val (textureWidth, textureHeight) = (Real.fromInt (Image.width texture), Real.fromInt (Image.height texture)) (*размеры текстуры*)

 	(*рисуем части треугольника*)
	fun dhl y = let
	    val isTopTr = y1 = y2 orelse y > (y2 - y1) (*определяем, какую часть треугольника нужно рисовать*)
	    val trh = Real.fromInt (if isTopTr then y3 - y2 else y2 - y1) (*высота текущей части треугольника*)

	    (*дальше через интерполяцию находим куда нужно ставить пиксель на картинке,
		откуда брать цвет пикселя из текстуры и степень его освещённости*)
	    val (k1, k2) = (Real.fromInt y / Real.fromInt height, (Real.fromInt y - (if isTopTr then Real.fromInt (y2 - y1) else 0.0)) / trh)
	    fun vectorInterpolate i1 i2 i3 = ((i3 <-> i1) <* k1 <+> i1, if isTopTr then i2 <+> (i3 <-> i2) <* k2 else i1 <+> (i2 <-> i1) <* k2)
	    fun realInterpolate p1 p2 p3 = ((p3 - p1) * k1 + p1, if isTopTr then p2 + (p3 - p2) * k2 else p1 + (p2 - p1) * k2)

	    val (a, b) = vectorInterpolate gv1 gv2 gv3
	    val (ax, bx) = if #x a > #x b then (Real.round (#x b), Real.round (#x a)) else (Real.round (#x a), Real.round (#x b))
	    val (la, lb) = realInterpolate l1 l2 l3
	    val (ta, tb) = vectorInterpolate tv1 tv2 tv3

	    fun drawPixels x = let
		val phi = if ax = bx then 1.0 else Real.fromInt (x - ax) / Real.fromInt (bx - ax)
		val {z = z, ...} = a <+> ((b <-> a) <* phi)

		val (l, {x = tx, y = ty, ...}) = if #x a > #x b
						 then (lb + (la - lb) * phi, tb <+> (ta <-> tb) <* phi)
						 else (la + (lb - la) * phi, ta <+> (tb <-> ta) <* phi)			
							  
		val color = Image.lightColor
				(Image.genGray 255)
				(*(#c (Image.getPixel (texture, Real.round (tx * textureWidth) - 1, Real.round (ty * textureHeight) - 1))) *)
				(Real.max (0.0, Real.min (l, 1.0)))
	    in 
		if x <= bx 
		then Image.setPixel (image, x, y + y1, {c = color, z = z}) before drawPixels (x + 1)
		else ()
	    end
	in
	    if y < height then drawPixels ax before dhl (y + 1) else ()
	end
    in 
	if y1 = y2 andalso y3 = y2 then () (*вырожденные треугольники не рисуем*)
	else if (from <*> ((nv1 <+> nv2 <+> nv3) <* (1.0 / 3.0))) <= 0.0 then () (*если треугольник не видно, то тоже его не рисуем*)
	else let
	    val vs = if y1 > y2 then Option.SOME {vertex1 = v2, vertex2 = v1, vertex3 = v3, normal = n}
		     else if y1 > y3 then Option.SOME  {vertex1 = v3, vertex2 = v2, vertex3 = v1, normal = n}
		     else if y2 > y3 then  Option.SOME  {vertex1 = v1, vertex2 = v3, vertex3 = v2, normal = n}
		     else NONE 
	in if Option.isSome vs then drawFace image (valOf vs) texture {from = from, to = to, top = top} else dhl 0
	end
    end

(*рисуем весь объект на картинке*)
fun drawObj image (OBJ.Obj(v, vt, vn, faces)) texture camera = let
    val {data = arr} = image
    val width = Real.fromInt (Array2.nCols arr)
    val height = Real.fromInt (Array2.nRows arr)	
    val depth = 1024.0

    fun vector (x, y, z) : Vector3d.real = {x = x, y = y, z = z}
    fun project ({x = x, y = y, z = z} : Vector3d.real) : Vector3d.real = {x = (x + 1.0) * width / 2.0, y = (y + 1.0) * height / 2.0, z = (z + 1.0) * depth / 2.0}

    fun df ((vi1, vti1, vni1), (vi2, vti2, vni2), (vi3, vti3, vni3)) = let
	val pk = Math.sqrt ((#from camera <-> #to camera) <*> (#from camera <-> #to camera))
	fun transform v = project (Transform.perspectiveDistortion (Transform.moveCamera v camera) pk)

	val (tv1, tv2, tv3) = app3 (fn vti => vector (List.nth(vt, vti - 1))) (vti1, vti2, vti3)
	val (nv1, nv2, nv3) = app3 (fn vni => vector (List.nth(vn, vni - 1))) (vni1, vni2, vni3)
	val (v1, v2, v3) = app3 (fn vi1 => transform (vector (List.nth(v, vi1 - 1)))) (vi1, vi2, vi3)
    in
	drawFace
	    image
	    ({vertex1 = {geometric = v1, texture = tv1, normal = nv1},
	      vertex2 = {geometric = v2, texture = tv2, normal = nv2},
	      vertex3 = {geometric = v3, texture = tv3, normal = nv3},
	      normal = {x = 0.0, y = 0.0, z = 0.0}} : face)
	    texture
	    camera
    end
in
    List.app df faces
end	

end
