
(*операции с трёхмерными векторами*)
structure Vector3d = struct

type int = {x : int, y : int, z : int}
type real = {x : Real.real, y : Real.real, z : Real.real}

(*общая функция, через которую удобно выражать популярные операторы над векторами*)
fun vectorOp agg sum ({x = x1, y = y1, z = z1} : real) {x = x2, y = y2, z = z2} = let
    fun f a1 a2 = (sum (a1, a2))
in
    agg {x = f x1 x2, y = f y1 y2, z = f z1 z2}
end

fun sub u v = vectorOp (fn x => x) (fn (x, y) => x - y) u v
fun add u v = vectorOp (fn x => x)  (fn (x, y) => x + y) u v
fun scale v k = vectorOp (fn x => x) (fn (x, _) => x  * k) v v
fun scalar u v = vectorOp (fn {x = x, y = y, z = z} => x + y + z) (fn (x, y) => x * y) u v

fun normalize {x = x, y = y, z = z} : real = let 
	val v : Real.real = x * x + y * y + z * z
    in 
	if Real.== (v, 0.0)
	then {x = 0.0, y = 0.0, z = 0.0} : real
	else {x = x / v, y = y / v, z = z / v} : real
    end

fun printVector3d {x = x, y = y, z = z} = print ("x = " ^ Real.toString x ^ " y = " ^ Real.toString y ^ " z = " ^ Real.toString z ^ "\n")

end

(*операции с матрицами*)
structure Matrix = struct

val zero = 0.0

fun product m1 m2 = let
    fun join [] _ = []
      | join _ [] = []
      | join (x :: xs) (y :: ys) = (x, y) :: join xs ys

    fun product row column = List.foldl (fn ((r, c), s) => r * c + s) zero (join row column)
in
    List.map (fn row => List.map (fn column => product row column) m2) m1
end

end
