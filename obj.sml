structure OBJ = 
struct

(*описываем внутренее представление obj формата*)
type GeometricVertex = (real*real*real);
type TextureCoordinate = (real*real*real);
type VertexNormal = (real*real*real);
type PolygonalFaceElement = (int*int*int);
type PolygonalFace = (PolygonalFaceElement*PolygonalFaceElement*PolygonalFaceElement);

datatype Obj = Obj of (GeometricVertex list*TextureCoordinate list*VertexNormal list*PolygonalFace list);

(*грузим построчно и парсим obj файл*)
fun loadOBJFile fileName = let
    fun parcePolygonalFaceElement line = 
	case String.tokens (fn c => c = #"/") line of
	    (vi :: vti :: vni :: _) => (valOf(Int.fromString vi), valOf(Int.fromString vti), valOf(Int.fromString vni))
	  | (vi :: _) 			  => (valOf(Int.fromString vi), valOf(Int.fromString vi), valOf(Int.fromString vi))
	  | _						  => (0, 0, 0)

    fun parseTokens ["v", x, y, z] (Obj(v, vt, vn, f)) = Obj((valOf(Real.fromString  x), valOf(Real.fromString y), valOf(Real.fromString z)) :: v, vt, vn, f)
      | parseTokens ["vt", x, y, z] (Obj(v, vt, vn, f)) = Obj(v, (valOf(Real.fromString  x), valOf(Real.fromString y), valOf(Real.fromString z)) :: vt, vn, f)
      | parseTokens ["vn", x, y, z] (Obj(v, vt, vn, f)) 	   = Obj(v, vt, (valOf(Real.fromString  x), valOf(Real.fromString y), valOf(Real.fromString z)) :: vn, f)
      | parseTokens ["f", pfe1, pfe2, pfe3] (Obj(v, vt, vn, f)) = Obj(v, vt, vn, (parcePolygonalFaceElement pfe1, parcePolygonalFaceElement pfe2, parcePolygonalFaceElement pfe3) :: f)
      | parseTokens _ obj					           		   = obj

    fun parseLine line obj =  parseTokens (String.tokens (fn c => c = #" ") line) obj

    fun loop content obj =
	case TextIO.inputLine content of 
	    SOME line => loop content (parseLine line obj)
	  | NONE		=> obj

    val input = TextIO.openIn fileName
    val Obj(v, vt, vn, f) = loop input (Obj([], [], [], []))
in
    Obj(List.rev v, List.rev vt, List.rev vn, List.rev f) before TextIO.closeIn input 
end;

end
