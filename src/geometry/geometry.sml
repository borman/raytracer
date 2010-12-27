functor Geometry(R: EXTREAL) : GEOMETRY =
struct 
  structure Real = R

  local open R in
    type scalar = R.real
    type vector = scalar * scalar * scalar
    type ray = {origin: vector, direction: vector}
    
    val null = (zero, zero, zero)
    val x = (one, zero, zero)
    val y = (zero, one, zero)
    val z = (zero, zero, one)

    fun isNull (x, y, z) = isZero(x) andalso isZero(y) andalso isZero(z)

    (* Basic vector ops *)
    fun add ((x1, y1, z1), (x2, y2, z2)): vector = 
      (x1 + x2, y1 + y2, z1 + z2)

    fun neg (x, y, z): vector = 
      (~x, ~y, ~z)

    fun mul (k, (x, y, z)): vector =
      (k*x, k*y, k*z)
      
    fun ((x1, y1, z1) dot (x2, y2, z2)): scalar =
      x1*x2 + y1*y2 + z1*z2

    fun ((x1, y1, z1) cross (x2, y2, z2)): vector =
      (y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2)

    (* Compound vector ops *)
    fun sub (v1, v2) = add (v1, neg v2)
    fun sqlength v = v dot v
    fun length v = Math.sqrt (sqlength v)
    fun norm v = mul(inv (length v), v)

    (* Infix synonyms *)
    fun (a --> b) = sub (b, a)
    fun (a <-- b) = sub (a, b)
    fun (a +-> b) = add (a, b)
    fun (a *-> b) = mul (a, b)

    (* Parallellity/perpendicularity tests *)
    fun (a || b) = isNull(a cross b)
    fun (a -| b) = isZero(a dot b)

    fun ray (origin, target) = {
      origin = origin, 
      direction = norm (origin --> target)
    }
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
