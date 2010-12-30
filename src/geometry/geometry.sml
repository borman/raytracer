structure Geometry:> GEOMETRY =
struct 
  local 
    open Real 
  in
    type vector = Vector3D.vector
    type ray = {origin: vector, direction: vector}
   
    (* Basic vector ops *)
    fun (a --> b) = Vector3D.sub (b, a)
    fun (a -- b) = Vector3D.sub (a, b)
    fun (a +-> b) = Vector3D.add (a, b)
    fun (a *-> b) = Vector3D.mul (a, b)
    fun (a dot b) = Vector3D.dot (a, b)
    fun (a cross b) = Vector3D.cross (a, b)
    fun neg v = Vector3D.neg v

    (* Compound vector ops *)
    fun sqlength v = v dot v
    fun length v = Math.sqrt (sqlength v)
    fun norm v = inv (length v) *-> v

    (* Check if a is nearer than b along dir *)
    fun nearer (dir, a, b) = dir dot (a --> b) > zero

    (* Parallellity/perpendicularity tests *)
    fun (a || b) = Vector3D.isNull (a cross b)
    fun (a -| b) = isZero (a dot b)

    fun reflect (vec, normal) = 
    let
      val d = (vec dot normal) *-> normal --> vec
    in
      vec -- two *-> d
    end

    fun ray (origin, target) = 
     {origin = origin, 
      direction = norm (origin --> target)}
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
