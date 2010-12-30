structure Vector3D:> VECTOR3D =
struct
  local
    open Real
  in
    type vector = real * real * real

    val null = (zero, zero, zero)
    val x = (one, zero, zero)
    val y = (zero, one, zero)
    val z = (zero, zero, one)

    fun isNull (x, y, z) = isZero x andalso isZero y andalso isZero z

    fun vector (x, y, z): vector = (x, y, z)
    fun unpack ((x, y, z): vector) = (x, y, z)

      (* Basic vector ops *)
    fun add ((x1, y1, z1), (x2, y2, z2)): vector = 
      (x1 + x2, y1 + y2, z1 + z2)

    fun sub ((x1, y1, z1), (x2, y2, z2)): vector = 
      (x1 - x2, y1 - y2, z1 - z2)

    fun neg (x, y, z): vector = 
      (~x, ~y, ~z)

    fun mul (k, (x, y, z)): vector =
      (k*x, k*y, k*z)

    fun ((x1, y1, z1) dot (x2, y2, z2)): real =
      x1*x2 + y1*y2 + z1*z2

    fun ((x1, y1, z1) cross (x2, y2, z2)): vector =
      (y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2)
  end
end
