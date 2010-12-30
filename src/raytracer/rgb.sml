structure Rgb: RGB =
struct
  local 
    open Real 
  in
    type color = 
     {r: real,
      g: real,
      b: real}

    fun rgb (r, g, b): color = {r = r, g = g, b = b}

    fun map f {r, g, b} = rgb (f r, f g, f b)

    fun zip f ({r = r1, g = g1, b = b1}, {r = r2, g = g2, b = b2}) = 
      rgb (f (r1, r2), f (g1, g2), f (b1, b2))

    fun saturate r = max (zero, min (one, r))

    fun add (col1, col2) = 
      map saturate (zip (op +) (col1, col2))
    fun mul (k, col) = 
      map (fn x => saturate (k*x)) col

    val white = rgb (one, one, one)
    val black = rgb (zero, zero, zero)
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
