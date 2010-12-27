functor Rgb(R: EXTREAL): RGB =
struct
  structure Real = R

  local open R in
    type color = {
      r: real,
      g: real,
      b: real
      }

    fun map f {r, g, b} = 
      {r=f r, g=f g, b=f b}
    fun zip f ({r=r1, g=g1, b=b1}, {r=r2, g=g2, b=b2}) =
      {r=f (r1, r2), g=f (g1, g2), b=f (b1, b2)}

    fun saturate r = max (zero, min (one, r))

    fun add (col1, col2) = 
      map saturate (zip (op +) (col1, col2))
    fun mul (k, col) = 
      map (saturate o (fn x => k*x)) col
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
