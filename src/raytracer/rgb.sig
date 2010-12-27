signature RGB =
sig
  type color = {
    r: Real.real,
    g: Real.real,
    b: Real.real
    }

  val map: (Real.real -> Real.real) -> color -> color
  val zip: (Real.real * Real.real -> Real.real) -> color * color -> color
  val add: color * color -> color
  val mul: Real.real * color -> color
end

(* vim: set ft=sml tw=76 nowrap et: *)
