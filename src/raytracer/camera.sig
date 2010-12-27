signature CAMERA = 
sig
  structure Geometry: GEOMETRY

  type coords = Geometry.scalar * Geometry.scalar
  type angle = Geometry.scalar

  datatype screen = Screen of coords * angle
  datatype camera = Camera of Geometry.ray * screen

  val projector: camera -> coords -> Geometry.ray
end

(* vim: set ft=sml tw=76 nowrap et: *)
