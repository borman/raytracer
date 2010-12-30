signature CAMERA = 
sig
  type coords = Geometry.scalar * Geometry.scalar
  datatype projection = Rectilinear

  type camera = 
   {angle: Geometry.scalar,
    aspect: Geometry.scalar,
    location: Geometry.vector,
    look_at: Geometry.vector,
    projection: projection}

  exception Direction

  val projector: camera -> coords -> Geometry.ray
end

(* vim: set ft=sml tw=76 nowrap et: *)
