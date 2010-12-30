signature CAMERA = 
sig
  type coords = real * real
  datatype projection = Rectilinear

  type camera = 
   {angle: real,
    aspect: real,
    location: Geometry.vector,
    look_at: Geometry.vector,
    projection: projection}

  exception Direction

  val projector: camera -> coords -> Geometry.ray
end

(* vim: set ft=sml tw=76 nowrap et: *)
