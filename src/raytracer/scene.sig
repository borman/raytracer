signature SCENE =
sig
  type sphere = 
   {center: Geometry.vector, 
    radius: real}

  type plane = 
   {pivot: Geometry.vector, 
    normal: Geometry.vector}

  type viewcone =
   {source: Geometry.vector,
    topleft: Geometry.vector,
    topright: Geometry.vector,
    bottomleft: Geometry.vector,
    bottomright: Geometry.vector}

  datatype object =
      Sphere of sphere
    | Plane of plane
    | Group of object list
    | Material of Shader.material * object

  type collision = 
   {point: Geometry.vector, 
    normal: Geometry.vector,
    object: object,
    material: Shader.material}

  val intersect: Geometry.ray -> object -> collision option
  val cut: viewcone -> object -> object option
end

(* vim: set ft=sml tw=76 nowrap et: *)
