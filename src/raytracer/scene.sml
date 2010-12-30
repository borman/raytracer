functor Scene(structure G: GEOMETRY
              structure S: SHADER): SCENE =
struct
  structure Geometry = G
  structure Shader = S

  local 
    open Geometry
    open Shader 
    open Real 
  in
    type sphere = 
     {center: vector, 
      radius: scalar}
    type plane = 
     {pivot: vector, 
      normal: vector}

    datatype object =
        Sphere of sphere
      | Plane of plane
      | Group of object list
      | Material of material * object

    type collision = 
     {point: vector, 
      normal: vector,
      object: object,
      material: Shader.material}

    (* Collisions nearer than sqrt(cutoff) will be ignored *)
    val cutoff_sq = 1E~10

    fun intersect ray scene = 
    let 
      val {origin, direction} = ray

      fun visible collision: collision option =
        if (nearer (direction, origin, #point collision)) 
           andalso (sqlength (origin --> #point collision) > cutoff_sq) 
          then SOME collision
        else NONE

      fun nearest (NONE, opt): collision option = opt
        | nearest (opt, NONE) = opt
        | nearest (SOME x, SOME y) = SOME 
         (if nearer (direction, #point x, #point y) 
            then x 
          else y)


      (* Sphere *)
      fun hit (mtl, object as Sphere {center, radius}) = 
        let
          val v = direction (* along the ray; normalized *)
          val p = origin +-> ((origin -->center) dot v) *-> v (* closest to the center *)
          val d_sq = sqlength (center --> p) (* distance to the center *)
        in
          if d_sq > radius*radius 
            then NONE
          else
            let
              fun cathetus (r, c_sq) = Math.sqrt (r*r - c_sq) (* find a cathetus *)
              fun shifted len = p +-> (len *-> v)

              val off = cathetus (radius, d_sq) (* distance to intersections *)
              val p1 = shifted (~off)
              and p2 = shifted (off)

              fun normalAt p = norm (center --> p)
              fun makeCollision p = visible 
               {point = p,
                normal = normalAt p,
                object = object,
                material = mtl}
            in
              nearest (makeCollision p1, makeCollision p2)
            end
        end

      (* Infinite plane *)
        | hit (mtl, object as Plane {pivot, normal}) =
        if direction -| normal 
          then NONE
        else
          let 
            val k = ((origin --> pivot) dot normal) / (direction dot normal)
          in
            visible {
              point = origin +-> k *-> direction,
              normal = normal,
              object = object,
              material = mtl}
          end

      (* Object group *)
        | hit (mtl, Group subscenes) = 
        foldl
          (fn (curr, acc) => nearest (hit (mtl, curr), acc))
          NONE
          subscenes

      (* Material application *)
        | hit (_, Material (mtl, subscene)) = hit (mtl, subscene)
    in
      hit (defaultMaterial, scene)
    end
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
