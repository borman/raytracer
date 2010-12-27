functor Scene(structure G: GEOMETRY
              structure S: SHADER
              sharing S.Real = S.Rgb.Real = G.Real): SCENE =
struct
  structure Geometry = G
  structure Shader = S

  local open G; open S; open Real in
    type sphere = {
      center: vector, 
      radius: scalar
      }
    type plane = {
      pivot: vector, 
      normal: vector
      }
    datatype object 
      = Sphere of sphere
      | Plane of plane
      | Group of (object list)
      | Material of material * object

    type collision = {
      point: vector, 
      normal: vector,
      object: object,
      material: S.material
      }

    fun intersect ray scene = 
    let 
      val {origin, direction} = ray;
      val baseMtl: material = {
        shader = Phong,
        ambientColor = {r = one, g = one, b = one},
        ambient = zero,
        diffuseColor = {r = one, g = one, b = one},
        diffuse = one,
        specularColor = {r = one, g = one, b = one},
        specular = zero,
        shininess = zero,
        reflect = Smooth,
        refract = Opaque
        };

      fun visible collision: collision option =
        if ((origin --> #point collision) dot direction) > zero then
          SOME collision
        else
          NONE

      fun nearest (NONE, opt): collision option = opt
        | nearest (opt, NONE) = opt
        | nearest (SOME x, SOME y) = SOME (
          if (direction dot (#point x --> #point y)) > zero then
            x 
          else 
            y
          )


      (* Sphere *)
      fun hit (mtl, sphere as Sphere {center, radius}) = 
        let
          val v = direction; (* along the ray; normalized *)
          val p = origin +-> (((origin -->center) dot v) *-> v); (* closest to the center *)
          val d_sq = sqlength (center --> p) (* distance to the center *)
        in
          if d_sq > radius*radius then
            NONE
          else
            let
              fun cat (r, c_sq) = Math.sqrt (r*r - c_sq); (* find a cathetus *)
              fun shifted len = p +-> (len *-> v)

              val off = cat (radius, d_sq); (* distance to intersections *)
              val p1 = shifted (~off)
              and p2 = shifted (off)

              fun normal p = norm (center --> p)
              fun coll p = visible {
                point = p,
                normal = normal p,
                object = sphere,
                material = mtl
                }
            in
              nearest (coll p1, coll p2)
            end
        end

      (* Infinite plane *)
        | hit (mtl, plane as Plane {pivot, normal}) =
        if direction -| normal then
          NONE
        else
          let 
            val k = ((origin --> pivot) dot normal) / (direction dot normal);
          in
            visible {
              point = origin +-> (k*->direction),
              normal = normal,
              object = plane,
              material = mtl
            }
          end

      (* Object group *)
        | hit (mtl, Group subscenes) = 
        foldl
          (fn (curr, acc) => nearest(hit (mtl, curr), acc))
          NONE
          subscenes

      (* Material application *)
        | hit (_, Material (mtl, subscene)) = hit (mtl, subscene)
    in
      hit (baseMtl, scene)
    end
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
