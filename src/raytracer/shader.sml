functor Shader (structure C: RGB
                structure G: GEOMETRY): SHADER =
struct
  structure Rgb = C
  structure Geometry = G

  local open G; open C; open Real in
    datatype reflectiveness 
      = Dull
      | Glossy of scalar
    datatype refractiveness
      = Opaque
      | Transparent of {
          transparency: scalar,
          refraction: scalar
          }
    datatype diffuseMethod = Lambert
    datatype specularMethod = Phong | Blinn
    type method = diffuseMethod * specularMethod
    type material = {
      shader: method,
      ambientColor: color,
      diffuseColor: color,
      specularColor: color,
      ambient: scalar,
      diffuse: scalar,
      specular: scalar,
      shininess: scalar,
      reflect: reflectiveness,
      refract: refractiveness
      }
    type light = {
      point: vector,
      diffuse: scalar,
      specular: scalar
    }
    type hit = {
      ambient: scalar,
      toCamera: vector,
      toLights: (vector * light) list,
      normal: vector
      }

    val defaultMaterial: material = {
      shader = (Lambert, Phong),
      ambientColor = {r = one, g = one, b = one},
      ambient = zero,
      diffuseColor = {r = one, g = one, b = one},
      diffuse = one,
      specularColor = {r = one, g = one, b = one},
      specular = zero,
      shininess = zero,
      reflect = Dull,
      refract = Opaque
      };

    local
      fun accumulate shadeFunc lights = foldl
          (fn (toLight, acc) => acc + shadeFunc toLight)
          zero
          lights

      (* Ambient lighting: trivial *)
      fun ambient (mtl: material, hit: hit) =
        #ambient hit;

      (* Diffuse lighting *)
      (* Lambert model *)
      fun lambert (mtl: material, hit: hit) =
      let
        fun diffusePart (toLight, light) = 
          (#diffuse light) * (toLight dot (#normal hit))
      in
        accumulate diffusePart (#toLights hit)
      end

      (* Specular lighting *)
      local
        fun phongFamily specular (mtl: material, hit: hit) =
        let
          val {normal, toCamera, ...} = hit
          fun specularPart (toLight, light) = (#specular light) * Math.pow (
            specular (toLight, normal, toCamera), 
            #shininess mtl
            )
        in
          accumulate specularPart (#toLights hit)
        end

        (* Phong model *)
        fun phongSpecular (toLight, normal, toCamera) =
        let
          fun reflect (vec, normal) = 
          let
            val d = ((vec dot normal) *-> normal) --> vec;
          in
            vec <-- (two *-> d)
          end
        in
          (reflect (toLight, normal)) dot toCamera
        end

        (* Blinn model *)
        fun blinnSpecular (toLight, normal, toCamera) =
        let
          fun bisect (vec, normal) =
            norm (vec +-> normal)
        in
          (bisect (toLight, toCamera)) dot normal
        end
      in
        val phong = phongFamily phongSpecular;  
        val blinn = phongFamily blinnSpecular;  
      end
    in
      fun shade (mtl: material, hit) = 
      let
        val (diffuseMethod, specularMethod) = #shader mtl;
        val diffuse = (case diffuseMethod of
                            Lambert => lambert
                            );
        val specular = (case specularMethod of
                             Phong => phong
                           | Blinn => blinn
                             );
      in
        Rgb.add (
          Rgb.add (
            Rgb.mul (#ambient mtl * ambient (mtl, hit), #ambientColor mtl),
            Rgb.mul (#diffuse mtl * diffuse (mtl, hit), #diffuseColor mtl)
            ),
          Rgb.mul (#specular mtl * specular (mtl, hit), #specularColor mtl)
          )
      end
    end
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
