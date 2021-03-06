structure Shader: SHADER =
struct
  local 
    open Geometry 
    open Rgb
    open Real 
  in
    datatype reflectiveness =
        Dull
      | Glossy of real

    datatype refractiveness =
        Opaque
      | Transparent of 
         {transparency: real,
          refraction: real}

    datatype diffuseMethod = Lambert
    datatype specularMethod = Phong | Blinn
    type method = diffuseMethod * specularMethod

    type material = 
     {shader: method,
      ambientColor: color,
      diffuseColor: color,
      specularColor: color,
      ambient: real,
      diffuse: real,
      specular: real,
      shininess: real,
      reflect: reflectiveness,
      refract: refractiveness}

    type light = 
     {point: vector,
      diffuse: real,
      specular: real}

    type hit = 
     {ambient: real,
      toCamera: vector,
      toLights: (vector * light) list,
      normal: vector}

    val defaultMaterial: material = 
     {shader = (Lambert, Phong),
      ambientColor = white,
      ambient = zero,
      diffuseColor = white,
      diffuse = one,
      specularColor = white,
      specular = zero,
      shininess = zero,
      reflect = Dull,
      refract = Opaque}

    local
      fun accumulate shadeFunc lights = foldl
          (fn (toLight, acc) => acc + shadeFunc toLight)
          zero
          lights

      (* Ambient lighting: trivial *)
      fun ambient (mtl: material, hit: hit) =
        #ambient hit

      (* Diffuse lighting *)
      (* Lambert model *)
      fun lambert (mtl: material, hit: hit) =
      let
        fun diffusePart (toLight, light) = 
          #diffuse light * (toLight dot #normal hit)
      in
        accumulate diffusePart (#toLights hit)
      end

      (* Specular lighting *)
      local
        fun phongFamily specular (mtl: material, hit: hit) =
        let
          val {normal, toCamera, ...} = hit
          fun specularPart (toLight, light) = #specular light * Math.pow 
           (specular (toLight, normal, toCamera), 
            #shininess mtl)
        in
          accumulate specularPart (#toLights hit)
        end

        (* Phong model *)
        fun phongSpecular (toLight, normal, toCamera) =
          (reflect (toLight, normal)) dot toCamera

        (* Blinn model *)
        fun blinnSpecular (toLight, normal, toCamera) =
        let
          fun bissect (vec, normal) =
            norm (vec +-> normal)
        in
          (bissect (toLight, toCamera)) dot normal
        end
      in
        val phong = phongFamily phongSpecular  
        val blinn = phongFamily blinnSpecular  
      end
    in
      fun shade (mtl: material, hit) = 
      let
        val (diffuseMethod, specularMethod) = #shader mtl
        val diffuse = (case diffuseMethod of
                            Lambert => lambert
                            )
        val specular = (case specularMethod of
                             Phong => phong
                           | Blinn => blinn
                             )
      in
        Rgb.add 
         (Rgb.add 
           (Rgb.mul (#ambient mtl * ambient (mtl, hit), #ambientColor mtl),
            Rgb.mul (#diffuse mtl * diffuse (mtl, hit), #diffuseColor mtl)),
          Rgb.mul (#specular mtl * specular (mtl, hit), #specularColor mtl))
      end
    end
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
