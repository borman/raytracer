functor Shader (structure R: EXTREAL
                structure C: RGB
                structure G: GEOMETRY
                sharing G.Real = C.Real = R): SHADER =
struct
  structure Real = R
  structure Rgb = C
  structure Geometry = G

  local open G; open C; open R in
    datatype reflectiveness 
      = Smooth
      | Glossy of real
    datatype refractiveness
      = Opaque
      | Transparent of {
          transparency: real,
          refraction: real
          }
    datatype method = Phong
    type material = {
      shader: method,
      ambientColor: color,
      diffuseColor: color,
      ambient: real,
      diffuse: real,
      specular: real,
      shininess: real,
      reflect: reflectiveness,
      refract: refractiveness
      }
    type hit = {
      toCamera: vector,
      toLight: vector,
      normal: vector
      }

    local
      fun phong (mtl: material, hit: hit) = #ambientColor mtl
    in
      fun shade (mtl: material, hit) = 
        case #shader mtl of
             Phong => phong (mtl, hit)
    end
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
