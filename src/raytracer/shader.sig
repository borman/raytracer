signature SHADER =
sig
  structure Rgb: RGB
  structure Geometry: GEOMETRY

  datatype reflectiveness 
    = Dull
    | Glossy of Geometry.scalar
  datatype refractiveness
    = Opaque
    | Transparent of {
        transparency: Geometry.scalar,
        refraction: Geometry.scalar
        }
  datatype diffuseMethod = Lambert
  datatype specularMethod = Phong | Blinn
  type method = diffuseMethod * specularMethod
  type material = {
    shader: method,
    ambientColor: Rgb.color,
    diffuseColor: Rgb.color,
    specularColor: Rgb.color,
    ambient: Geometry.scalar,
    diffuse: Geometry.scalar,
    specular: Geometry.scalar,
    shininess: Geometry.scalar,
    reflect: reflectiveness,
    refract: refractiveness
    }
  type light = {
    point: Geometry.vector,
    diffuse: Geometry.scalar,
    specular: Geometry.scalar
    }
  type hit = {
    ambient: Geometry.scalar,
    toCamera: Geometry.vector,
    toLights: (Geometry.vector * light) list,
    normal: Geometry.vector
    }

  val defaultMaterial: material

  val shade: material * hit -> Rgb.color
end

(* vim: set ft=sml tw=76 nowrap et: *)
