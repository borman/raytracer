signature SHADER =
sig
  structure Real: EXTREAL
  structure Rgb: RGB
  structure Geometry: GEOMETRY

  datatype reflectiveness 
    = Dull
    | Glossy of Real.real
  datatype refractiveness
    = Opaque
    | Transparent of {
        transparency: Real.real,
        refraction: Real.real
        }
  datatype diffuseMethod = Lambert
  datatype specularMethod = Phong | Blinn
  type method = diffuseMethod * specularMethod
  type material = {
    shader: method,
    ambientColor: Rgb.color,
    diffuseColor: Rgb.color,
    specularColor: Rgb.color,
    ambient: Real.real,
    diffuse: Real.real,
    specular: Real.real,
    shininess: Real.real,
    reflect: reflectiveness,
    refract: refractiveness
    }
  type hit = {
    ambient: Real.real,
    toCamera: Geometry.vector,
    toLights: Geometry.vector list,
    normal: Geometry.vector
    }

  val defaultMaterial: material

  val shade: material * hit -> Rgb.color
end

(* vim: set ft=sml tw=76 nowrap et: *)
