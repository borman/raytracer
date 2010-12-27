signature SHADER =
sig
  structure Real: EXTREAL
  structure Rgb: RGB
  structure Geometry: GEOMETRY

  datatype reflectiveness 
    = Smooth
    | Glossy of Real.real
  datatype refractiveness
    = Opaque
    | Transparent of {
        transparency: Real.real,
        refraction: Real.real
        }
  datatype method = Phong
  type material = {
    shader: method,
    ambientColor: Rgb.color,
    diffuseColor: Rgb.color,
    ambient: Real.real,
    diffuse: Real.real,
    specular: Real.real,
    shininess: Real.real,
    reflect: reflectiveness,
    refract: refractiveness
    }
  type hit = {
    toCamera: Geometry.vector,
    toLight: Geometry.vector,
    normal: Geometry.vector
    }

  val shade: material * hit -> Rgb.color
end

(* vim: set ft=sml tw=76 nowrap et: *)
