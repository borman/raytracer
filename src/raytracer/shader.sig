signature SHADER =
sig
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
   {ambient: real,
    ambientColor: Rgb.color,
    diffuse: real,
    diffuseColor: Rgb.color,
    shader: method,
    shininess: real,
    reflect: reflectiveness,
    refract: refractiveness,
    specular: real,
    specularColor: Rgb.color}

  type light = 
   {diffuse: real,
    point: Geometry.vector,
    specular: real}

  type hit = 
   {ambient: real,
    normal: Geometry.vector,
    toCamera: Geometry.vector,
    toLights: (Geometry.vector * light) list}

  val defaultMaterial: material

  val shade: material * hit -> Rgb.color
end

(* vim: set ft=sml tw=76 nowrap et: *)
