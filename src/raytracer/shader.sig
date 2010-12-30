signature SHADER =
sig
  datatype reflectiveness =
      Dull
    | Glossy of Geometry.scalar

  datatype refractiveness =
      Opaque
    | Transparent of 
       {transparency: Geometry.scalar,
        refraction: Geometry.scalar}
        
  datatype diffuseMethod = Lambert
  datatype specularMethod = Phong | Blinn
  type method = diffuseMethod * specularMethod

  type material = 
   {ambient: Geometry.scalar,
    ambientColor: Rgb.color,
    diffuse: Geometry.scalar,
    diffuseColor: Rgb.color,
    shader: method,
    shininess: Geometry.scalar,
    reflect: reflectiveness,
    refract: refractiveness,
    specular: Geometry.scalar,
    specularColor: Rgb.color}

  type light = 
   {diffuse: Geometry.scalar,
    point: Geometry.vector,
    specular: Geometry.scalar}

  type hit = 
   {ambient: Geometry.scalar,
    normal: Geometry.vector,
    toCamera: Geometry.vector,
    toLights: (Geometry.vector * light) list}

  val defaultMaterial: material

  val shade: material * hit -> Rgb.color
end

(* vim: set ft=sml tw=76 nowrap et: *)
