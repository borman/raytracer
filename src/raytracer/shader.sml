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
      specularColor: color,
      ambient: real,
      diffuse: real,
      specular: real,
      shininess: real,
      reflect: reflectiveness,
      refract: refractiveness
      }
    type hit = {
      ambient: real,
      toCamera: vector,
      toLights: vector list,
      normal: vector
      }

    local
      fun phong (mtl: material, hit: hit) =
      let
        val ambient = #ambient mtl * #ambient hit;
        val {
          normal, 
          toCamera, 
          toLights, 
          ...} = hit;
        val {
          diffuse=matDiffuse, 
          specular=matSpecular,
          shininess=matShininess, 
          ...} = mtl;

        fun calcDiffuse toLight = matDiffuse * (toLight dot normal);
        val diffuse = foldl
          (fn (toLight, acc) => acc + calcDiffuse toLight)
          zero
          toLights

        fun reflect (vec, normal) = 
        let
          val d = ((vec dot normal) *-> normal) --> vec;
        in
          vec <-- (two *-> d)
        end;
        fun calcSpecular toLight = matSpecular * 
            Math.pow (
              (reflect (toLight, normal)) dot toCamera, 
              matShininess
              );
        val specular = foldl
          (fn (toLight, acc) => acc + calcSpecular toLight)
          zero
          toLights
      in
        Rgb.add (
          Rgb.add (
            Rgb.mul (ambient, #ambientColor mtl), 
            Rgb.mul (diffuse, #diffuseColor mtl)
            ),
          Rgb.mul (specular, #specularColor mtl)
          )
      end
    in
      fun shade (mtl: material, hit) = 
        case #shader mtl of
             Phong => phong (mtl, hit)
    end
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
