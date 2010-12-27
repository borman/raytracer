functor Raytracer (structure S: SCENE
                   structure C: CAMERA
                   sharing C.Geometry.Real 
                         = S.Geometry.Real 
                         = S.Shader.Real 
                         = S.Shader.Rgb.Real): RAYTRACER =
struct
  structure Geometry = C.Geometry
  structure Scene = S
  structure Shader = S.Shader
  structure Camera = C

  local open Geometry in
    type pixel = {
      z: scalar, 
      angle: scalar,
      color: Shader.Rgb.color
      }

    fun trace scene ray: pixel = 
    case S.intersect ray scene of 
         NONE => {
           z = Real.posInf, 
           angle = Real.zero,
           color = {r=Real.zero, g=Real.zero, b=Real.zero}
           }
       | SOME ({point, normal, material, ...}: S.collision) => {
           z = length (#origin ray --> point),
           angle = normal dot (#direction ray),
           color = #ambientColor material
           }

    fun renderPixel scene camera =
    let
      val projector = Camera.projector camera;
      val tracer = trace scene
    in
      tracer o projector
    end
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
