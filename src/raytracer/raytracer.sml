functor Raytracer (structure S: SCENE
                   structure C: CAMERA): RAYTRACER =
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

    fun trace (scene, lights) p_ray: pixel = 
    let
      fun hit (point, normal) =
      let
        fun illume light =
        let
          val lightRay = ray (point, #point light)
        in
          case S.intersect lightRay scene of
               NONE => SOME (#direction lightRay, light)
             | SOME {point=hitPoint, ...} =>
                 (*
                 if nearer (#direction lightRay, #point light, hitPoint) then
                   SOME (#direction lightRay, light)
                 else
                   *)
                   NONE
        end

        val light = (Real.fromInt(10), Real.fromInt(0), Real.fromInt(10))
      in
        {
          ambient = Real.one,
          toCamera = neg (#direction p_ray),
          toLights = foldl
            (fn (light, acc) => case illume light of
                                     NONE => acc
                                   | SOME x => x::acc)
            []
            lights,
          normal = normal
        }
      end
    in
      case S.intersect p_ray scene of 
           NONE => {
             z = Real.posInf, 
             angle = Real.zero,
             color = {r=Real.zero, g=Real.zero, b=Real.zero}
             }
         | SOME ({point, normal, material, ...}: S.collision) => {
             z = length (#origin p_ray --> point),
             angle = normal dot (#direction p_ray),
             color = Shader.shade (material, hit (point, normal))
             }
    end

    fun renderPixel (scene, lights) camera =
    let
      val projector = Camera.projector camera;
      val tracer = trace (scene, lights)
    in
      tracer o projector
    end
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
