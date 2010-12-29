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
                 if nearer (#direction lightRay, #point light, hitPoint) then
                   SOME (#direction lightRay, light)
                 else
                   NONE
        end

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

      val voidPixel = {
        z = Real.posInf, 
        angle = Real.zero,
        color = {r=Real.zero, g=Real.zero, b=Real.zero}
        }

      fun reflect (vec, normal) = 
      let
        val d = ((vec dot normal) *-> normal) --> vec
      in
        vec <-- (Real.two *-> d)
      end

      fun reflect_ray (normal, point, p_ray: ray) = {
        origin = point, 
        direction = reflect 
          (neg (#direction p_ray), normal)
        }

      fun do_trace (_, 0) = voidPixel
        | do_trace (p_ray, n) =
        case S.intersect p_ray scene of 
             NONE => voidPixel
           | SOME ({point, normal, material, ...}: S.collision) => {
               z = length (#origin p_ray --> point),
               angle = normal dot (#direction p_ray),
               color = 
                 let
                   val shaded = Shader.shade (material, hit (point, normal))
                 in
                   case #reflect material of
                        Shader.Dull => shaded
                      | Shader.Glossy transp => 
                          Rgb.add (
                            (* Rgb.mul (1.0-transp, shaded), *)
                            shaded,
                            Rgb.mul (transp, 
                              #color (do_trace 
                                (reflect_ray (normal, point, p_ray), n-1)
                                )
                              )
                            )
                 end
               }
    in
      do_trace (p_ray, 10)
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
