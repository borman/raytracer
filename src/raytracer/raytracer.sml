structure Raytracer: RAYTRACER =
struct
  local 
    open Geometry 
  in
    type pixel = 
     {z: real, 
      angle: real,
      color: Rgb.color}

    fun trace (fullScene, cutScene, lights) p_ray: pixel = 
    let
      fun hit (point, normal) =
      let
        fun illume light =
        let
          val lightRay = ray (point, #point light)
        in
          case Scene.intersect lightRay fullScene of
               NONE => SOME (#direction lightRay, light)
             | SOME {point=hitPoint, ...} =>
                 if nearer (#direction lightRay, #point light, hitPoint) then
                   SOME (#direction lightRay, light)
                 else
                   NONE
        end
      in
       {ambient = Real.one,
        toCamera = neg (#direction p_ray),
        toLights = foldl
          (fn (light, acc) => case illume light of
                                   NONE => acc
                                 | SOME x => x::acc)
          []
          lights,
        normal = normal}
      end

      val voidPixel = 
       {z = Real.posInf, 
        angle = Real.zero,
        color = Rgb.black}

      fun reflect_ray (normal, point, p_ray: ray) = 
       {origin = point, 
        direction = reflect 
          (neg (#direction p_ray), normal)}

      fun do_trace (_, _, 0) = voidPixel
        | do_trace (thisScene, p_ray, n) =
        case Scene.intersect p_ray thisScene of 
             NONE => voidPixel
           | SOME ({point, normal, material, ...}: Scene.collision) => 
              {z = length (#origin p_ray --> point),
               angle = normal dot #direction p_ray,
               color = 
                 let
                   val shaded = Shader.shade (material, hit (point, normal))
                 in
                   case #reflect material of
                        Shader.Dull => shaded
                      | Shader.Glossy transp => 
                          Rgb.add 
                           ((* Rgb.mul (1.0-transp, shaded), *)
                            shaded,
                            Rgb.mul (transp, 
                              reflection (normal, point, p_ray, n)))
                 end}
      and reflection (normal, point, p_ray, n) = 
        #color (do_trace 
         (fullScene, 
          reflect_ray (normal, point, p_ray), 
          n-1))
    in
      do_trace (cutScene, p_ray, 10)
    end

    fun renderPixel (scene, lights) camera =
    let
      val projector = Camera.projector camera
      val tracer = trace (scene, scene, lights)
    in
      tracer o projector
    end

    fun renderRegion (fullScene, lights) camera region =
    let
      val projector = Camera.projector camera
      val (w, h) = Image.size (#base region)
      val (rw, rh) = (real w, real h)
      fun cam (x, y) = (real x / rw, real y / rh)

      fun cone (x1, y1, x2, y2) =
      let
        val dir = #direction o projector o cam
        val (tl, tr, bl, br) = 
         (dir (x1, y1), dir (x2, y1), dir (x1, y2), dir (x2, y2))
      in
       {source = #location camera,
        topleft = tl,
        topright = tr,
        bottomleft = bl,
        bottomright = br}
      end

      val coords =
       (#col region,
        #row region,
        #col region + valOf (#ncols region),
        #row region + valOf (#nrows region))
      val cutScene = Option.getOpt 
       (Scene.cut (cone coords) fullScene, 
        Scene.Group [])
      val renderer = trace (fullScene, cutScene, lights) o projector o cam
    in
      Image.modifyi 
        Image.RowMajor
        (fn (y, x, _) => renderer (x, y))
        region
    end
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
