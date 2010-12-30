signature RAYTRACER =
sig
  type pixel

  val trace: (Scene.object * Scene.object * Shader.light list) 
          -> Geometry.ray 
          -> pixel

  val renderPixel: (Scene.object * Shader.light list) 
                 -> Camera.camera 
                 -> Camera.coords 
                 -> pixel

  val renderRegion: (Scene.object * Shader.light list) 
                  -> Camera.camera 
                  -> pixel Image.region
                  -> unit
end

(* vim: set ft=sml tw=76 nowrap et: *)
