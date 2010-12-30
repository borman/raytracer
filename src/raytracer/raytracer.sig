signature RAYTRACER =
sig
  type pixel
  type sampler = (Camera.coords -> pixel) 
               -> int * int 
               -> Image.coords
               -> pixel

  val trace: (Scene.object * Scene.object * Shader.light list) 
           -> Geometry.ray 
           -> pixel

  val renderPixel: (Scene.object * Shader.light list) 
                 -> Camera.camera 
                 -> sampler
                 -> Image.size
                 -> Image.coords 
                 -> pixel

  val renderRegion: (Scene.object * Shader.light list) 
                  -> Camera.camera 
                  -> sampler
                  -> pixel Image.region
                  -> unit

  val noSample: sampler
  val super4x: sampler
end

(* vim: set ft=sml tw=76 nowrap et: *)
