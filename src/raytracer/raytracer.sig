signature RAYTRACER =
sig
  structure Geometry: GEOMETRY
  structure Scene: SCENE
  structure Camera: CAMERA
  structure Shader: SHADER

  type pixel

  val trace: (Scene.object * Shader.light list) -> Geometry.ray -> pixel
  val renderPixel: (Scene.object * Shader.light list) -> Camera.camera -> Camera.coords -> pixel
end

(* vim: set ft=sml tw=76 nowrap et: *)
