signature RAYTRACER =
sig
  structure Geometry: GEOMETRY
  structure Scene: SCENE
  structure Camera: CAMERA

  type pixel

  val trace: Scene.object -> Geometry.ray -> pixel
  val renderPixel: Scene.object -> Camera.camera -> Camera.coords -> pixel
end

(* vim: set ft=sml tw=76 nowrap et: *)
