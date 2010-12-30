(*** Runner ***)

(* Shorthands *)
type image = Raytracer.pixel Image.image

val vec = Vector3D.vector


fun saveZ (img: image, filename) =
let
  fun grayscale maxDepth z = 
    255 - 
     (if z<maxDepth 
        then Real.round (z * 255.0 / maxDepth) 
      else 255)

  fun maxNormal (a, b) =
    case (Real.isNormal a, Real.isNormal b) of
         (true, true) => Real.max (a, b)
       | (true, false) => a
       | (false, true) => b
       | (false, false) => 0.0

  val maxDepth = Real.min 
   (10.0, 
    Image.fold 
      (fn (pix, acc) => maxNormal (#z pix, acc))
      0.0 
      img)
  val textImg = Image.toGray 
    (chr o grayscale maxDepth o #z)
    img
in
  PNMWriter.save (filename, Image.size img, PNMWriter.Grayscale textImg)
end

fun saveNormals (img: image, filename) =
let
  fun grayscale angle = 
    Real.round (Real.abs angle * 255.0)

  val textImg = Image.toGray 
    (chr o grayscale o #angle)
    img
in
  PNMWriter.save (filename, Image.size img, PNMWriter.Grayscale textImg)
end

fun saveColors (img: image, filename) =
let
  fun grayscale x = Real.round (x * 255.0)
  fun rgb_to_string {r, g, b} = 
    String.implode (map (chr o grayscale) [r, g, b])
  val rgbImg = Image.toColor (rgb_to_string o #color) img
in
  PNMWriter.save (filename, Image.size img, PNMWriter.Color rgbImg)
end



(** Data **)

fun createScene () = 
let
  open Geometry 
  open Scene 
  open Camera 

  fun solidMtl color = 
   {shader = (Shader.Lambert, Shader.Blinn),
    ambientColor = color,
    ambient = 0.2,
    diffuseColor = color,
    diffuse = 1.0,
    specularColor = Rgb.white,
    specular = 1.0,
    shininess = 1000.0,
    reflect = Shader.Glossy 0.6,
    refract = Shader.Opaque}

  val lights = 
   [{point = vec (8.0, 0.0, 10.0),
     diffuse = 0.5,
     specular = 0.5},
    {point = vec (0.0, 8.0, 10.0),
     diffuse = 0.5,
     specular = 0.5}]

  val scene = Group 
   [Group 
    (List.tabulate 
     (100, 
       (fn n => 
         let
           val p = real (n div 10)
           val q = real (n mod 10)
           val base = vec (2.0, 0.0, 0.1)
           val middle = base +-> vec (5.0, 5.0, 0.0)
           val pos = base +-> vec (p, q, 0.0)
           val r = 2.0 / (1.0 + length (middle --> pos))
           val color = 
            {r = p/10.0, 
             g = q/10.0, 
             b = 1.0 - Math.sqrt((p*p + q*q)/2.0)/10.0}
         in
           Material 
            (solidMtl color,
             Sphere 
              {center = pos +-> vec (0.0, 0.0, r * 2.0),
               radius = r})
         end))),
    Material 
     (solidMtl (Rgb.rgb (0.8, 0.8, 0.8)),
      Plane 
       {pivot = vec (0.0, 0.0, 0.0),
        normal = vec (0.0, 0.0, 1.0)})]

  val cam =
   {location = vec (~1.0, ~1.0, 10.0),
    look_at = vec (5.5, 5.0, 2.2),
    aspect = 1.0,
    angle = (Math.pi / 3.0) / Math.sqrt 2.0,
    projection = Rectilinear}
in
  (lights, scene, cam)
end


(** Evaluation **)

fun main (arg0:string, argv: string list) =
let
  val do_antialias = true

  val image_size = (512, 512)

  val (lights, scene, cam) = createScene ()
  val renderer = Raytracer.renderPixel (scene, lights) cam

  (* Turn antialiasing on *)
  val sampler = 
    if do_antialias 
      then Raytracer.super4x
    else Raytracer.noSample
in
  print "Rendering...\n"
  ; 
  let
    (*
    val img = Image.render 
      (Raytracer.renderPixel (scene, lights) cam sampler (512, 512))
      (512, 512)
      *)
    val img = Image.array (512, 512, {z=0.0, angle=0.0, color=Rgb.black})
  in
    Image.modifyRegions 
      (Raytracer.renderRegion (scene, lights) cam sampler) 
      (64, 64)
      img
    ; print "Rendered, saving...\n"
    ; saveZ (img, "ray_z.pgm")
    ; saveNormals (img, "ray_normals.pgm")
    ; saveColors (img, "ray_colors.ppm")
    ; OS.Process.success
  end
end;

(*
SMLofNJ.exportFn("raytracer", main);
*)

main ("raytracer", []);


(* vim: set ft=sml tw=76 nowrap et: *)
