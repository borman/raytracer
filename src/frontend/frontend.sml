(*** Runner ***)

structure Image = Image (Rgb)
structure Shader = Shader (
  structure C = Rgb
  structure G = Geometry)
structure FlatCamera = FlatCamera (Geometry)
structure Scene = Scene (
  structure G = Geometry
  structure S = Shader)
structure Raytracer = Raytracer (
  structure S = Scene
  structure C = FlatCamera)

type render_worker = FlatCamera.coords -> Raytracer.pixel
type image = Raytracer.pixel Image.image


fun antialias (renderWorker: render_worker) =
  fn (x, y) =>
  let
    val ds = 
     [(0.0, 0.0),
      (0.5, 0.0),
      (0.0, 0.5),
      (0.5, 0.5)]
    fun shift (x, y) (dx, dy) = (x+dx, y+dy)
    val subpixels = map 
      (renderWorker o shift (x, y))
      ds
    fun upd_real (v, acc) = acc + v*0.25
    fun upd_rgb (v, acc) = Rgb.add(acc, Rgb.mul(0.25, v)) 
    fun upd_pix (v: Raytracer.pixel, acc: Raytracer.pixel) = {
      z = upd_real (#z v, #z acc),
      angle = upd_real (#angle v, #angle acc),
      color = upd_rgb (#color v, #color acc)
      }
  in
    foldl 
      upd_pix
      {z = 0.0, angle = 0.0, color = Rgb.black} 
      subpixels
  end

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
    Image.foldl 
      (fn (pix, acc) => maxNormal (#z pix, acc))
      0.0 
      img)
  val textImg = Image.mapToGray 
    (fn pix => chr (grayscale maxDepth (#z pix)))
    img
in
  Image.saveGray (textImg, filename)
end

fun saveNormals (img: image, filename) =
let
  fun grayscale angle = 
    Real.round (Real.min (1.0, Real.abs angle) * 255.0)

  val textImg = Image.mapToGray 
    (fn pix => chr (grayscale (#angle pix)))
    img
in
  Image.saveGray (textImg, filename)
end

fun saveColors (img: image, filename) =
let
  val rgbImg = Image.map (#color) img
in
  Image.saveRGB (rgbImg, filename)
end



(** Data **)

local 
  open Geometry 
  open Scene 
  open FlatCamera 
in
  fun solidMtl color = 
   {shader = (Shader.Lambert, Shader.Blinn),
    ambientColor = color,
    ambient = 0.2,
    diffuseColor = color,
    diffuse = 1.0,
    specularColor = Rgb.white,
    specular = 1.0,
    shininess = 100.0,
    reflect = Shader.Glossy 0.7,
    refract = Shader.Opaque}

  val lights = 
   [{point = (8.0, 0.0, 10.0),
     diffuse = 0.5,
     specular = 0.5},
    {point = (0.0, 8.0, 10.0),
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
           val base = (2.0, 0.0, 0.1)
           val middle = base +-> (5.0, 5.0, 0.0)
           val pos = base +-> (p, q, 0.0)
           val r = 2.0 / (1.0 + length (middle --> pos))
           val color = 
            {r = p/10.0, 
             g = q/10.0, 
             b = 1.0 - Math.sqrt((p*p + q*q)/2.0)/10.0}
         in
           Material 
            (solidMtl color,
             Sphere 
              {center = pos +-> (0.0, 0.0, r * 2.0),
               radius = r})
         end))),
    Material 
     (solidMtl Rgb.white,
      Plane 
       {pivot = (0.0, 0.0, 0.0),
        normal = (0.0, 0.0, 1.0)})]

  val cam = Camera 
   (ray ((~1.0, ~1.0, 10.0), (5.5, 5.0, 2.2)),
    Screen ((512.0, 512.0), Math.pi/6.0))
end


(** Evaluation **)

fun main (arg0:string, argv: string list) =
let
  val do_antialias = true

  val renderer = Raytracer.renderPixel (scene, lights) cam

  (* Turn antialiasing on *)
  val renderer = 
    if do_antialias 
      then antialias renderer
    else renderer
in
  print "Rendering...\n"
  ; 
  let
    val img = Image.render renderer (512, 512)
  in
    print "Rendered, saving...\n"
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
