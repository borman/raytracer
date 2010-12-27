(*** Geometry ***)

(* Infix vector operators *)  
infix -->
infix <--
infix +->
infix *->
infix ||
infix -|
infix dot
infix cross


(* Signatures *)
signature EXTREAL =
sig
  include REAL

  val zero: real
  val one: real
  val epsilon: real
  
  val isZero: real -> bool
  val inv: real -> real
end

signature GEOMETRY =
sig
  structure Real: EXTREAL

  (* Types *)
  type scalar = Real.real
  type vector = scalar * scalar * scalar
  type ray = {origin: vector, direction: vector}

  (* Constructors *)
  val ray: vector * vector -> ray

  (* Axes *)
  val null: vector
  val x: vector
  val y: vector
  val z: vector

  val isNull: vector -> bool

  (* Basic vector ops *)
  val add: vector * vector -> vector
  val neg: vector -> vector
  val mul: scalar * vector -> vector
  val dot: vector * vector -> scalar
  val cross: vector * vector -> vector
  
  (* Infix synonyms *)
  val --> : vector * vector -> vector
  val <-- : vector * vector -> vector
  val +-> : vector * vector -> vector
  val *-> : scalar * vector -> vector

  (* Compound vector ops *)
  val sub: vector * vector -> vector
  val sqlength: vector -> scalar
  val length: vector -> scalar
  val norm: vector -> vector

  (* Parallellity/perpendicularity tests *)
  val ||  : vector * vector -> bool
  val -|  : vector * vector -> bool
end


(* Functors *)
structure ExtReal : EXTREAL =
struct
  open Real

  val zero = 0.0
  val one = 1.0
  val epsilon = 1E~12

  fun isZero a = abs(a) < epsilon
  fun inv a = one / a
end

functor Geometry(R: EXTREAL) : GEOMETRY =
struct 
  structure Real = R

  local open R in
    type scalar = R.real
    type vector = scalar * scalar * scalar
    type ray = {origin: vector, direction: vector}
    
    val null = (zero, zero, zero)
    val x = (one, zero, zero)
    val y = (zero, one, zero)
    val z = (zero, zero, one)

    fun isNull (x, y, z) = isZero(x) andalso isZero(y) andalso isZero(z)

    (* Basic vector ops *)
    fun add ((x1, y1, z1), (x2, y2, z2)): vector = 
      (x1 + x2, y1 + y2, z1 + z2)

    fun neg (x, y, z): vector = 
      (~x, ~y, ~z)

    fun mul (k, (x, y, z)): vector =
      (k*x, k*y, k*z)
      
    fun ((x1, y1, z1) dot (x2, y2, z2)): scalar =
      x1*x2 + y1*y2 + z1*z2

    fun ((x1, y1, z1) cross (x2, y2, z2)): vector =
      (y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2)

    (* Compound vector ops *)
    fun sub (v1, v2) = add (v1, neg v2)
    fun sqlength v = v dot v
    fun length v = Math.sqrt (sqlength v)
    fun norm v = mul(inv (length v), v)

    (* Infix synonyms *)
    fun (a --> b) = sub (b, a)
    fun (a <-- b) = sub (a, b)
    fun (a +-> b) = add (a, b)
    fun (a *-> b) = mul (a, b)

    (* Parallellity/perpendicularity tests *)
    fun (a || b) = isNull(a cross b)
    fun (a -| b) = isZero(a dot b)

    fun ray (origin, target) = {
      origin = origin, 
      direction = norm (origin --> target)
    }
  end
end


(*** Raytracer ***)

signature CAMERA = 
sig
  structure Geometry: GEOMETRY

  type coords = Geometry.scalar * Geometry.scalar
  type angle = Geometry.scalar

  datatype screen = Screen of coords * angle
  datatype camera = Camera of Geometry.ray * screen

  val projector: camera -> coords -> Geometry.ray
end

signature RGB =
sig
  structure Real: EXTREAL;
  type color = {
    r: Real.real,
    g: Real.real,
    b: Real.real
    }

  val map: (Real.real -> Real.real) -> color -> color
  val zip: (Real.real * Real.real -> Real.real) -> color * color -> color
  val add: color * color -> color
  val mul: Real.real * color -> color
end

signature SHADER =
sig
  structure Real: EXTREAL
  structure Rgb: RGB
  structure Geometry: GEOMETRY

  datatype reflectiveness 
    = Smooth
    | Glossy of Real.real
  datatype refractiveness
    = Opaque
    | Transparent of {
        transparency: Real.real,
        refraction: Real.real
        }
  datatype method = Phong
  type material = {
    shader: method,
    ambientColor: Rgb.color,
    diffuseColor: Rgb.color,
    ambient: Real.real,
    diffuse: Real.real,
    specular: Real.real,
    shininess: Real.real,
    reflect: reflectiveness,
    refract: refractiveness
    }
  type hit = {
    toCamera: Geometry.vector,
    toLight: Geometry.vector,
    normal: Geometry.vector
    }

  val shade: material * hit -> Rgb.color
end

signature SCENE =
sig
  structure Shader: SHADER
  structure Geometry: GEOMETRY

  type sphere = {
    center: Geometry.vector, 
    radius: Geometry.scalar
    }
  type plane = {
    pivot: Geometry.vector, 
    normal: Geometry.vector
    }
  datatype object 
    = Sphere of sphere
    | Plane of plane
    | Group of (object list)
    | Material of Shader.material * object

  type collision = {
    point: Geometry.vector, 
    normal: Geometry.vector,
    object: object,
    material: Shader.material
    }

  val intersect: Geometry.ray -> object -> collision option
end

signature RAYTRACER =
sig
  structure Geometry: GEOMETRY
  structure Scene: SCENE
  structure Camera: CAMERA

  type pixel

  val trace: Scene.object -> Geometry.ray -> pixel
  val renderPixel: Scene.object -> Camera.camera -> Camera.coords -> pixel
end


functor Rgb(R: EXTREAL): RGB =
struct
  structure Real = R

  local open R in
    type color = {
      r: real,
      g: real,
      b: real
      }

    fun map f {r, g, b} = 
      {r=f r, g=f g, b=f b}
    fun zip f ({r=r1, g=g1, b=b1}, {r=r2, g=g2, b=b2}) =
      {r=f (r1, r2), g=f (g1, g2), b=f (b1, b2)}

    fun saturate r = max (zero, min (one, r))

    fun add (col1, col2) = 
      map saturate (zip (op +) (col1, col2))
    fun mul (k, col) = 
      map (saturate o (fn x => k*x)) col
  end
end

functor FlatCamera(G: GEOMETRY): CAMERA =
struct
  structure Geometry = G

  local open G; open Real in
    type coords = scalar * scalar
    type angle = scalar
    datatype screen = Screen of coords * angle;
    datatype camera = Camera of ray * screen

    fun projector cam =
    let
      val Camera (
        {origin, direction}, 
        Screen ((width, height), angle)) = cam;
      
      val camv = direction;
      val horizontal = norm (camv cross z); (* FIXME: camv || z is bad *)
      val vertical = neg (norm (horizontal cross camv));
      
      val halfdiag = length camv * Math.tan angle;
      val phi = Math.atan2 (height, width);

      val halfwidth = halfdiag * Math.cos phi;
      val halfheight = halfdiag * Math.sin phi;

      val screenOrigin = origin +-> direction 
          +-> neg (halfwidth *-> horizontal) 
          +-> neg (halfheight *-> vertical);

      val screenX = ((halfwidth+halfwidth)/width) *-> horizontal;
      val screenY = ((halfheight+halfheight)/height) *-> vertical 
    in
      fn (x, y) => ray (
        origin,
        screenOrigin +-> (x *-> screenX) +-> (y *-> screenY)
        )
    end
  end 
end

functor Shader (structure R: EXTREAL
                structure C: RGB
                structure G: GEOMETRY
                sharing G.Real = C.Real = R): SHADER =
struct
  structure Real = R
  structure Rgb = C
  structure Geometry = G

  local open G; open C; open R in
    datatype reflectiveness 
      = Smooth
      | Glossy of real
    datatype refractiveness
      = Opaque
      | Transparent of {
          transparency: real,
          refraction: real
          }
    datatype method = Phong
    type material = {
      shader: method,
      ambientColor: color,
      diffuseColor: color,
      ambient: real,
      diffuse: real,
      specular: real,
      shininess: real,
      reflect: reflectiveness,
      refract: refractiveness
      }
    type hit = {
      toCamera: vector,
      toLight: vector,
      normal: vector
      }

    local
      fun phong (mtl: material, hit: hit) = #ambientColor mtl
    in
      fun shade (mtl: material, hit) = 
        case #shader mtl of
             Phong => phong (mtl, hit)
    end
  end
end

functor Scene(structure G: GEOMETRY
              structure S: SHADER
              sharing S.Real = S.Rgb.Real = G.Real): SCENE =
struct
  structure Geometry = G
  structure Shader = S

  local open G; open S; open Real in
    type sphere = {
      center: vector, 
      radius: scalar
      }
    type plane = {
      pivot: vector, 
      normal: vector
      }
    datatype object 
      = Sphere of sphere
      | Plane of plane
      | Group of (object list)
      | Material of material * object

    type collision = {
      point: vector, 
      normal: vector,
      object: object,
      material: S.material
      }

    fun intersect ray scene = 
    let 
      val {origin, direction} = ray;
      val baseMtl: material = {
        shader = Phong,
        ambientColor = {r = one, g = one, b = one},
        ambient = zero,
        diffuseColor = {r = one, g = one, b = one},
        diffuse = one,
        specular = zero,
        shininess = zero,
        reflect = Smooth,
        refract = Opaque
        };

      fun visible collision: collision option =
        if ((origin --> #point collision) dot direction) > zero then
          SOME collision
        else
          NONE

      fun nearest (NONE, opt): collision option = opt
        | nearest (opt, NONE) = opt
        | nearest (SOME x, SOME y) = SOME (
          if (direction dot (#point x --> #point y)) > zero then
            x 
          else 
            y
          )


      (* Sphere *)
      fun hit (mtl, sphere as Sphere {center, radius}) = 
        let
          val v = direction; (* along the ray; normalized *)
          val p = origin +-> (((origin -->center) dot v) *-> v); (* closest to the center *)
          val d_sq = sqlength (center --> p) (* distance to the center *)
        in
          if d_sq > radius*radius then
            NONE
          else
            let
              fun cat (r, c_sq) = Math.sqrt (r*r - c_sq); (* find a cathetus *)
              fun shifted len = p +-> (len *-> v)

              val off = cat (radius, d_sq); (* distance to intersections *)
              val p1 = shifted (~off)
              and p2 = shifted (off)

              fun normal p = norm (center --> p)
              fun coll p = visible {
                point = p,
                normal = normal p,
                object = sphere,
                material = mtl
                }
            in
              nearest (coll p1, coll p2)
            end
        end

      (* Infinite plane *)
        | hit (mtl, plane as Plane {pivot, normal}) =
        if direction -| normal then
          NONE
        else
          let 
            val k = ((origin --> pivot) dot normal) / (direction dot normal);
          in
            visible {
              point = origin +-> (k*->direction),
              normal = normal,
              object = plane,
              material = mtl
            }
          end

      (* Object group *)
        | hit (mtl, Group subscenes) = 
        foldl
          (fn (curr, acc) => nearest(hit (mtl, curr), acc))
          NONE
          subscenes

      (* Material application *)
        | hit (_, Material (mtl, subscene)) = hit (mtl, subscene)
    in
      hit (baseMtl, scene)
    end
  end
end

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

functor Image (structure R: EXTREAL
               structure C: RGB
               sharing C.Real = R) =
struct
  structure Real = R
  structure Rgb = C

  type 'a image = {
    width: int, 
    height: int,
    pixels: 'a vector
    }
  type gray_image = {
    width: int, 
    height: int,
    pixels: CharVector.vector
    }

  fun render renderer (w, h): 'a image = {
    width = w,
    height = h,
    pixels = Vector.tabulate (
      w*h, 
      (fn n =>
        let 
          val (x, y) = (n mod w, n div w)
        in
          renderer (Real.fromInt x, Real.fromInt y)
        end)
      )
    }

  fun mapToGray f ({width, height, pixels}: 'a image) = {
    width = width,
    height = height,
    pixels = CharVector.tabulate (
      (width * height),
      fn n => f (Vector.sub (pixels, n))
      )
    }

  fun map f ({width, height, pixels}: 'a image) = {
    width = width,
    height = height,
    pixels = Vector.tabulate (
      width * height,
      fn n => f (Vector.sub (pixels, n))
      )
    }

  fun foldl f base (img: 'a image) = Vector.foldl f base (#pixels img)

  fun saveGray (img: gray_image, filename) =
  let
    val file = TextIO.openOut filename
  in
    TextIO.output (file, String.concat [
      "P5\n",
      List.foldr 
        (fn (n, acc) => (Int.toString n) ^ " " ^ acc) 
        "\n" 
        [#width img, #height img, 255],
      #pixels img 
      ]);  
    TextIO.closeOut file
  end

  fun saveRGB (img: Rgb.color image, filename) =
  let
    open Real
    val file = TextIO.openOut filename
    fun rgb_to_str {r, g, b} = String.implode (
      List.map
        (fn x => chr (round (fromInt(255) * x)))
        [r, g, b]
      )
  in
    TextIO.output (file, String.concat (
      [
        "P6\n",
        List.foldr 
          (fn (n, acc) => (Int.toString n) ^ " " ^ acc) 
          "\n" 
          [#width img, #height img, 255] 
      ] @ 
      Vector.foldr 
        (fn (col, acc) => (rgb_to_str col)::acc)
        []
        (#pixels img)
      ));  
    TextIO.closeOut file
  end
end

(*** Runner ***)

structure Geometry = Geometry(ExtReal)
structure Rgb = Rgb(ExtReal)
structure Image = Image(
  structure R = ExtReal
  structure C = Rgb)
structure Shader = Shader(
  structure R = ExtReal
  structure C = Rgb
  structure G = Geometry)
structure FlatCamera = FlatCamera(Geometry)
structure Scene = Scene(
  structure G = Geometry
  structure S = Shader)
structure Raytracer = Raytracer(
  structure S = Scene
  structure C = FlatCamera)

type render_worker = FlatCamera.coords -> Raytracer.pixel
type image = Raytracer.pixel Image.image


fun antialias (renderWorker: render_worker) =
  fn (x, y) =>
  let
    val ds = [
      (0.0, 0.0),
      (0.5, 0.0),
      (0.0, 0.5),
      (0.5, 0.5)];
    fun shift (x, y) (dx, dy) = (x+dx, y+dy);
    val subpixels = map 
      renderWorker 
      (map 
        (shift (x, y)) 
        ds
      );
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
      {z=0.0, angle=0.0, color={r=0.0, g=0.0, b=0.0}} 
      subpixels
  end

fun saveZ (img: image, filename) =
let
  fun grayscale maxDepth z = 
    255 - (
      if z<maxDepth then 
        Real.round (z*255.0/maxDepth) 
      else
        255
      );

  fun maxNormal (a, b) =
    case (Real.isNormal(a), Real.isNormal(b)) of
         (true, true) => Real.max(a, b)
       | (true, false) => a
       | (false, true) => b
       | (false, false) => 0.0

  val maxDepth = Real.min (
    10.0, 
    Image.foldl 
      (fn (pix, acc) => maxNormal (#z pix, acc))
      0.0 
      img
    );
  val textImg = Image.mapToGray 
    (fn pix => chr (grayscale maxDepth (#z pix)))
    img
in
  Image.saveGray (textImg, filename)
end

fun saveNormals (img: image, filename) =
let
  fun grayscale angle = 
    Real.round (Real.min(1.0, Real.abs(angle)) * 255.0)

  val textImg = Image.mapToGray 
    (fn pix => chr (grayscale (#angle pix)))
    img
in
  Image.saveGray (textImg, filename)
end

fun saveColors (img: image, filename) =
let
  val rgbImg = Image.map 
    (#color)
    img
in
  Image.saveRGB (rgbImg, filename)
end



(** Data **)

local open Geometry; open Scene; open FlatCamera in
  fun solidMtl color = {
    shader = Shader.Phong,
    ambientColor = color,
    ambient = 0.2,
    diffuseColor = color,
    diffuse = 1.0,
    specular = 0.0,
    shininess = 0.0,
    reflect = Shader.Smooth,
    refract = Shader.Opaque
    }
  val scene_data = Group [
    Group (
      List.tabulate (
        100, 
        (fn n => 
          let
            val p = real (n div 10);
            val q = real (n mod 10);
            val base = (2.0, 0.0, 0.7);
            val middle = base +-> (5.0, 5.0, 0.0);
            val pos = base +-> (p, q, 0.0);
            val r = 2.0 / (1.0 + length (middle --> pos));
            val color = {r=p/10.0, g=q/10.0, b=1.0};
          in
            Material (
              solidMtl color,
              Sphere {
                center = pos,
                radius = r
                }
              )
          end)
        )
    ),
    Material (
      solidMtl {r=0.5, g=0.5, b=0.5},
      Plane {
        pivot = (0.0, 0.0, 0.0),
        normal = (0.0, 0.0, 1.0)
        }
      )
    ]
  val cam = Camera (
    ray ( 
      (0.0, 0.0, 3.0), 
      (2.5, 2.0, 1.0)
      ),
    Screen (
      (512.0, 512.0), 
      Math.pi/4.0
      )
    )
end;


(** Evaluation **)

fun main (arg0:string, argv: string list) =
let
  val do_antialias = true;
  val renderer = Raytracer.renderPixel scene_data cam;
  (* Turn antialiasing on *)
  val renderer = if do_antialias then
                   antialias renderer
                 else
                   renderer;
  val () = print "Rendering...\n";
  val img = Image.render renderer (512, 512);
  val () = print "Rendered, saving...\n";
  val () = saveZ (img, "ray_z.pgm");
  val () = saveNormals (img, "ray_normals.pgm")
  val () = saveColors (img, "ray_colors.ppm")
in
  0
end;

main ("raytracer", []);
