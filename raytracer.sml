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

signature SCENE =
sig
  structure Geometry: GEOMETRY

  type sphere = {
    center: Geometry.vector, 
    radius: Geometry.scalar
    }
  type plane = {
    pivot: Geometry.vector, 
    normal: Geometry.vector
    }
  datatype reflectiveness 
    = Smooth
    | Glossy of Geometry.scalar
  datatype refractiveness
    = Opaque
    | Transparent of {
        transparency: Geometry.scalar,
        refraction: Geometry.scalar
        }
  type color = {
    r: Geometry.scalar, 
    g: Geometry.scalar,
    b: Geometry.scalar
    }
  type material = {
    ambientColor: color,
    diffuseColor: color,
    ambient: Geometry.scalar,
    diffuse: Geometry.scalar,
    specular: Geometry.scalar,
    shininess: Geometry.scalar,
    reflect: reflectiveness,
    refract: refractiveness
    }
  datatype object 
    = Sphere of sphere
    | Plane of plane
    | Group of (object list)
    | Material of material * object

  type collision = {
    point: Geometry.vector, 
    normal: Geometry.vector
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

functor SimpleScene(G: GEOMETRY): SCENE =
struct
  structure Geometry = G

  local open G; open Real in
    type sphere = {
      center: vector, 
      radius: scalar
      }
    type plane = {
      pivot: vector, 
      normal: vector
      }
    type collision = {
      point: vector, 
      normal: vector
      }
    datatype reflectiveness 
      = Smooth
      | Glossy of scalar
    datatype refractiveness
      = Opaque
      | Transparent of {
          transparency: scalar,
          refraction: scalar
          }
    type color = {
      r: scalar, 
      g: scalar,
      b: scalar
      }
    type material = {
      ambientColor: color,
      diffuseColor: color,
      ambient: scalar,
      diffuse: scalar,
      specular: scalar,
      shininess: scalar,
      reflect: reflectiveness,
      refract: refractiveness
      }
    datatype object 
      = Sphere of sphere
      | Plane of plane
      | Group of (object list)
      | Material of material * object

    fun intersect ray scene = 
    let 
      val {origin, direction} = ray;
      val baseMtl: material = {
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
      fun ray_sphere {center, radius} = 
      let
        val v = direction; (* along the ray; normalized *)
        val p = (center dot v) *-> v; (* closest to the center *)
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
          in
            nearest (
              visible {
                point = p1,
                normal = normal p1
              },
              visible {
                point = p2,
                normal = normal p2
              }
            )
          end
      end

      (* Infinite plane *)
      fun ray_plane {pivot, normal} =
      if direction -| normal then
        NONE
      else
        let 
          val k = ((origin --> pivot) dot normal) / (direction dot normal);
        in
          visible {
            point = origin +-> (k*->direction),
            normal = normal
          }
        end

      (* Object group *)
      fun ray_subscene subscenes =
      (foldl
        (fn (curr, acc) => nearest(intersect ray curr, acc))
        NONE
        subscenes
        )
    in
      (case scene of
            Sphere s => ray_sphere s
          | Plane p => ray_plane p
          | Group g => ray_subscene g
      )
    end
  end
end

functor Raytracer (structure S: SCENE
                   structure C: CAMERA
                   sharing C.Geometry.Real = S.Geometry.Real): RAYTRACER =
struct
  structure Geometry = C.Geometry
  structure Scene = S
  structure Camera = C

  local open Geometry in
    type pixel = {z: scalar, angle: scalar}

    fun trace scene ray: pixel = 
    case S.intersect ray scene of 
         NONE => {
           z = Real.posInf, 
           angle = Real.zero
           }
       | SOME {point, normal} => {
           z = length (#origin ray --> point),
           angle = normal dot (#direction ray) 
           } 
    
  end
end

structure Image =
struct
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
end

(*** Runner ***)

structure Geometry3D = Geometry(ExtReal)
structure FlatCamera3D = FlatCamera(Geometry3D)
structure SimpleScene3D = SimpleScene(Geometry3D)
structure SimpleRaytracer = Raytracer(
  structure G = Geometry3D
  structure S = SimpleScene3D
  structure C = FlatCamera3D)

type render_worker = FlatCamera3D.coords -> SimpleRaytracer.pixel
type image = SimpleRaytracer.pixel Image.image

fun renderPixel p_scene p_camera =
let
  val projector = FlatCamera3D.projector p_camera;
  val tracer = SimpleRaytracer.trace p_scene
in
  fn coords => tracer (projector coords)
end

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
    fun upd (acc, v) = acc + v*0.25
  in
    foldl 
      (fn ({z, angle}, {z=zacc, angle=aacc}) => 
        {z=upd (zacc, z), angle=upd (aacc, angle)}) 
      {z=0.0, angle=0.0} 
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




(** Data **)

local open Geometry3D; open SimpleScene3D; open FlatCamera3D in
  val scene_data = Group [
    Group (
      List.tabulate (
        100, 
        (fn n => 
          let
            val z = 1.0 + 0.1 * real(n);
            val phi = 1.0 * Math.pi * real(n)/10.0;
            val r =  real(n)/30.0
          in
            Sphere {
              center = (z, Math.cos phi, Math.sin phi),
              radius = r
              }
          end)
        )
    ),
    Plane {
      pivot = (0.0, 0.0, 0.0),
      normal = (0.0, 0.0, 1.0)
      }
    ]
  val cam = Camera (
    ray ( 
      (0.0, 0.0, 0.5), 
      (1.0, 0.2, 0.5)
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
  val renderer = renderPixel scene_data cam;
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
in
  0
end;

main ("raytracer", []);
