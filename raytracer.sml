(*** Geometry ***)

(* Infix vector operators *)  
infix -->
infix <--
infix +->
infix *->
infix *|
infix **


(* Signatures *)
signature SCALARSPACE =
sig
  include REAL

  type scalar = real

  val zero: scalar
  val one: scalar

  val inv : scalar -> scalar
end

signature VECTORSPACE3D =
sig
  include SCALARSPACE

  type vector = scalar * scalar * scalar

  val add: vector * vector -> vector
  val mul: scalar * vector -> vector
  val dot: vector * vector -> scalar
  val cross: vector * vector -> vector
  
  (* Sugar *)
  val neg: vector -> vector
  val sub: vector * vector -> vector
  val length: vector -> scalar
  val norm: vector -> vector
end

signature GEOMETRY =
sig
  include VECTORSPACE3D

  type ray = {origin: vector, target: vector}

  val x: vector
  val y: vector
  val z: vector

  val --> : vector * vector -> vector
  val <-- : vector * vector -> vector
  val +-> : vector * vector -> vector
  val *-> : scalar * vector -> vector
  val *|  : vector * vector -> scalar
  val **  : vector * vector -> vector
end


(* Functors *)
functor RealSpace(R: REAL) : SCALARSPACE =
struct
  open R
  type scalar = real

  val zero = fromInt(0)
  val one = fromInt(1)

  fun inv a = one / a
end

functor VectorSpace3D(F: SCALARSPACE) : VECTORSPACE3D =
struct 
  open F
  type vector = scalar * scalar * scalar

  fun add ((x1, y1, z1), (x2, y2, z2)): vector = 
    (x1 + x2, y1 + y2, z1 + z2)

  fun mul (k, (x, y, z)): vector =
    (k*x, k*y, k*z)
    
  fun dot ((x1, y1, z1), (x2, y2, z2)): scalar =
    x1*x2 + y1*y2 + z1*z2

  fun cross ((x1, y1, z1), (x2, y2, z2)): vector =
    (y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2)

  (* Sugar *)
  fun neg v = mul (~one, v)
  fun sub (v1, v2) = add (v1, neg v2)
  fun length v = Math.sqrt (dot (v, v))
  fun norm v = mul(inv (length v), v)
end

functor Geometry(S: VECTORSPACE3D) : GEOMETRY =
struct
  open S;
  type ray = {origin: vector, target: vector}

  val x = (one, zero, zero)
  val y = (zero, one, zero)
  val z = (zero, zero, one)

  fun (a --> b) = sub (b, a)
  fun (a <-- b) = sub (a, b)
  fun (a +-> b) = add (a, b)
  fun (a *-> b) = mul (a, b)
  fun (a *| b) = dot (a, b)
  fun (a ** b) = cross (a, b)
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
  type sphere = {center: Geometry.vector, radius: Geometry.scalar}
  datatype scene = Sphere of sphere
                 | Group of (scene list)

  val intersect: scene -> Geometry.ray -> Geometry.vector option
end

functor FlatCamera(G: GEOMETRY): CAMERA =
struct
  structure Geometry = G
  type coords = G.scalar * G.scalar
  type angle = G.scalar
  datatype screen = Screen of coords * angle;
  datatype camera = Camera of G.ray * screen

  fun projector cam =
  let
    open G;
    val Camera (
      {origin, target}, 
      Screen ((width, height), angle)) = cam;
    
    val camv = origin --> target;
    val horizontal = norm (camv ** z);
    val vertical = norm (horizontal ** camv);
    
    val halfdiag = length camv * Math.tan angle;
    val phi = Math.atan2 (height, width);

    val halfwidth = halfdiag * Math.cos phi;
    val halfheight = halfdiag * Math.sin phi;

    val screenOrigin = target 
        +-> neg (halfwidth *-> horizontal) 
        +-> (halfheight *-> vertical);
    val screenX = ((halfwidth+halfwidth)/width) *-> horizontal;
    val screenY = neg (((halfheight+halfheight)/height) *-> vertical) 
  in
    fn (x, y) => {
      origin = origin, 
      target = screenOrigin +-> (x *-> screenX) +-> (y *-> screenY)
      }
  end
    
end

functor SimpleScene(G: GEOMETRY): SCENE =
struct
  structure Geometry = G
  type sphere = {center: Geometry.vector, radius: Geometry.scalar}
  datatype scene = Sphere of sphere
                 | Group of (scene list)

  fun intersect p_scene p_ray = 
  let 
    open G

    fun dist v = length (#origin p_ray --> v)
    fun nearest (NONE, NONE) = NONE
      | nearest (NONE, SOME x) = SOME x
      | nearest (SOME x, NONE) = SOME x
      | nearest (SOME x, SOME y) = SOME (if dist x < dist y then x else y)

    fun ray_sphere ({origin, target}, {center, radius}) = 
    let
      val v = norm (origin --> target); (* along the ray; normalized *)
      val p = (center *| v) *-> v; (* closest to the center *)
      val d = length (center --> p) (* distance to the center *)
    in
      if d > radius then
        NONE
      else
        let
          fun cat (r, c) = Math.sqrt (r*r - c*c); (* find a cathetus *)
          val off = cat (radius, d); (* distance to intersections *)
          val [p1, p2] = map (fn x => p +-> (x *-> v)) [~off, off]
        in
          nearest (SOME p1, SOME p2)
        end
    end

    fun ray_subscene (r, subscenes) =
      foldl nearest NONE (map (fn x => intersect x p_ray) subscenes)
  in
    case p_scene of
         Sphere s => ray_sphere (p_ray, s)
       | Group g => ray_subscene (p_ray, g)
  end
end

functor Raytracer (structure G: GEOMETRY 
                   structure S: SCENE
                   structure C: CAMERA
                   sharing C.Geometry = S.Geometry = G) =
struct
  local open G in
    type pixel = {z: scalar}

    fun trace scene ray = 
    let
      val dist = 
        (case S.intersect scene ray of 
              NONE => posInf
            | SOME v => length (#origin ray --> v))
    in
      {z=dist}                   
    end
  end
end


(*** Runner ***)

structure Geometry3D = Geometry(VectorSpace3D(RealSpace(Real)))
structure FlatCamera3D = FlatCamera(Geometry3D)
structure SimpleScene3D = SimpleScene(Geometry3D)
structure SimpleRaytracer = Raytracer(
  structure G = Geometry3D
  structure S = SimpleScene3D
  structure C = FlatCamera3D)

type render_worker = FlatCamera3D.coords -> SimpleRaytracer.pixel

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
    val subpixels = map renderWorker (map (shift (x, y)) ds);
  in
    foldl (fn ({z}, {z=acc}) => {z=acc+z*0.25}) {z=0.0} subpixels
  end

fun renderPGM 
  (renderWorker: render_worker)
  filename 
  (w, h) =
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

  val pic = List.tabulate (
    w*h, 
    (fn n =>
      let 
        val (x, y) = (n mod w, n div w)
      in
        #z (renderWorker (Real.fromInt x, Real.fromInt y))
      end)
    );

  val maxDepth = foldl maxNormal 0.0 pic;
  val text = String.implode 
    (map (chr o (grayscale maxDepth)) pic);

  val file = TextIO.openOut filename
in
  TextIO.output (file, String.concat [
    "P5\n",
    foldr 
      (fn (l, r) => l ^ " " ^ r) 
      "\n" 
      (map 
        Int.toString 
        [w, h, 200]),
    text
    ]) 
    before 
    TextIO.closeOut file
end

local open Geometry3D; open SimpleScene3D; open FlatCamera3D in
  val scene_data = Group (
    List.tabulate (
      10, 
      (fn n => 
        let
          val z = 3.0 + 0.2 * real(n);
          val phi = 2.0 * Math.pi * real(n)/10.0;
        in
          Sphere {
            center = (z, Math.cos phi, Math.sin phi),
            radius = 0.5
            }
        end)
      )
    )
  val cam = Camera (
    { 
      origin = (0.0, 0.0, 0.0), 
      target = (1.0, 0.0, 0.0)
    },
    Screen (
      (512.0, 512.0), 
      Math.pi/6.0)
    )
end;

(* renderPGM (antialias (renderPixel scene_data cam)) "tracer.pgm" (512, 512) *)
renderPGM (antialias (renderPixel scene_data cam)) "tracer.pgm" (512, 512) 

