signature VECTOR3D = 
sig
  type vector

  (* Constructors *)
  val vector: real * real * real -> vector
  val unpack: vector -> real * real * real

  (* Axes *)
  val null: vector
  val x: vector
  val y: vector
  val z: vector

  val isNull: vector -> bool

  (* Basic vector ops *)
  val add: vector * vector -> vector
  val sub: vector * vector -> vector
  val neg: vector -> vector
  val mul: real * vector -> vector
  val dot: vector * vector -> real
  val cross: vector * vector -> vector
end

(* vim: set ft=sml tw=76 nowrap et: *)
