signature GEOMETRY =
sig
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
  val nearer: vector * vector * vector -> bool

  (* Parallellity/perpendicularity tests *)
  val ||  : vector * vector -> bool
  val -|  : vector * vector -> bool
end

(* vim: set ft=sml tw=76 nowrap et: *)
