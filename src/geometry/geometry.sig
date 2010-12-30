signature GEOMETRY =
sig
  (* Types *)
  type vector = Vector3D.vector
  type ray = 
   {origin: vector, 
    direction: vector}

  val ray: vector * vector -> ray
  
  (* Basic vector ops *)
  val --> : vector * vector -> vector
  val -- : vector * vector -> vector
  val +-> : vector * vector -> vector
  val *-> : real * vector -> vector

  val neg: vector -> vector
  val dot: vector * vector -> real
  val cross: vector * vector -> vector

  (* Compound vector ops *)
  val sqlength: vector -> real
  val length: vector -> real
  val norm: vector -> vector
  val nearer: vector * vector * vector -> bool
  val reflect: vector * vector -> vector

  (* Parallellity/perpendicularity tests *)
  val || : vector * vector -> bool
  val -| : vector * vector -> bool
end

(* vim: set ft=sml tw=76 nowrap et: *)
