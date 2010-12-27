signature EXTREAL =
sig
  include REAL

  val zero: real
  val one: real
  val two: real
  val epsilon: real
  
  val isZero: real -> bool
  val inv: real -> real
end
(* vim: set ft=sml tw=76 nowrap et: *)
