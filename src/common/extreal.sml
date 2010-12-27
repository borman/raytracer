structure ExtReal : EXTREAL =
struct
  open Real

  val zero = 0.0
  val one = 1.0
  val epsilon = 1E~12

  fun isZero a = abs(a) < epsilon
  fun inv a = one / a
end

(* vim: set ft=sml tw=76 nowrap et: *)
