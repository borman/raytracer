signature PNM_WRITER =
sig
  datatype contents = 
      Grayscale of string
    | Color of string vector

  val save: string * (int * int) * contents -> unit
end

(* vim: set ft=sml tw=76 nowrap et: *)
