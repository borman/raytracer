structure Image =
struct
  open Array2

  type 'a image = 'a array

  fun render renderer (w, h): 'a image = 
  let
    val (rw, rh) = (real w, real h)
  in
    tabulate RowMajor (w, h, 
      fn (y, x) => renderer (real x / rw, real y / rh))
  end

  fun convert tabulator f image = 
  let
    val (h, w) = dimensions image
  in
    tabulator
     (w*h,
      fn n => f (sub (image, n div w, n mod w)))
  end
        
  fun toGray f image = convert CharVector.tabulate f image
  fun toColor f image = convert Vector.tabulate f image

  fun fold f base img = Array2.fold RowMajor f base img

  fun size img = 
  let
    val (h, w) = dimensions img
  in
    (w, h)
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
