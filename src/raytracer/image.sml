functor Image (R: RGB) =
struct
  structure Rgb = R

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
      Int.toString(#width img),
      " ",
      Int.toString(#height img),
      " ",
      Int.toString(255),
      "\n",
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
        Int.toString(#width img),
        " ",
        Int.toString(#height img),
        " ",
        Int.toString(255),
        "\n"
      ] @ 
      Vector.foldr 
        (fn (col, acc) => (rgb_to_str col)::acc)
        []
        (#pixels img)
      ));  
    TextIO.closeOut file
  end
end

(* vim: set ft=sml tw=76 nowrap et: *)
