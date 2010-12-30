structure PNMWriter:> PNM_WRITER = 
struct
  datatype contents = 
      Grayscale of string
    | Color of string vector

  fun save (filename, (width, height), contents) =
  let
    val file = TextIO.openOut filename
    fun write x = TextIO.output (file, x)
    fun write_header magic = 
      List.app write
       [magic,
        Int.toString(width),
        " ",
        Int.toString(height),
        " ",
        Int.toString(255),
        "\n"]
  in
   (case contents of
         Grayscale pixels =>
          (write_header "P3"
           ; write pixels)
       | Color pixels =>
          (write_header "P6"
           ; Vector.app write pixels))

    ; TextIO.closeOut file
  end
end
