functor FlatCamera(G: GEOMETRY): CAMERA =
struct
  structure Geometry = G

  local open G; open Real in
    type coords = scalar * scalar
    type angle = scalar
    datatype screen = Screen of coords * angle;
    datatype camera = Camera of ray * screen

    fun projector cam =
    let
      val Camera (
        {origin, direction}, 
        Screen ((width, height), angle)) = cam;
      
      val camv = direction;
      val horizontal = norm (camv cross z); (* FIXME: camv || z is bad *)
      val vertical = neg (norm (horizontal cross camv));
      
      val halfdiag = length camv * Math.tan angle;
      val phi = Math.atan2 (height, width);

      val halfwidth = halfdiag * Math.cos phi;
      val halfheight = halfdiag * Math.sin phi;

      val screenOrigin = origin +-> direction 
          +-> neg (halfwidth *-> horizontal) 
          +-> neg (halfheight *-> vertical);

      val screenX = (halfwidth * two / width) *-> horizontal;
      val screenY = (halfheight * two / height) *-> vertical 
    in
      fn (x, y) => ray (
        origin,
        screenOrigin +-> (x *-> screenX) +-> (y *-> screenY)
        )
    end
  end 
end

(* vim: set ft=sml tw=76 nowrap et: *)
