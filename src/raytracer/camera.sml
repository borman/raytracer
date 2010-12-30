structure Camera: CAMERA =
struct
  exception Direction

  local 
    open Geometry 
    open Real 
  in
    type coords = real * real
    datatype projection = Rectilinear

    type camera = 
     {angle: real,
      aspect: real,
      location: vector,
      look_at: vector,
      projection: projection}

    fun projector cam =
    let
      val {angle,
           aspect,
           location,
           look_at,
           projection} = cam
      
      fun rectilinear () = 
      let
        val camv = norm (location --> look_at) (* Camera view direction *)
        val right = 
          if camv || Vector3D.z (* FIXME: camv || z is bad *)
            then raise Direction 
          else norm (camv cross Vector3D.z) 
        val up = right cross camv

        val screenX = (Math.tan angle) *-> right
        val screenY = (Math.tan angle / aspect) *-> (neg up)

        val screenOrigin = location +-> camv 
          -- (inv two *-> screenX) -- (inv two *-> screenY)
      in
        fn (x, y) => ray 
         (location,
          screenOrigin +-> (x *-> screenX) +-> (y *-> screenY))
      end
    in
      case projection of
           Rectilinear => rectilinear ()
    end
  end 
end

(* vim: set ft=sml tw=76 nowrap et: *)
