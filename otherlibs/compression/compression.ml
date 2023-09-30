external supported_ : unit -> bool = "caml_compression_supported"

let supported = supported_ ()


module Marshal = struct

external to_channel_ : out_channel -> 'a -> Marshal.extern_flags list -> unit
       = "caml_compressed_marshal_to_channel"

let to_channel =
  if supported then to_channel_ else Marshal.to_channel

external from_channel_ : in_channel -> 'a
       = "caml_compressed_marshal_from_channel"

let from_channel =
  if supported then from_channel_ else Marshal.from_channel

end
