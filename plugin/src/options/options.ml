(* --- Options for fix-to-elim --- *)

(*
 * By default, treat dependencies as opaque rather than transparent
 * (disabled by default)
 *)
let opt_default_opaque = ref (false)
let _ = Goptions.declare_bool_option {
  Goptions.optdepr = false;
  Goptions.optname = "Treat definitions as opaque by default";
  Goptions.optkey = ["Preprocess"; "default"; "opaque"];
  Goptions.optread = (fun () -> !opt_default_opaque);
  Goptions.optwrite = (fun b -> opt_default_opaque := b);
}

let is_default_opaque () = !opt_default_opaque
