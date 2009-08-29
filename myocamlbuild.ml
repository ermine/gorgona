open Ocamlbuild_plugin
open Command

let _ = dispatch begin function
  | After_rules ->
      ocaml_lib ~extern:true ~dir:"+lablgtk2" "lablgtk"
  | _ ->
      ()
end
