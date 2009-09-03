open Ocamlbuild_plugin
open Myocamlbuild_config

let _ =
  let f = Unix.open_process_in "git describe --always" in
  let answer = input_line f in
    ignore (Unix.close_process_in f);
    let out = open_out "REVISION" in
      output_string out answer;
      close_out out

let _ = dispatch begin function
  | After_rules ->
      rule "version.ml"
        ~prod:"version.ml"
        ~deps:["version.ml.src"; "REVISION"]
        (fun _ _ ->
           let revision = with_input_file "REVISION" input_line in           
             Seq [rm_f "version.ml";
                  Cmd (S[A"sed"; A"-e";
                         A(Format.sprintf "s,VERSION,%s," revision);
                         Sh"<"; P"version.ml.src"; Sh">"; Px"version.ml"]);
                  chmod (A"-w") "version.ml"]
        );

      extern "cryptokit";
      ocaml_lib ~extern:true ~dir:"+lablgtk2" "lablgtk";
      extern "mltls";
      extern "treap";
      extern "xml";
      extern ~cma:"xMPP" ~tag_name:"use_xmpp" "xmpp";
      extern "logger";
      
  | _ ->
      ()
end
