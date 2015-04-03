open Ocamlbuild_plugin
open Ocamlbuild_pack

let lib_dir pkg =
  let ic = Unix.open_process_in ("ocamlfind query " ^ pkg) in
  let line = input_line ic in
  close_in ic;
  line

let target_with_extension ext =
  List.exists (fun s -> Pathname.get_extension s = ext) !Options.targets

let copy from_chan to_chan =
  try
    while true do
      output_string to_chan (input_line from_chan);
      output_char to_chan '\n';
    done
  with _ -> ()

let append source dest =
  let source_chan = open_in source in
  let dest_chan   = open_out_gen [Open_append] 0 dest in
  copy source_chan dest_chan;
  close_out_noerr dest_chan;
  close_in_noerr source_chan
  
let rec copy_mlt_files path =
  Pathname.readdir path
  |> Array.iter
    (fun p ->
      if Pathname.is_directory (path / p) then
        copy_mlt_files (path / p)
      else if Pathname.check_extension p "mlt" then
        begin
          let source_file = Pathname.update_extension "ml" p in
          let append_too  = !Options.build_dir / path / source_file in
          if Sys.file_exists append_too then
            begin
              Log.dprintf 0 "Appending %S to %S\n" p append_too;
              append p append_too
            end
          else
            Log.dprintf 0 "Test file %S but no source %S\n" p append_too
        end
(* let src = path / p in
        let dst = !Options.build_dir / path / p in
        Shell.mkdir_p (!Options.build_dir / path);
        Pathname.copy src dst
        end *)
      else
        ())

let integrate_coverage = true

let () =
  let additional_rules =
    function
      | Before_hygiene  -> ()
      | After_hygiene   -> ()
      | Before_options  -> ()
      | After_options   -> ()
      | Before_rules    -> ()
      | After_rules     ->
          begin
            if target_with_extension "test" then copy_mlt_files "src";
            rule "Create a test target."
              ~prod:"%.test"
              ~dep:"%.native"
              begin fun env _build ->
                let test = env "%.test" and native = env "%.native" in
                Seq [ mv native test
                    ; Cmd (S [ A"ln"
                              ; A"-sf"
                              ; P (!Options.build_dir/test)
                              ; A Pathname.parent_dir_name])
                ]
              end;
            if target_with_extension "test" then
              begin
                if integrate_coverage then
                  begin
                    let bsdir = lib_dir "bisect" in
                    flag ["pp"]
                      (*(S [P (!Options.build_dir / "tools/joiner.native") *)
                      (S [ A "camlp4o"
                         ; A "str.cma"
                         ; A (bsdir / "bisect_pp.cmo")]);
                    flag ["compile"]
                      (S [A"-I"; A bsdir]);
                    flag ["link"; "byte"; "program"]
                      (S [A"-I"; A bsdir; A"bisect.cmo"]);
                    flag ["link"; "native"; "program"]
                      (S [A"-I"; A bsdir; A"bisect.cmx"])
                  end
                else
                  flag ["pp"]
                    (S [ A(lib_dir "kaputt" / "kaputt_pp.byte")
                       ; A"on"; A "camlp4o"]);
              end
          end
  in
  dispatch additional_rules
