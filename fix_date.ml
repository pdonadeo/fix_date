open Core
open Base.Poly

let dry_run = ref false

let finally handler f x =
  let r =
    try f x
    with e ->
      handler ();
      raise e
  in
  handler ();
  r

let with_dispose ~dispose f x = finally (fun () -> dispose x) f x

let rec fold_on_dir_tree f acc start_dir =
  let rec loop_the_dir handle acc =
    let entry = Core_unix.readdir_opt handle in

    match entry with
    | None -> acc (* No more entry *)
    | Some entry -> begin
      (* another entry to process *)
      if entry <> "." && entry <> ".." (* skip . and .. *)
      then begin
        let file_name = Filename.concat start_dir entry in
        let kind = try Some (Core_unix.stat file_name).Core_unix.st_kind with Core_unix.Unix_error _ -> None in

        match kind with
        | Some kind -> begin
          let new_acc = f kind acc file_name in
          match kind with
          | Core_unix.S_DIR -> loop_the_dir handle (fold_on_dir_tree f new_acc file_name)
          | _ -> loop_the_dir handle new_acc
        end
        | None -> loop_the_dir handle acc (* cannot stat, continue *)
      end
      else loop_the_dir handle acc (* continue looping *)
    end
  in

  (* loop_the_dir *)
  with_dispose ~dispose:Core_unix.closedir (fun h -> loop_the_dir h acc) (Core_unix.opendir start_dir)

type exif_data_type =
  | Ascii
  | Byte
  | SignedByte
  | Long
  | Rational
  | Short
  | SLong
  | SRational
  | SShort
  | XmpText
  | Undefined

let string_of_exif_data_type e =
  match e with
  | Ascii -> "Ascii"
  | Byte -> "Byte"
  | SignedByte -> "SByte"
  | Long -> "Long"
  | Rational -> "Rational"
  | Short -> "Short"
  | SLong -> "SLong"
  | SRational -> "SRational"
  | SShort -> "SShort"
  | XmpText -> "XmpText"
  | Undefined -> "Undefined"

let decode_type type_ =
  match type_ with
  | "0x0000" -> Undefined
  | "0x0012" -> Undefined
  | "0x004e" -> Undefined
  | "0x0053" -> Undefined
  | "0x00ff" -> Undefined
  | "0x0100" -> Undefined
  | "0x0211" -> Undefined
  | "0x0600" -> Undefined
  | "0x0c00" -> Undefined
  | "Ascii" -> Ascii
  | "Byte" -> Byte
  | "SByte" -> SignedByte
  | "Long" -> Long
  | "Rational" -> Rational
  | "Short" -> Short
  | "SLong" -> SLong
  | "SRational" -> SRational
  | "SShort" -> SShort
  | "XmpText" -> XmpText
  | "Undefined" -> Undefined
  | _ -> failwith ("decode_type: unknown type " ^ type_)

type prop = {
  name : string;
  type_ : exif_data_type;
  length : int;
  value : string;
}

let make_exif_prop name type_ length value =
  let name = String.slice name 5 (String.length name) in
  let type_ = decode_type type_ in
  let length = int_of_string length in
  { name; type_; length; value }

let prop_name_is_interesting name =
  if String.is_prefix name ~prefix:"Canon"
     || String.is_prefix name ~prefix:"Fujifilm"
     || String.is_prefix name ~prefix:"Image2"
     || String.is_prefix name ~prefix:"Image3"
     || String.is_prefix name ~prefix:"Iop"
     || String.is_prefix name ~prefix:"MakerNote"
     || String.is_prefix name ~prefix:"Minolta"
     || String.is_prefix name ~prefix:"Nikon"
     || String.is_prefix name ~prefix:"Olympus"
     || String.is_prefix name ~prefix:"Samsung2"
     || String.is_prefix name ~prefix:"Sony1"
  then false
  else true

let is prefix prop = if String.prefix prop.name (String.length prefix) = prefix then true else false

let time_of_string s =
  let open String.Search_pattern in
  if String.length s = 19
  then begin
    let pattern = create ":" in
    let s = replace_first pattern ~in_:s ~with_:"-" in
    let s = replace_first pattern ~in_:s ~with_:"-" in
    let s = if s.[5] = ' ' then replace_first (create " ") ~in_:s ~with_:"0" else s in
    try Some (Time_float_unix.of_string s) with _ -> None
  end
  else None

module Image = struct
  type t = { date_time : Time_float_unix.t option }

  let empty = { date_time = None }
  let prefix = "Image."
  let is = is prefix

  let update image p =
    if is p
    then begin
      match (image, p.name) with
      | Some _, "Image.DateTime" | None, "Image.DateTime" -> Some { date_time = time_of_string p.value }
      | _, _ -> image
    end
    else image
end

module Photo = struct
  type t = {
    date_time_digitized : Time_float_unix.t option;
    date_time_original : Time_float_unix.t option;
  }

  let empty = { date_time_digitized = None; date_time_original = None }
  let prefix = "Photo."
  let is = is prefix

  let update photo p =
    if is p
    then begin
      match (photo, p.name) with
      | Some i, "Photo.DateTimeDigitized" -> Some { i with date_time_digitized = time_of_string p.value }
      | Some i, "Photo.DateTimeOriginal" -> Some { i with date_time_original = time_of_string p.value }
      | None, "Photo.DateTimeDigitized" -> Some { empty with date_time_digitized = time_of_string p.value }
      | None, "Photo.DateTimeOriginal" -> Some { empty with date_time_original = time_of_string p.value }
      | _, _ -> photo
    end
    else photo
end

module Exif = struct
  type t = {
    fname : string;
    image : Image.t option;
    photo : Photo.t option;
  }

  let empty = { fname = ""; image = None; photo = None }
end

let exif_of_prop_list fname p_list =
  let open Exif in
  List.fold p_list ~init:{ Exif.empty with fname } ~f:(fun exif p ->
      { exif with image = Image.update exif.image p; photo = Photo.update exif.photo p })

let extract_exiv2 fname =
  try
    let output_lines = Shell.run_lines "exiv2" ["-pt"; fname] in
    let prop_list =
      List.fold ~init:[] output_lines ~f:(fun acc line ->
          if String.prefix line 5 = "Exif."
          then begin
            let tokens = String.split line ~on:' ' |> List.filter ~f:(( <> ) "") in
            let prop_raw = List.hd_exn tokens in
            if prop_name_is_interesting prop_raw
            then begin
              let type_raw = List.nth_exn tokens 1 in
              let length_raw = List.nth_exn tokens 2 in
              let value_raw = List.slice tokens 3 (List.length tokens) |> String.concat ~sep:" " in
              make_exif_prop prop_raw type_raw length_raw value_raw :: acc
            end
            else acc
          end
          else acc)
    in
    Some (exif_of_prop_list fname prop_list)
  with Shell.Process.Failed _ -> None

let regexp1 =
  Str.regexp "^.*/\\([1-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\\)\\.\\(jpg\\|png\\)$"

let regexp2 =
  Str.regexp
    "^.*/\\(20[0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\) \
     \\([0-9][0-9]\\)\\.\\([0-9][0-9]\\)\\.\\([0-9][0-9]\\)[-_0-9a-zA-Z]*\\.\\(jpg\\|png\\|mp4\\|jpeg\\|3gp\\|mov\\)$"

let regexp3 =
  Str.regexp
    "^.*/video-\\(20[0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\.mp4$"

let regexp4 =
  Str.regexp
    "^.*/\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)_\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[-_0-9a-zA-Z]*\\.\\(jpg\\|png\\|mp4\\|JPG\\)$"

let regexp5 =
  Str.regexp
    "^.*/IMG_\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)_\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\).\\(jpg\\|png\\|mp4\\)"

let regexp6 =
  Str.regexp
    "^.*/VID_\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)_\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\).\\(jpg\\|png\\|mp4\\|3gp\\)"

let regexp7 =
  Str.regexp
    "^.*/CameraZOOM-\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[0-9][0-9][0-9]\\.\\(jpg\\|png\\|mp4\\)$"

let regexp8 =
  Str.regexp
    "^.*/Screenshot_\\(20[0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\.\\(png\\|jpg\\)$"

let regexp9 = Str.regexp "^.*/IMG-\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)-W\\(.+\\).jpg$"
let regexp10 = Str.regexp "^.*/VID-\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)-W\\(.+\\).mp4$"

let regexp11 =
  Str.regexp
    "^.*/\\(20[0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\.webm$"

let regexp12 =
  Str.regexp
    "^.*/Screenshot from \\(20[0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\) \
     \\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\.\\(png\\|jpg\\)$"

let regexp13 =
  Str.regexp
    "^.*/Screenshot from \\(20[0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\) \
     \\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\.\\(png\\|jpg\\)$"

let regexp14 =
  Str.regexp
    "^.*/Screenshot_\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\.\\(jpg\\|png\\)$"

let regexp15 =
  Str.regexp
    "^.*/profile_\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)_\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\(_[0-9]\\)?\\.png$"

let regexp16 =
  Str.regexp
    "^.*/photo_\\(20[0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)_\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\.jpg$"

let regexp17 =
  Str.regexp
    "^.*/IMG_\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)_\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)_[0-9]+.\\(jpg\\|png\\|mp4\\)"

let regexp18 =
  Str.regexp
    "^.*/VID\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\).\\(jpg\\|png\\|mp4\\|3gp\\)"

let cruft1 = Str.regexp "^.*\\.dropbox$"
let cruft2 = Str.regexp "^.*\\.DS_Store$"

let extract_group_or_def ?(def = 0) group_num str =
  try Str.matched_group group_num str |> int_of_string with
  | Invalid_argument _ -> def
  | Failure _ -> def

let ts_of_string s =
  let year = Str.matched_group 1 s |> int_of_string in
  let month = Str.matched_group 2 s |> int_of_string in
  let day = Str.matched_group 3 s |> int_of_string in
  let hour = extract_group_or_def 4 s in
  let min = extract_group_or_def 5 s in
  let sec = extract_group_or_def 6 s in
  let ts_string = Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" year month day hour min sec in
  Time_float_unix.of_string ts_string

let time_of_float s = Time_float_unix.(s |> Span.of_sec |> of_span_since_epoch)
let time_to_float t = Time_float_unix.(t |> to_span_since_epoch |> Span.to_sec)

let extract_date_from_name fname =
  if Str.string_match regexp1 fname 0
  then begin
    let parsed_float = Str.matched_group 1 fname |> int_of_string |> Float.of_int in
    Some (time_of_float (parsed_float /. 1000.))
  end
  else if Str.string_match regexp2 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp3 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp4 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp5 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp6 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp7 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp8 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp9 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp10 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp11 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp12 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp13 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp14 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp15 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp16 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp17 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match regexp18 fname 0
  then Some (ts_of_string fname)
  else if Str.string_match cruft1 fname 0
  then None
  else if Str.string_match cruft2 fname 0
  then None
  else None

let datetime_of_exif exif =
  let open Exif in
  let decide_dtd_or_dto dtd dto =
    let diff = Time_float_unix.diff dtd dto |> Time_float_unix.Span.to_sec in
    if Float.abs diff < 5.0 then Some (Time_float_unix.min dtd dto) else Some dto
  in

  let decide_3 dt dtd dto =
    let min_t = Time_float_unix.min dt dtd |> Time_float_unix.min dto in
    let max_t = Time_float_unix.max dt dtd |> Time_float_unix.max dto in
    let diff = Time_float_unix.diff min_t max_t |> Time_float_unix.Span.to_sec in
    if Float.abs diff < 5.0
    then Some min_t
    else begin
      let mid_t =
        let open Time_float_unix in
        if min_t <= dt && dt < max_t
        then dt
        else if min_t <= dtd && dtd < max_t
        then dtd
        else if min_t <= dto && dto < max_t
        then dto
        else failwith "Questo non dovrebbe mai accadere!"
      in
      assert (Time_float_unix.(min_t <= mid_t && mid_t < max_t));
      if Float.abs (Time_float_unix.diff min_t mid_t |> Time_float_unix.Span.to_sec) < 1.0
      then Some min_t
      else if Float.abs (Time_float_unix.diff max_t mid_t |> Time_float_unix.Span.to_sec) < 1.0
      then Some mid_t
      else failwith "TODO: PROMPT THE USER!"
    end
  in

  match exif with
  | None -> None
  | Some exif -> begin
    match (exif.image, exif.photo) with
    | Some image, Some photo -> begin
      let open Image in
      let open Photo in
      match (image.date_time, photo.date_time_digitized, photo.date_time_original) with
      | Some dt, Some dtd, Some dto -> decide_3 dt dtd dto
      | Some _dt, Some _dtd, None ->
        failwith "Questa situazione non si era mai verificata su oltre 11.000 file analizzati"
      | Some dt, None, Some dto -> decide_dtd_or_dto dt dto
      | Some dt, None, None -> Some dt
      | None, Some dtd, Some dto -> decide_dtd_or_dto dtd dto
      | None, Some dtd, None -> Some dtd
      | None, None, Some dto -> Some dto
      | None, None, None -> None
    end
    | None, Some photo -> begin
      let open Photo in
      match (photo.date_time_digitized, photo.date_time_original) with
      | Some dtd, Some dto -> decide_dtd_or_dto dtd dto
      | None, Some dto -> Some dto
      | Some dtd, None -> Some dtd
      | None, None -> None
    end
    | Some image, None -> image.Image.date_time
    | None, None -> None
  end

let fix_mtime fname mtime =
  let open Core_unix in
  if !dry_run
  then Printf.printf "FILE MTIME CHANGED TO %s: \"%s\"\n%!" (mtime |> time_of_float |> Time_float_unix.to_string) fname
  else utimes fname ~access:(stat fname).st_atime ~modif:mtime

let rec user_decide fname exif_dt fn_dt =
  let file_stat = Core_unix.stat fname in
  let mtime = file_stat.Core_unix.st_mtime in
  let file_dt = time_of_float mtime in

  let rec ask_and_read_reply () =
    Printf.printf "\nFILE: \"%s\"\n%!" fname;
    Printf.printf "  1) Unix mtime    = %s\n%!" (Time_float_unix.to_string file_dt);
    Printf.printf "  2) EXIF time     = %s\n%!" (Time_float_unix.to_string exif_dt);
    Printf.printf "  3) filename time = %s\n%!" (Time_float_unix.to_string fn_dt);
    Printf.printf "  4) DO NOTHING\n%!";
    try In_channel.(input_line_exn stdin) |> String.strip |> int_of_string with Failure _ -> ask_and_read_reply ()
  in
  match ask_and_read_reply () with
  | 1 -> Some file_dt
  | 2 -> Some exif_dt
  | 3 -> Some fn_dt
  | 4 -> None
  | _ -> user_decide fname exif_dt fn_dt

let set_exif fname tstamp =
  let open Time_float_unix in
  let ts_str = format tstamp ~zone:(Lazy.force Zone.local) "%Y:%m:%d %H:%M:%S" in
  let tags_to_set =
    [
      "Exif.Image.DateTime";
      "Exif.Image.PreviewDateTime";
      "Exif.Photo.DateTimeDigitized";
      "Exif.Photo.DateTimeOriginal";
      "Xmp.exif.DateTimeOriginal";
      "Xmp.photoshop.DateCreated";
      "Xmp.tiff.DateTime";
      "Xmp.xmp.CreateDate";
      "Xmp.xmp.MetadataDate";
      "Xmp.xmp.ModifyDate";
    ]
  in
  List.iter tags_to_set ~f:(fun tag ->
      if !dry_run
      then Printf.printf "SETTING TAG %s FOR FILE %s TO \"%s\"\n%!" tag fname ts_str
      else begin
        try Shell.run "exiv2" ["-M"; "set " ^ tag ^ " \"" ^ ts_str ^ "\""; fname]
        with _ -> Printf.eprintf "Eccezione mentre imposto tag %s per il file \"%s\"\n%!" tag fname
      end)

let rename_file fname dt =
  let dir, file = Filename.split fname in
  let _file, ext = Filename.split_extension file in
  match ext with
  | Some ext -> begin
    let file' = Time_float_unix.format dt ~zone:(Lazy.force Time_float_unix.Zone.local) "%Y-%m-%d %H.%M.%S" in
    let fname' = dir ^ "/" ^ file' ^ "." ^ ext in
    if !dry_run then Printf.printf "RENAME FILE \"%s\" -> \"%s\"" fname fname' else Sys_unix.rename fname fname';
    Some fname'
  end
  | None -> None

let action_on_file kind () fname =
  match kind with
  | Core_unix.S_REG -> begin
    (if !dry_run then Printf.printf "Considering file \"%s\"\n%!" fname;
     let exif_datetime = extract_exiv2 fname |> datetime_of_exif in
     let filename_datetime = extract_date_from_name fname in
     match (exif_datetime, filename_datetime) with
     | Some exif_dt, Some fn_dt -> begin
       if Float.abs (Time_float_unix.diff exif_dt fn_dt |> Time_float_unix.Span.to_sec) < 15.0
       then begin
         let file_stat = Core_unix.stat fname in
         let exif_dt_float = time_to_float exif_dt in
         if Float.abs (exif_dt_float -. file_stat.Core_unix.st_mtime) > 1.0
         then begin
           fix_mtime fname exif_dt_float
         end
       end
       else begin
         let user_decision = user_decide fname exif_dt fn_dt in
         match user_decision with
         | Some dt -> begin
           let fname' = rename_file fname dt in
           let fname =
             match fname' with
             | None -> fname
             | Some f -> f
           in
           set_exif fname dt;
           fix_mtime fname (time_to_float dt)
         end
         | None -> ()
       end
     end
     | Some ts, None -> begin
       let ts_float = time_to_float ts in
       let file_stat = Core_unix.stat fname in
       if Float.abs (ts_float -. file_stat.Core_unix.st_mtime) > 1.0
       then begin
         fix_mtime fname ts_float
       end
       (* TODO TODO TODO CHIEDERE ALL'UTENTE SE RINOMINARE IL FILE? *)
     end
     | None, Some ts -> begin
       let ts_float = time_to_float ts in
       let file_stat = Core_unix.stat fname in
       if Float.abs (ts_float -. file_stat.Core_unix.st_mtime) > 1.0
       then begin
         set_exif fname ts;
         fix_mtime fname ts_float
       end
     end
     | None, None -> begin Printf.eprintf "NESSUNA INFO CRONOLOGICA: \"%s\"\n%!" fname end);
    if !dry_run then Printf.printf "\n%!"
  end
  | _ -> ()

let main d_run start_dir =
  if d_run then dry_run := true;
  fold_on_dir_tree action_on_file () start_dir

let command =
  Command.basic
    ~summary:"Fix EXIV2 informations and/or filenames of pictures in a directory, recursively"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map dry_run = flag "-n" ~aliases:["--dry-run"] no_arg ~doc:" actually do nothing"
     and start_dir = anon ("start_dir" %: string) in
     fun () -> main dry_run start_dir)

let version = "1.0"

let build_info =
  (Shell.run_first_line_exn "which" ["fix_date"] |> Core_unix.stat).Core_unix.st_mtime
  |> time_of_float
  |> Time_float_unix.to_sec_string ~zone:(Lazy.force Time_float_unix.Zone.local)

let () = Command_unix.run ~version ~build_info command
