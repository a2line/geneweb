(* Copyright (c) 1998-2007 INRIA *)

let eprintf = Printf.eprintf

let stop_server = ref "STOP_SERVER"
let out_fname = "~" ^ (Filename.remove_extension (Filename.basename Sys.executable_name)) ^ "_response.txt"
let cgi = ref false
let proc = ref 0

let wserver_sock = ref Unix.stdout
let wserver_addr = ref ""
let wsocket () = !wserver_sock

let skip_possible_remaining_chars fd =
  let b = Bytes.create 3 in
  try
    let rec loop () =
      match Unix.select [fd] [] [] 5.0 with
      | [_], [], [] ->
        let len = Unix.read fd b 0 (Bytes.length b) in
        if len = Bytes.length b then loop ()
      | _ -> ()
    in
    loop ()
  with 
  | Unix.Unix_error (Unix.ECONNABORTED, _, _) -> ()
  | Unix.Unix_error (Unix.ECONNRESET, _, _) -> ()

type printing_status = Nothing | Status | Contents

let printing_state = ref Nothing

let status_string status = 
  match status with
  | Def.OK -> "200 OK"
  | Def.Moved_Permanently -> "301 Moved Permanently"
  | Def.Found -> "302 Found"
  | Def.Bad_Request -> "400 Bad Request"
  | Def.Unauthorized -> "401 Unauthorized"
  | Def.Forbidden -> "403 Forbidden"
  | Def.Not_Found -> "404 Not Found"
  | Def.Method_Not_Allowed -> "405 Method Not Allowed"
  | Internal_Server_Error -> "500 Internal Server Error"
  | Service_Unavailable -> "503 Service Unavailable"
  | HTTP_Version_Not_Supported -> "505 HTTP Version Not Supported"

(* max_http like TCP PDU size : typical max MTU IP (1400) tu MAX TCP PDU (64k) *)
let max_http = ref 1400
let http_buff = Buffer.create !max_http
let last_status = ref ""
let buffer_add_nl s =
  Buffer.add_string http_buff s;
  if not !cgi then Buffer.add_int8 http_buff 13;
  Buffer.add_int8 http_buff 10

(* printing header cgi mode : see https://www.ietf.org/rfc/rfc3875.txt (CGI/1.1) :
                              "Status:" status-code SP reason-phrase NL
    otherwise http mode      : see https://tools.ietf.org/html/rfc7231 (HTTP/1.1 )
                              "HTTP/1.1" SP status-code SP reason-phrase NL *)
let http status =
  if !printing_state <> Nothing then failwith "HTTP Status already sent";
  printing_state := Status;
  last_status := status_string status;
  let response_status = 
    if !cgi then Printf.sprintf  "Status:%s" !last_status
    else Printf.sprintf "HTTP/1.1 %s" !last_status;
  in
  Buffer.clear http_buff;
  buffer_add_nl response_status

let header s =
  if !printing_state <> Status then
    if !printing_state = Nothing then http Def.OK
    else failwith "Cannot write HTTP headers: page contents already started";
    (* In CGI mode, it MUST NOT return any header fields that relate to client-side 
    communication issues and could affect the server's ability to send the response to the client.*)
    let f, v = try let i = String.index s ':' in
      (String.sub s 0 i),
      (String.sub s (i + 2) (String.length s - i - 2))
    with Not_found -> (s, "")
    in
    match String.lowercase_ascii f with
    | "connection" -> ()
    | "date"
    | "server" ->  (* ignore HTTP field, not need in CGI mode*)
      if not !cgi then buffer_add_nl s
    | _ ->
      buffer_add_nl (if !cgi then f ^ ":" ^ v else s)

let print_buffer s =
  let len = String.length s in
  let olen = Buffer.length http_buff in
  let n = !max_http - olen in
  if n < len then begin
    if n > 0 then Buffer.add_string http_buff (String.sub s 0 n);
    let writed = 
      try 
        Unix.write_substring !wserver_sock (Buffer.contents http_buff) 0 (olen+n)
      with e -> 
        Printf.eprintf "%s Writing error %d-%d : %s\n%!" !wserver_addr olen n (Printexc.to_string e);
        raise e
    in
    if writed <> (olen+n) then (* data lost *)
      Printf.eprintf "%s : %d/%d bytes writed, lost data\n%!" !wserver_addr writed (olen+n);
    Buffer.clear http_buff;
    if (len - n) < !max_http then begin
      if len > n then Buffer.add_string http_buff (String.sub s n (len-n))
    end else begin
      let writed = 
        try Unix.write_substring !wserver_sock s n (len-n)
        with e -> 
          Printf.eprintf "%s Writing error %d-%d : %s\n%!" !wserver_addr len n (Printexc.to_string e);
          raise e
      in
      if writed <> (len-n) then (* data lost *)
        Printf.eprintf "%s : %d-%d bytes writed, lost data\n%!" !wserver_addr writed (len-n)
    end
  end
  else
    Buffer.add_string http_buff s

let print_contents s =
  if !printing_state <> Contents then begin
    if !printing_state = Nothing then http Def.OK;
    (*  see RFC 7320, §6.1 : https://tools.ietf.org/html/rfc7230#page-50
        A server that does not support persistent connections MUST send the
        "close" connection option in every response message that does not
        have a 1xx (Informational) status code.*)
    if not !cgi then buffer_add_nl "Connection: close";
    buffer_add_nl "";
    if !cgi then begin
      (* ignore translation \n to \r\n under Windows *)
      set_binary_mode_out stdout true;
      print_string (Buffer.contents http_buff)
    end;
    printing_state := Contents
  end;
  if !cgi then print_string s
  else if s <> "" then print_buffer s

let flush_contents () =
  if !cgi then flush stdout 
  else
  let olen = Buffer.length http_buff in
  if olen > 0 then
    let writed = 
      try Unix.write_substring !wserver_sock (Buffer.contents http_buff) 0 olen
      with e -> 
        Printf.eprintf "%s Writing/flush error %d : %s\n%!" !wserver_addr olen (Printexc.to_string e); 0
    in 
    if writed <> olen then (* data lost *)
      Printf.eprintf "%s : %d/%d bytes writed, lost data (end)\n%!" !wserver_addr writed olen;
    Buffer.reset http_buff

let print_filename fname ctype priv = 
  let res = if priv then "private" else "public" in
  match try Some (open_in_bin fname) with _ -> None with
    Some ic ->
      let buf = Bytes.create !max_http in
      let flen = in_channel_length ic in
      http Def.OK;
      header (Printf.sprintf "Content-Type: %s" ctype);
      header (Printf.sprintf "Content-Length: %d" flen);
      header (Printf.sprintf "Content-Disposition: inline; filename=%s" (Filename.basename fname));
      header (Printf.sprintf "Cache-Control: %s, max-age=%d" res (60 * 60 * 24 * 365));
      print_contents "";
      flush_contents ();
      let rec loop len =
        if len = 0 then ()
        else
          let olen = min (Bytes.length buf) len in
          really_input ic buf 0 olen;
          let writed = 
            try Unix.write !wserver_sock buf 0 olen
            with e -> 
              Printf.eprintf "%s Writing error %d/%d : %s\n%!" !wserver_addr olen flen (Printexc.to_string e);
              raise e
          in
          if writed <> olen then (* data lost *)
            Printf.eprintf "%s : %d-%d bytes writed, lost data\n%!" !wserver_addr writed olen;
          loop (len - olen)
      in
      loop flen;
      close_in ic;
      printing_state := Nothing;
      true
  | None ->
      false
      
let print_http_filename fname = 
  match try Some (open_in_bin fname) with _ -> None with
    Some ic ->
      let buf = Bytes.create !max_http in
      let flen = in_channel_length ic in
      let rec loop len =
        if len = 0 then ()
        else
          let olen = min (Bytes.length buf) len in
          really_input ic buf 0 olen;
          let writed = 
            try Unix.write !wserver_sock buf 0 olen
            with e -> 
              Printf.eprintf "%s Writing error %d/%d : %s\n%!" !wserver_addr olen flen (Printexc.to_string e);
              raise e
          in
          if writed <> olen then (* data lost *)
            Printf.eprintf "%s : %d-%d bytes writed, lost data\n%!" !wserver_addr writed olen;
          loop (len - olen)
      in
      loop flen;
      close_in ic;
      printing_state:= Nothing;
      true
  | None ->
      false

let http_redirect_temporarily url =
  http Def.Found;
  header ("Location: " ^ url);
  print_contents "";
  flush_contents ()

let buff = ref (Bytes.create 80)
let store len x =
  if len >= Bytes.length !buff then
    buff := Bytes.extend !buff 0 (Bytes.length !buff);
  Bytes.set !buff len x;
  succ len
let get_buff len = Bytes.sub_string !buff 0 len

(* HTTP/1.1 method see  https://tools.ietf.org/html/rfc7231 , §4.3 *)
type http_method = Http_get | Http_head | Http_post | Http_put | Http_delete 
                 | Http_connect | Http_options | Http_trace | Unknown_method | No_method

let get_request strm =
  let rec loop len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '\010' ->
        Stream.junk strm__;
        let s = strm__ in
        if len = 0 then [] else let str = get_buff len in str :: loop 0 s
    | Some '\013' -> Stream.junk strm__; loop len strm__
    | Some c -> Stream.junk strm__; loop (store len c) strm__
    | _ -> if len = 0 then [] else [get_buff len]
  in
  let request = loop 0 strm in
  let http_request = try List.hd request with _ -> "" in
  let meth, s, m = 
    try let i = String.index http_request ' ' in
      (match (String.uppercase_ascii @@ String.sub http_request 0 i) with
        | "GET" -> Http_get
        | "HEAD" -> Http_head
        | "POST" -> Http_post
        | "PUT" -> Http_put
        | "DELETE" -> Http_delete
        | "CONNECT" -> Http_connect
        | "OPTIONS" -> Http_options
        | "TRACE" -> Http_trace
        | _ -> Unknown_method)
        (* note : in HTTP, the first / is mandatory for the path. 
        It is intentionally omitted in path_and_query for further processing  *)
      , (String.sub http_request (i + 2) (String.length http_request - i - 2) )
      , (String.uppercase_ascii @@ String.sub http_request 0 i)
    with Not_found -> No_method, "",""
  in
  let (path_and_query, http_ver) = try let i = String.rindex s ' ' in
    String.sub s 0 i,
    String.sub s (i + 1) (String.length s - i - 1)
  with Not_found -> (s, "")
  in 
  meth, path_and_query, http_ver, request

let timeout tmout spid _ =
  Unix.kill spid Sys.sigkill;
  Unix.kill spid Sys.sigterm;
  let pid = Unix.fork () in
  if pid = 0 then
    if Unix.fork () = 0 then
      begin
        http Def.OK;
        header "Content-type: text/html; charset=UTF-8";
        print_contents "<head><title>Time out</title></head>\n";
        print_contents "<body><h1>Time out</h1>\n";
        print_contents ("Computation time > " ^ string_of_int tmout ^ "second(s)\n");
        print_contents "</body>\n";
        (* flush !wserver_oc; *)
        flush_contents ();
        exit 0
      end
    else exit 0;
  let _ = Unix.waitpid [] pid in (); exit 2

let get_request_and_content strm =
  let meth, path_and_query, _, request = get_request strm in
  let contents =
    match Mutil.extract_param "content-length: " ' ' request with
    | "" -> ""
    | x -> String.init (int_of_string x) (fun _ -> Stream.next strm)
  in
  let path, query = try
    let i = String.index path_and_query '?' in
    String.sub path_and_query 0 i,
    String.sub path_and_query (i + 1) (String.length path_and_query - i - 1)
  with Not_found -> path_and_query, ""
  in
  meth, path, query, request, contents

let string_of_sockaddr =
  function
    Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (a, p) -> (Unix.string_of_inet_addr a) ^ ":" ^ (string_of_int p)

let sockaddr_of_string str =
  try
    let i = String.index str ':' in
    Unix.ADDR_INET (
      Unix.inet_addr_of_string (String.sub str 0 i), 
      int_of_string (String.sub str (i + 1) (String.length str - i - 1) )
    )
  with _ ->  Unix.ADDR_UNIX str

let check_stopping () =
  if Sys.file_exists !stop_server then
    begin
      flush stdout;
      eprintf "\nServer stopped by presence of file %s.\n" !stop_server;
      eprintf "Remove that file to allow servers to run again.\n";
      flush stderr;
      exit 0
    end

let log_exn e info addr path query = 
  let systime () =
    let now = Unix.gettimeofday () in
    let tm = Unix.localtime (now) in
    let sd = Float.to_int @@ 1000000.0 *. (mod_float now 1.0)  in 
    Printf.sprintf "%02d:%02d:%02d.%06d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec sd
  in
  let rec env_vars lvar =
    match lvar with
    | var::lvar -> 
        (Printf.sprintf 
            "   %s=%s\n" 
            var (try Sys.getenv var with Not_found -> "")
        ) ^ (env_vars lvar)
    | _ -> ""
  in
  let fname =  Sys.executable_name ^ "-error.txt " in
  let msg = Printf.sprintf 
              "---- %s - Unexcepted exception : %s\n\
              - Raised with request %s?%s%s\n\
              - Mode : %s, process id = %d\n\
              - Environnement :\n%s\
              - HTTP state (printing_state) : %s before exception\n\
              - Info :\n%s\n"
              (systime()) 
              (match e with
              | Sys_error msg -> "Sys_error - " ^ msg
              | e -> Printexc.to_string e) 
              path query 
              ((if addr <> "" then " from " else "") ^ addr)
              (if !cgi then "CGI script" else "HTTP server") (Unix.getpid ())
              (env_vars [ "LANG"; "REMOTE_HOST"; "REMOTE_ADDR"; "SCRIPT_NAME"; "PATH_INFO"; "QUERY_STRING"
                        ; "SERVER_NAME"; "SERVER_PORT"; "SERVER_PROTOCOL"; "SERVER_SOFTWARE"
                        ; "AUTH_TYPE"; "REQUEST_METHOD"; "CONTENT_TYPE"; "CONTENT_LENGTH"
                        ; "HTTP_ACCEPT_LANGUAGE"; "HTTP_REFERER"; "HTTP_USER_AGENT";"GATEWAY_INTERFACE" ]
              )
              (match !printing_state with 
              | Nothing -> "Nothing sent"
              | Status -> "HTTP status set without content"
              | Contents -> "Some HTTP data sent" 
              )
              info 
  in
  try
    let oc = open_out_gen ([Open_wronly; Open_append; Open_creat]) 0o777 fname in
    let logsize = out_channel_length oc in 
    let oc = if logsize < 16000 then oc 
    else (close_out oc; open_out_gen ([Open_wronly; Open_trunc; Open_creat]) 0o777 fname)
    in 
    output_string oc msg;
    close_out_noerr oc;
    fname
  with _ -> 
    output_string stderr msg;
    flush stderr;
    ""

let print_internal_error e addr path query =
  let backtrace = Printexc.get_backtrace () in
  let fname = log_exn e backtrace addr path query in 
  let state = !printing_state in
  if !printing_state <> Contents then begin
    http (if !cgi then Def.OK else Def.Internal_Server_Error);
    header "Content-type: text/html; charset=UTF-8";
  end;
  print_contents ( Printf.sprintf 
                "<!DOCTYPE html>\n<html><head><title>Geneweb server error</title></head>\n<body>\n\
                  <h1>Unexpected Geneweb error, request not complete :</h1>\n\
                  <pre>- Raised with request %s?%s%s\n\
                  - Mode : %s, process id = %d\n\
                  - HTTP state (printing_state) : %s before exception\n\
                  - Exception : %s\n\
                  - Backtrace :\n<hr>\n%s\n</pre><hr>\n\
                  Saved in %s<hr>\n
                  <a href=\\>[Home page]</a>  \
                  <a href=\"javascript:window.history.go(-1)\">[Previous page]</a>\n\
                  </body>\n</html>\n"
                  path query 
                  ((if addr <> "" then " from " else "") ^ addr)
                  (if !cgi then "CGI script" else "HTTP server") (Unix.getpid ())
                  (match state with 
                    | Nothing -> "Nothing sent"
                    | Status -> "HTTP status set without content"
                    | Contents -> "Some HTTP data sent" 
                  )
                  (match e with 
                  | Sys_error msg -> "Sys_error - " ^ msg
                  | _ -> Printexc.to_string e
                  )
                  backtrace
                  fname
                ) 

let treat_connection syslog tmout callback addr fd =
  let (meth, path, query, request, contents) =
    let strm = Stream.of_channel (Unix.in_channel_of_descr fd) in
    get_request_and_content strm
  in 
  let query = 
    if meth = Http_post then
      if query = "" then contents else (query ^ "&" ^ contents) 
    else query
  in
  match meth with
  | No_method ->  (* unlikely but possible if no data sent ; do nothing/ignore *)
        syslog `LOG_DEBUG ( Printf.sprintf
                              "%s Ignore empty connection (no data read)" (string_of_sockaddr addr)
                          );
      ()
  | Http_post     (* application/x-www-form-urlencoded *)
  | Http_get ->
      begin 
        syslog `LOG_DEBUG ( Printf.sprintf
                              "%s Request : GET %s?%s" 
                              (string_of_sockaddr addr) path query
                          );
        try 
          callback (addr, request) path query
        with e -> 
          let msg = match Unix.getsockopt_error fd with
          | Some m -> Unix.error_message m
          | None -> "No socket error"
          in
          print_internal_error e ((string_of_sockaddr addr) ^ " ---" ^ msg) path query
      end
  | _ -> 
      http Def.Method_Not_Allowed;
      header "Content-type: text/html; charset=UTF-8";
      header "Connection: close";
      print_contents "<html>\n\
                    <<head>title>Not allowed HTTP method</title></head>\n\
                    <body>Not allowed HTTP method</body>\n\
                    </html>\n";
  ;
  if !printing_state = Status then 
    failwith ("Unexcepted HTTP printing state, connection from " ^ (string_of_sockaddr addr) ^ " without content sent :" );
  printing_state := Nothing
  
(* elementary HTTP server, unix mode with fork   *)
#ifdef UNIX

let rec list_remove x =
  function
    [] -> failwith "list_remove"
  | y :: l -> if x = y then l else y :: list_remove x l

let pids = ref []

let cleanup_sons () =
  List.iter begin fun p ->
    match fst (Unix.waitpid [ Unix.WNOHANG ] p) with
    | 0 -> ()
    | exception Unix.Unix_error (Unix.ECHILD, "waitpid", _)
    (* should not be needed anymore since [Unix.getpid () <> ppid] loop security *)
    | _ -> pids := list_remove p !pids
  end !pids

let wait_available max_clients s =
  match max_clients with
    Some m ->
      if List.length !pids >= m then
        (let (pid, _) = Unix.wait () in pids := list_remove pid !pids);
      if !pids <> [] then cleanup_sons ();
      let stop_verbose = ref false in
      while !pids <> [] && Unix.select [s] [] [] 15.0 = ([], [], []) do
        cleanup_sons ();
        if !pids <> [] && not !stop_verbose then
          begin
            stop_verbose := true;
            let tm = Unix.localtime (Unix.time ()) in
            eprintf
              "*** %02d/%02d/%4d %02d:%02d:%02d %d process(es) remaining after cleanup (%d)\n"
              tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year)
              tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
              (List.length !pids) (List.hd !pids);
            flush stderr;
            ()
          end
      done
  | None -> ()

let accept_connection tmout max_clients callback s =
  wait_available max_clients s;
  let (t, addr) = Unix.accept s in
  Unix.setsockopt t Unix.SO_KEEPALIVE true;
  match try Some (Unix.fork ()) with _ -> None with
  | Some 0 ->
    begin
      try
        if max_clients = None && Unix.fork () <> 0 then exit 0;
        Unix.close s;
        wserver_sock := t;
        flush_contents ();
        if tmout > 0 then begin 
          let spid = Unix.fork () in
            if spid > 0 then begin
              ignore @@ Sys.signal Sys.sigalrm (Sys.Signal_handle (timeout tmout spid)) ;
              ignore @@ Unix.alarm tmout ;
              ignore @@ Unix.waitpid [] spid ;
              ignore @@ Sys.signal Sys.sigalrm Sys.Signal_default ;
              exit 0
            end
        end;
        treat_connection tmout callback addr t;
        flush_contents ();
        (try Unix.shutdown !wserver_sock Unix.SHUTDOWN_SEND with _ ->());
        skip_possible_remaining_chars !wserver_sock;
        exit 0
      with
      | Unix.Unix_error (Unix.ECONNRESET, "read", _) -> exit 0
      | e -> raise e
    end
  | Some id ->
      Unix.close t;
      if max_clients = None then let _ = Unix.waitpid [] id in ()
      else pids := id :: !pids
  | None -> 
      Unix.close t; eprintf "Fork failed\n"; flush stderr
#endif

(* elementary HTTP server, basic mode for windows *)
#ifdef WINDOWS
type conn_kind = Server | Connected_client | Closed_client
type conn_info = {
    addr : Unix.sockaddr;
    fd : Unix.file_descr;
    oc : out_channel;
    start_time : float;
    mutable kind : conn_kind }

let proceed_connection syslog tmout callback addr fd_conn =
  let addr_string = match addr with
      Unix.ADDR_UNIX a -> a
    | Unix.ADDR_INET (a, _) -> Unix.string_of_inet_addr a
  in  
  let (meth, path, query, request, contents) =
  let strm = Stream.of_channel (Unix.in_channel_of_descr fd_conn) in
    get_request_and_content strm
  in
  let query = if meth = Http_post then contents else query
  in
  if meth <> No_method then begin
    if meth = Http_post || (Filename.extension path) <> "" || (String.index_opt path '/') <> None then begin
      callback (addr, request) path query
    end else begin
      let env = Array.append (Unix.environment ())
                 [| "GATEWAY_INTERFACE=RELAY/HTTP" 
                  ; "REMOTE_HOST="
                  ; "REMOTE_ADDR=" ^ addr_string
                  ; "REQUEST_METHOD=GET"
                  ; "SCRIPT_NAME=" ^ Sys.argv.(0)
                  ; "PATH_INFO=" ^ path
                  ; "QUERY_STRING=" ^ query
                  ; "HTTP_COOKIE=" ^ (Mutil.extract_param "content-length: " '\n' request)
                  ; "CONTENT_TYPE=" ^ (Mutil.extract_param "content-type: " '\n' request)
                  ; "HTTP_ACCEPT_LANGUAGE=" ^ (Mutil.extract_param "accept_language: " '\n' request)
                  ; "HTTP_REFERER=" ^ (Mutil.extract_param "referer: " '\n' request)
                  ; "HTTP_USER_AGENT=" ^ (Mutil.extract_param "user-agent: " '\n' request)
                  ; "HTTP_AUTHORIZATION=" ^ (Mutil.extract_param "authorization: " '\n' request)
                  |]
      in
      let fd_out = Unix.openfile out_fname [Unix.O_WRONLY; O_CREAT; O_TRUNC] 0o640  in
      let pid = Unix.create_process_env Sys.argv.(0) Sys.argv env  Unix.stdin fd_out Unix.stderr in
      syslog `LOG_DEBUG (Printf.sprintf "%s Proceed request with pid %d '%s?%s'" (string_of_sockaddr addr) pid path query );
      Unix.close fd_out;
      ignore @@ Unix.waitpid [] pid;
      if not (print_http_filename out_fname) then
         failwith ("Gwd error sending response file : "  ^ out_fname)
      else
        Sys.remove out_fname
    end
  end

let wserver_basic syslog tmout max_clients g s addr_server =
  let server = {
    addr = addr_server;
    start_time = Unix.time ();
    fd = s;
    oc = stdout;
    kind = Server } 
  in
  let cl = ref [server] in
  let fdl = ref [s] in
  let remove_from_poll fd =
    fdl:=List.filter (fun t -> t <> fd ) !fdl;
    cl:=List.filter (fun t -> t.fd <> fd ) !cl
  in 
  let max_conn = 
    match max_clients with
    | Some max -> if max = 0 then 10 else max * 5
    | None -> 10
  in 
  let conn_tmout = Float.of_int (if tmout < 30 then 30 else tmout) in
  let used_mem () = 
    let st = Gc.stat () in 
    (st.live_blocks * Sys.word_size) / 8 / 1024
  in 
  let shutdown_noerr conn = try Unix.shutdown conn.fd Unix.SHUTDOWN_ALL with
  | Unix.Unix_error(Unix.ECONNRESET, "shutdown", "")
  | Unix.Unix_error(Unix.ECONNABORTED, "shutdown", "") -> ()
  | exc -> 
    Printf.eprintf "Shutdown connection from %s, unknown exception : %s\n%!" 
      (string_of_sockaddr conn.addr)
      (Printexc.to_string exc)
  in
  let err_nb = ref 0 in

  let mem_limit = ref (used_mem ()) in 
  syslog `LOG_DEBUG (Printf.sprintf "Starting with timeout=%.0f sec., %d ko of memory used" conn_tmout !mem_limit);
  while true do
    Unix.sleepf 0.01;
    check_stopping ();
    match Unix.select !fdl [] [] 15.0 with 
    | ([], _, _) ->
      let n = List.length !fdl in
      if n > 0 then begin 
          List.iter (fun conn -> 
            let ttl =  Unix.time() -. conn.start_time in
            if (ttl >= conn_tmout) && (conn.kind = Connected_client) then begin
                shutdown_noerr conn;
                Unix.close conn.fd;
                Printf.eprintf "%s connection closed (timeout)\n%!" (string_of_sockaddr conn.addr);
                syslog `LOG_DEBUG (Printf.sprintf "%s connection closed (timeout)" (string_of_sockaddr conn.addr) );
                fdl:=List.filter (fun t -> t <> conn.fd ) !fdl;
                conn.kind <- Closed_client
              end else 
            if conn.kind = Connected_client then begin
                try let _ = Unix.getpeername conn.fd in ()
                with e -> 
                  Printf.eprintf "Connection %s, closed by peer : %s \n%!" (string_of_sockaddr conn.addr)  (Printexc.to_string e);
                  fdl:=List.filter (fun t -> t <> conn.fd ) !fdl;
                  conn.kind <- Closed_client
                end else
            let mem = ref (used_mem ()) in 
              if !mem > !mem_limit then begin
                  syslog `LOG_INFO (Printf.sprintf "%d ko of memory used, invoke heap compaction" !mem);
                  Gc.compact (); 
                  mem := used_mem ();
                  mem_limit := !mem * 2;
              end
#ifdef DEBUG
              ;Printf.eprintf "Cnt[%d], Err[%d] - %d..%d ko used   \r%!" (List.length !fdl) !err_nb !mem !mem_limit
#endif
          ) !cl;
          cl:=List.filter (fun conn -> conn.kind <> Closed_client ) !cl;
          let n = List.length !cl in
          if n > max_conn then failwith ("Too many connection remaining : " ^ (string_of_int n))
        end
    | ( l, _, _) -> 
      let rec loop l i = 
        match l with
        | [] ->
          ()
        | fd :: lfd -> 
          if fd=s then begin (* accept new incoming connection *)
            let (client_fd, client_addr) = Unix.accept ~cloexec:true fd in 
            Unix.setsockopt client_fd Unix.SO_KEEPALIVE true;
            cl :=  { 
              addr = client_addr;
              fd = client_fd;
              oc = Unix.out_channel_of_descr client_fd;
              start_time = Unix.gettimeofday ();
              kind = Connected_client } :: !cl;
              fdl := client_fd :: !fdl
          end else begin (* treat incoming connection *)
            let conn = List.find ( fun t -> t.fd = fd ) !cl in
            let t0 = Unix.gettimeofday () in 
              wserver_sock := conn.fd;
              wserver_addr := string_of_sockaddr conn.addr;
              syslog `LOG_DEBUG (
                      Printf.sprintf "%s treat connection starting %.6f sec." 
                                      (string_of_sockaddr conn.addr)
                                      ( (Unix.gettimeofday  ()) -. conn.start_time)
                      );
              begin try 
                if !proc > 0 then 
                  proceed_connection syslog tmout g conn.addr conn.fd
                else
                  treat_connection syslog tmout g conn.addr fd;
                flush_contents ();
              with 
              | Unix.Unix_error (Unix.ECONNRESET, "write", _) ->
                syslog `LOG_DEBUG (
                  Printf.sprintf "%s reseted after %.6f sec." 
                                  (string_of_sockaddr conn.addr)
                                  ( (Unix.gettimeofday ()) -. conn.start_time)
                  );
              | Unix.Unix_error (Unix.ECONNABORTED, "write", _) ->
                syslog `LOG_DEBUG (
                  Printf.sprintf "%s closed by peer after %.6f sec." 
                                  (string_of_sockaddr conn.addr)
                                  ( (Unix.gettimeofday ()) -. conn.start_time)
                  );
              | e -> raise e
              end;
              shutdown_noerr conn;
              Unix.close conn.fd;
              let elapsed = (Unix.gettimeofday()) -. t0 in
              syslog `LOG_DEBUG (
                      Printf.sprintf "%s connection closed after %.6f sec. with status %s in %.6f secs." 
                                      (string_of_sockaddr conn.addr)
                                      ( (Unix.gettimeofday()) -. conn.start_time)
                                      !last_status
                                      elapsed
                      );
              remove_from_poll conn.fd
            end;
          loop lfd (i+1)
      in
      loop l 0
  done
#endif
  
let f syslog addr_opt port tmout max_clients g =
  let addr =
    match addr_opt with
      Some addr ->
        begin try Unix.inet_addr_of_string addr with
          Failure _ -> (Unix.gethostbyname addr).Unix.h_addr_list.(0)
        end
    | None -> Unix.inet_addr_any
  in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let a =  (Unix.ADDR_INET (addr, port)) in 
  Unix.bind s a;
  Unix.listen s 10;
  Unix.setsockopt s Unix.SO_KEEPALIVE true;
  let tm = Unix.localtime (Unix.time ()) in
  eprintf "Ready %4d-%02d-%02d %02d:%02d port %d...\n%!"
    (1900 + tm.Unix.tm_year)
    (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    port ;
#ifdef WINDOWS
  wserver_basic syslog tmout max_clients g s a
#else
  let _ = Unix.nice 1 in
  while true do
    check_stopping ();
    try accept_connection tmout max_clients g s with
    | Unix.Unix_error (Unix.ECONNRESET, "accept", _) as e ->
      syslog `LOG_INFO (Printexc.to_string e)
    | Sys_error msg as e when msg = "Broken pipe" ->
      syslog `LOG_INFO (Printexc.to_string e)
    | e -> raise e
  done
#endif
