(* Copyright (c) 1998-2007 INRIA *)

let eprintf = Printf.eprintf

let stop_server = ref "STOP_SERVER"
let cgi = ref false

let wserver_sock = ref Unix.stdout
let wsocket () = !wserver_sock

let wserver_oc = ref stdout
let woc () = !wserver_oc

let printnl () =
  if not !cgi then output_byte !wserver_oc 13;
  output_byte !wserver_oc 10

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

  let http status =
    if !printing_state <> Nothing then failwith "HTTP Status already sent";
    (* ignore translation \n to \r\n under Windows *)
    set_binary_mode_out !wserver_oc true; 
    printing_state := Status;
    (* printing header cgi mode : see https://www.ietf.org/rfc/rfc3875.txt (CGI/1.1) :
                                 "Status:" status-code SP reason-phrase NL
       otherwise http mode      : see https://tools.ietf.org/html/rfc7231 (HTTP/1.1 )
                                 "HTTP/1.1" SP status-code SP reason-phrase NL
    *)
    if !cgi then Printf.fprintf !wserver_oc "Status:%s" (status_string status)
    else Printf.fprintf !wserver_oc "HTTP/1.1 %s" (status_string status);
    printnl ()

let header s =
  if !printing_state <> Status then
    if !printing_state = Nothing then http Def.OK
    else failwith "Cannot write HTTP headers: page contents already started";
    if !cgi then
      (* In CGI mode, it MUST NOT return any header fields that relate to client-side 
      communication issues and could affect the server's ability to send the response to the client.*)
      let f, v = try let i = String.index s ':' in
        (String.sub s 0 i),
        (String.sub s (i + 2) (String.length s - i - 2))
      with Not_found -> (s, "")
      in
      match String.lowercase_ascii f with
      | "connection"
      | "date"
      | "server" ->  
        () (* ignore HTTP field, not need in CGI mode*)
      | _ ->
        output_string !wserver_oc (f ^ ":" ^ v);
        printnl ()
    else
      (output_string !wserver_oc s; printnl ())

let printf fmt =
  if !printing_state <> Contents then
    begin
      if !printing_state = Nothing then http Def.OK;
      printnl ();
      printing_state := Contents
    end;
  Printf.fprintf !wserver_oc fmt

let print_string s =
  if !printing_state <> Contents then
    begin
      if !printing_state = Nothing then http Def.OK;
      printnl ();
      printing_state := Contents
    end ;
  output_string !wserver_oc s

let http_redirect_temporarily url =
  http Def.Found;
  header ("Location: " ^ url);
  printf ""

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
        printf "<head><title>Time out</title></head>\n";
        printf "<body><h1>Time out</h1>\n";
        printf "Computation time > %d second(s)\n" tmout;
        printf "</body>\n";
        flush !wserver_oc;
        exit 0
      end
    else exit 0;
  let _ = Unix.waitpid [] pid in (); exit 2

let get_request_and_content strm =
  let meth, path_and_query, _, request = get_request strm in
  let content =
    match Mutil.extract_param "content-length: " ' ' request with
    | "" -> ""
    | x -> String.init (int_of_string x) (fun _ -> Stream.next strm)
  in
  meth, path_and_query, request, content

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
      (try Printf.sprintf "   %s=%s\n" var (Sys.getenv var) with Not_found -> "")
      ^ (env_vars lvar)
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
              (env_vars [ "LANG"; "REMOTE_HOST"; "REMOTE_ADDR"; "SCRIPT_NAME"; "QUERY_STRING"
                        ; "SERVER_NAME"; "SERVER_PORT"; "SERVER_PROTOCOL"; "SERVER_SOFTWARE"
                        ; "AUTH_TYPE"; "REQUEST_METHOD"; "CONTENT_TYPE"; "CONTENT_LENGTH"
                        ; "HTTP_ACCEPT_LANGUAGE"; "HTTP_REFERER"; "HTTP_USER_AGENT";"GATEWAY_INTERFACE" ]
              )
              (match !printing_state with 
              | Nothing -> "Nothing sent"
              | Status -> "HTTP status sent without content"
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
  if !printing_state = Nothing then 
    http (if !cgi then Def.OK else Def.Internal_Server_Error);
  if !printing_state = Status then begin
      header "Content-type: text/html; charset=UTF-8";
      header "Connection: close"
    end;
  printf "<!DOCTYPE html>\n<html><head><title>Geneweb server error</title></head>\n<body>\n\
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
            | Status -> "HTTP status sent without content"
            | Contents -> "Some HTTP data sent" 
          )
          (match e with 
          | Sys_error msg -> "Sys_error - " ^ msg
          | _ -> Printexc.to_string e
          )
          backtrace
          fname

let treat_connection tmout callback addr fd =
  let (meth, path_and_query, request, contents) =
    let strm = Stream.of_channel (Unix.in_channel_of_descr fd) in
    get_request_and_content strm
  in 
  let path, query = try
    let i = String.index path_and_query '?' in
    String.sub path_and_query 0 i,
    String.sub path_and_query (i + 1) (String.length path_and_query - i - 1)
  with Not_found -> path_and_query, ""
  in
  let query = 
    if meth = Http_post then
      if query = "" then contents else (query ^ "&" ^ contents) 
    else query
  in
  match meth with
  | No_method ->  (* unlikely but possible if no data sent ; do nothing/ignore *)
      ()
  | Http_post     (* application/x-www-form-urlencoded *)
  | Http_get ->
      begin 
        try 
          callback (addr, request) path query
        with e -> 
          let msg = match Unix.getsockopt_error fd with
          | Some m -> Unix.error_message m
          | None -> "None"
          in
          print_internal_error e ((string_of_sockaddr addr) ^ " ---" ^ msg) path query
      end
  | _ -> 
      http Def.Method_Not_Allowed;
      header "Content-type: text/html; charset=UTF-8";
      header "Connection: close";
      printf "<html>\n\
              <<head>title>Not allowed HTTP method</title></head>\n\
              <body>Not allowed HTTP method</body>\n\
              </html>\n";
  ;
  if !printing_state = Status then 
    failwith ("Unexcepted HTTP printing state, connection from " ^ (string_of_sockaddr addr) ^ " without content sent :" );
  printing_state := Nothing

(* elementary HTTP server, unix mode with fork   *)
#ifdef UNIX

let connection_closed = ref false

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

let skip_possible_remaining_chars fd =
  if not !connection_closed then
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
    with Unix.Unix_error (Unix.ECONNRESET, _, _) -> ()
  
 let close_connection () =
  if not !connection_closed then begin
    flush !wserver_oc;
    (try Unix.shutdown !wserver_sock Unix.SHUTDOWN_SEND with _ ->());
    skip_possible_remaining_chars !wserver_sock ;
    close_out !wserver_oc ;
    connection_closed := true
  end

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
        wserver_oc := Unix.out_channel_of_descr t;
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
        treat_connection tmout callback addr t ;
        close_connection () ;
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
type conn_kind = Server | Connected_client  | Closed_client
type conn_info = {
    addr : Unix.sockaddr;
    fd : Unix.file_descr;
    oc : out_channel;
    start_time : float;
    mutable kind : conn_kind }

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
  let flush_nosyserr conn = 
    try flush conn.oc with 
    | Sys_error msg -> incr err_nb; Printf.eprintf "Flush error %s : %s\n%!" (string_of_sockaddr conn.addr) msg
    | e -> raise e 
  in
  let mem_limit = ref (used_mem ()) in 
  Printf.eprintf "Starting with timeout=%.0f sec., max connection=%d, %d ko of memory used\n%!" conn_tmout max_conn !mem_limit;
  syslog `LOG_DEBUG (Printf.sprintf "Starting with timeout=%.0f sec., %d ko of memory used" conn_tmout !mem_limit);
  while true do
    Unix.sleepf 0.01;
    check_stopping ();
    (* DO NOT reduce the maximal delay too low; 
       the operating system might consider too many system calls as an attack and break connections *)
    match Unix.select !fdl [] [] 15.0 with 
    | ([], _, _) ->
      let n = List.length !fdl in
      if n > 0 then begin 
          List.iter (fun conn -> 
            let ttl =  Unix.time() -. conn.start_time in
            if (ttl >= conn_tmout) && (conn.kind = Connected_client) then begin
                shutdown_noerr conn;
                close_out_noerr conn.oc;
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
            let (client_fd, client_addr) = Unix.accept fd in 
            Unix.setsockopt client_fd Unix.SO_KEEPALIVE true;
            cl :=  { 
              addr = client_addr;
              fd = client_fd;
              oc = Unix.out_channel_of_descr client_fd;
              start_time = Unix.time ();
              kind = Connected_client } :: !cl;
              fdl := client_fd :: !fdl
          end else begin (* treat incoming connection *)
            let conn = List.find ( fun t -> t.fd = fd ) !cl in
              wserver_sock := conn.fd;
              wserver_oc := conn.oc;
              treat_connection tmout g conn.addr fd;
              flush_nosyserr conn;
              shutdown_noerr conn;
              close_out_noerr conn.oc;
              Unix.close conn.fd;
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
